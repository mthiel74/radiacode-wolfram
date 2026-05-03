(* ::Package:: *)

(* RadiaCodeTools`Device`
   Wolfram-native API for talking to a connected RadiaCode device.

   Implementation note
   -------------------
   RadiaCode is a vendor-class USB device with bulk endpoints
   (0x01 / 0x81) using a custom command set; Wolfram
   ships no libusb bindings, so a *truly* native implementation would
   require LibraryLink + hidapi (or libusb).  As a pragmatic stand-in
   we drive the device through the upstream Python tools — the same
   `radiacode` library `radiacode_poll.py` and `rcmultispg.py` use —
   and parse the standard outputs back into Wolfram structures.  The
   call signatures below are the Wolfram API surface; the bridge is an
   implementation detail, and a future C-level rewrite via LibraryLink
   would not change them.
*)

BeginPackage["RadiaCodeTools`Device`",
  {"RadiaCodeTools`Formats`",
   "RadiaCodeTools`LiveViewer`"}];

RadiaCodeDevices::usage =
  "RadiaCodeDevices[] returns a list of serial numbers of every \
RadiaCode device currently attached over USB.";

RadiaCodeAcquire::usage =
  "RadiaCodeAcquire[] reads the device's current cumulative spectrum \
and returns it as an Association compatible with ImportN42 / \
ImportRCSpectrum.  Options:\n\
  \"AccumulationTime\" -> Quantity | seconds (default 0 — instantaneous \
read)\n\
  \"AccumulationDose\" -> uSv (mutually exclusive with time)\n\
  \"Bluetooth\"        -> Mac address string | Automatic\n\
  \"BackgroundSubtract\" -> True | False\n\
  \"OutputFile\"       -> Automatic | path (default: temp; deleted on return)";

RadiaCodeStream::usage =
  "RadiaCodeStream[] starts an rcmultispg subprocess and returns a \
LiveViewer stream id which can be passed to RadiaCodeDashboard / \
RadiaCodeStreamState / CloseRadiaCodeStream.  Options:\n\
  \"Devices\"        -> list of serial numbers | Automatic (all)\n\
  \"PollingInterval\" -> seconds between device polls (default 5)\n\
  \"GpsdURL\"        -> URL string | None\n\
  \"PollInterval\"   -> Wolfram-side stream poll period (default 0.5)";

RadiaCodeReset::usage =
  "RadiaCodeReset[\"Spectrum\"] resets the device's accumulated \
spectrum.  RadiaCodeReset[\"Dose\"] resets the cumulative dose.  Both \
are destructive — confirm intent first.";

RadiaCodeAutoStream::usage =
  "RadiaCodeAutoStream[opts] picks the right live-streaming backend \
automatically: if the libusb LibraryLink shim is built and loaded, \
RadiaCodeNativeStream is used (no Python at runtime).  Otherwise \
RadiaCodeStream is used (Python bridge).  Either way returns a \
stream id suitable for RadiaCodeDashboard.  Prints which backend \
fired.";

$RadiaCodePython::usage =
  "$RadiaCodePython holds the path or name of the Python interpreter \
used by Device.wl.  Default: \"python3\".";

$RadiaCodeRepoRoot::usage =
  "$RadiaCodeRepoRoot is the directory holding the upstream repo's \
src/ subfolder (which provides the `radiacode_tools` package).  \
Auto-detected from this file's location.";

Begin["`Private`"];

(* ----- locate the repo ----- *)

resolveRepoRoot[] :=
  Module[{here, candidates, trial},
    here = DirectoryName[$InputFileName /. "" :> NotebookFileName[]];
    candidates = NestList[ParentDirectory, here, 6];
    trial = SelectFirst[candidates,
      DirectoryQ[FileNameJoin[{#, "src", "radiacode_tools"}]] &,
      None];
    If[trial === None, ParentDirectory[ParentDirectory[here]], trial]
  ];

(* The Mathematica front-end on macOS gets a stripped-down PATH that
   typically excludes Homebrew, anaconda, and `~/.local/bin`, so a bare
   "python3" RunProcess call fails with "Program python3 not found".
   Resolve to an absolute path at load time. *)
detectPython[] :=
  Module[{candidates, found, which, home},
    home = $HomeDirectory;
    candidates = {
      FileNameJoin[{home, "anaconda3", "bin", "python3"}],
      FileNameJoin[{home, "miniconda3", "bin", "python3"}],
      FileNameJoin[{home, ".local", "bin", "python3"}],
      "/opt/homebrew/bin/python3",
      "/opt/anaconda3/bin/python3",
      "/usr/local/bin/python3",
      "/usr/bin/python3"
    };
    found = SelectFirst[candidates, FileExistsQ];
    If[StringQ[found], Return[found]];
    (* Fallback: ask the login shell, which loads the user's profile *)
    which = Quiet @ RunProcess[
      {"/bin/bash", "-l", "-c", "command -v python3"}, "StandardOutput"];
    If[StringQ[which] && StringTrim[which] =!= "" &&
       FileExistsQ[StringTrim[which]],
       StringTrim[which],
       "python3"]
  ];

(* Re-evaluate on every Get so a stale value left over from an older
   version of this package doesn't pin us to a broken interpreter.
   Preserve the user's explicit override iff it points to a real file. *)
If[!StringQ[$RadiaCodeRepoRoot] || !DirectoryQ[$RadiaCodeRepoRoot],
   $RadiaCodeRepoRoot = resolveRepoRoot[]];
If[!StringQ[$RadiaCodePython] || !FileExistsQ[$RadiaCodePython],
   $RadiaCodePython = detectPython[]];

repoSrc[] := FileNameJoin[{$RadiaCodeRepoRoot, "src"}];

scriptPath[name_String] :=
  FileNameJoin[{repoSrc[], name <> ".py"}];

(* Merge PYTHONPATH into the inherited environment rather than
   replacing it; the child still needs HOME, LANG, USER, etc. *)
buildPythonEnv[] :=
  Module[{env, current},
    env = Quiet @ GetEnvironment[];
    If[!ListQ[env], env = {}];
    current = Lookup[Association @@ env, "PYTHONPATH", ""];
    Append[
      Association @@ env,
      "PYTHONPATH" -> If[current === "" || current === None,
                          repoSrc[],
                          repoSrc[] <> ":" <> current]]
  ];

runPython[args_List] :=
  RunProcess[
    Prepend[args, $RadiaCodePython],
    All,
    "",
    ProcessEnvironment -> buildPythonEnv[]
  ];

(* ----- device enumeration ----- *)

RadiaCodeDevices[] :=
  Module[{out, lines},
    out = runPython[{"-c",
      "from radiacode_tools.rc_utils import find_radiacode_devices\n" <>
      "import sys\n" <>
      "for sn in find_radiacode_devices(): print(sn)"}];
    If[!AssociationQ[out] || out["ExitCode"] =!= 0,
      Return[Failure["PythonBridge",
        <|"MessageTemplate" -> "Could not enumerate devices: `1`",
          "MessageParameters" -> {Lookup[out, "StandardError", "?"]}|>]]];
    lines = Select[StringSplit[Lookup[out, "StandardOutput", ""], "\n"],
                    StringTrim[#] =!= "" &];
    StringTrim /@ lines
  ];

(* ----- single-shot acquisition (radiacode_poll.py wrapper) ----- *)

formatHMS[seconds_?NumericQ] :=
  Module[{s = Round[seconds], h, m},
    h = Quotient[s, 3600];
    m = Quotient[Mod[s, 3600], 60];
    s = Mod[s, 60];
    StringJoin[
      IntegerString[h, 10, 2], ":",
      IntegerString[m, 10, 2], ":",
      IntegerString[s, 10, 2]]
  ];

durationToSeconds[q_Quantity] := QuantityMagnitude[UnitConvert[q, "Seconds"]];
durationToSeconds[n_?NumericQ] := n;

Options[RadiaCodeAcquire] = {
  "AccumulationTime"    -> Automatic,
  "AccumulationDose"    -> Automatic,
  "Bluetooth"           -> None,
  "BackgroundSubtract"  -> False,
  "OutputFile"          -> Automatic
};

RadiaCodeAcquire[OptionsPattern[]] :=
  Module[{accT, accD, bt, bgSub, outFile, args, proc, result, cleanup},
    accT   = OptionValue["AccumulationTime"];
    accD   = OptionValue["AccumulationDose"];
    bt     = OptionValue["Bluetooth"];
    bgSub  = OptionValue["BackgroundSubtract"];
    outFile = OptionValue["OutputFile"];
    cleanup = (outFile === Automatic);
    If[cleanup,
      outFile = FileNameJoin[{$TemporaryDirectory,
        "rcacquire-" <> ToString[RandomInteger[10^9]] <> ".n42"}];
      If[FileExistsQ[outFile], DeleteFile[outFile]]];

    args = {scriptPath["radiacode_poll"]};
    If[StringQ[bt], args = Join[args, {"-b", bt}]];
    Which[
      accT =!= Automatic && accT =!= None,
        args = Join[args, {"--accumulate-time", formatHMS[durationToSeconds[accT]]}],
      accD =!= Automatic && accD =!= None,
        args = Join[args, {"--accumulate-dose", ToString[accD]}]];
    If[bgSub, AppendTo[args, "-B"]];
    args = Append[args, outFile];

    proc = runPython[args];
    If[!AssociationQ[proc] || proc["ExitCode"] =!= 0,
      If[cleanup && FileExistsQ[outFile], DeleteFile[outFile]];
      Return[Failure["AcquireFailed",
        <|"MessageTemplate" -> "radiacode_poll exited `1`: `2`",
          "MessageParameters" -> {Lookup[proc, "ExitCode", "?"],
                                   Lookup[proc, "StandardError", ""]}|>]]];
    If[!FileExistsQ[outFile],
      Return[Failure["AcquireFailed",
        <|"MessageTemplate" -> "radiacode_poll wrote no output to `1`",
          "MessageParameters" -> {outFile}|>]]];
    result = RadiaCodeTools`Formats`ImportN42[outFile];
    If[cleanup, Quiet @ DeleteFile[outFile]];
    result
  ];

(* ----- streaming (rcmultispg.py wrapper) ----- *)

Options[RadiaCodeStream] = {
  "Devices"         -> Automatic,
  "PollingInterval" -> 5.0,
  "GpsdURL"         -> None,
  "PollInterval"    -> 0.5
};

RadiaCodeStream[OptionsPattern[]] :=
  Module[{devices, interval, gpsd, command, pollI, env, envPrefix},
    devices  = OptionValue["Devices"];
    interval = OptionValue["PollingInterval"];
    gpsd     = OptionValue["GpsdURL"];
    pollI    = OptionValue["PollInterval"];

    (* The subprocess inherits PATH from `sh`, but we still need
       PYTHONPATH set so radiacode_tools is importable from src/. *)
    envPrefix = {"env", "PYTHONPATH=" <> repoSrc[]};
    command = Join[envPrefix,
      {$RadiaCodePython, scriptPath["rcmultispg"], "--stdout",
       "-i", ToString[interval]}];
    If[ListQ[devices],
      Scan[(command = Join[command, {"-d", #}]) &, devices]];
    If[StringQ[gpsd], command = Join[command, {"-g", gpsd}]];

    RadiaCodeTools`LiveViewer`OpenRadiaCodeStream[command,
      "PollInterval" -> pollI]
  ];

(* ----- reset ----- *)

RadiaCodeReset[what_String] :=
  Module[{flag, args, proc, tmp},
    flag = Switch[what,
      "Spectrum", "--reset-spectrum",
      "Dose",     "--reset-dose",
      _,          Return[Failure["BadArgument",
                    <|"MessageTemplate" ->
                       "RadiaCodeReset target must be \"Spectrum\" or \"Dose\""|>]]];
    tmp = FileNameJoin[{$TemporaryDirectory,
      "rcreset-" <> ToString[RandomInteger[10^9]] <> ".n42"}];
    If[FileExistsQ[tmp], DeleteFile[tmp]];
    args = {scriptPath["radiacode_poll"], flag, tmp};
    proc = runPython[args];
    If[FileExistsQ[tmp], Quiet @ DeleteFile[tmp]];
    If[!AssociationQ[proc] || proc["ExitCode"] =!= 0,
      Return[Failure["ResetFailed",
        <|"MessageTemplate" -> "reset exited `1`: `2`",
          "MessageParameters" -> {Lookup[proc, "ExitCode", "?"],
                                   Lookup[proc, "StandardError", ""]}|>]]];
    True
  ];

(* ---- auto-select between native libusb and Python bridge ---- *)

Options[RadiaCodeAutoStream] = {
  "PollInterval"  -> 1.0,
  "SpectrumEvery" -> 5
};

RadiaCodeAutoStream[opts:OptionsPattern[]] :=
  If[TrueQ @ Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ[],
    Print["RadiaCodeAutoStream: using libusb LibraryLink (no Python)."];
    RadiaCodeTools`DeviceNative`RadiaCodeNativeStream[
      "PollInterval"  -> OptionValue["PollInterval"],
      "SpectrumEvery" -> OptionValue["SpectrumEvery"]],
    Print["RadiaCodeAutoStream: native libusb shim not built; \
falling back to the Python bridge.  See Installation Option C in \
the post for how to build the libusb .dylib if you'd rather avoid \
Python at runtime."];
    RadiaCodeStream["PollingInterval" -> OptionValue["PollInterval"]]
  ];

End[];
EndPackage[];
