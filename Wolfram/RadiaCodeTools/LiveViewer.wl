(* ::Package:: *)

(* RadiaCodeTools`LiveViewer`
   Live consumption of an rcmultispg ndjson stream — live spectrum,
   count-rate trace, and dose-rate trace as a Dynamic dashboard.

   Architecture (see Wolfram/README.md): RadiaCode is USB-HID, not
   serial.  The Python `rcmultispg.py --stdout` tool drives the device
   and writes one JSON record per line; this package tails that stream
   from a file or via a subprocess and feeds a Dynamic[] view. *)

BeginPackage["RadiaCodeTools`LiveViewer`",
  {"RadiaCodeTools`Formats`", "RadiaCodeTools`SpectrumPlot`"}];

OpenRadiaCodeStream::usage =
  "OpenRadiaCodeStream[command] starts a subprocess (a list of \
arguments suitable for StartProcess) whose stdout is an ndjson stream \
and tails it.  OpenRadiaCodeStream[file_String] tails an existing \
file.  Both forms return a stream id (string) that can be passed to \
RadiaCodeDashboard / CloseRadiaCodeStream / RadiaCodeStreamState.  \
Options:\n\
  \"PollInterval\" -> seconds (default 0.5)";

CloseRadiaCodeStream::usage =
  "CloseRadiaCodeStream[id] stops the polling task, terminates the \
subprocess if any, and deletes the buffer file.";

RadiaCodeDashboard::usage =
  "RadiaCodeDashboard[id] returns a Dynamic[] dashboard showing the \
latest spectrum, count-rate trace, and dose-rate trace for the named \
stream.";

RadiaCodeStreamState::usage =
  "RadiaCodeStreamState[id] returns the current Association of \
accumulated stream state (spectrum, realtime history, gps history, \
record count, status).";

RadiaCodeListStreams::usage =
  "RadiaCodeListStreams[] returns the ids of all open streams.";

PlaybackNDJson::usage =
  "PlaybackNDJson[file] is a convenience wrapper that opens an ndjson \
file as a stream for offline playback / development.";

PollRadiaCodeStream::usage =
  "PollRadiaCodeStream[id] forces an immediate read of any new lines \
in the buffer.  Normally called automatically by the scheduled task, \
but useful for testing or for manually-driven dashboards.";

OpenRadiaCodeNativeStream::usage =
  "OpenRadiaCodeNativeStream[handle] starts a streaming session \
backed by the pure-Wolfram libusb LibraryLink driver \
(DeviceNative.wl) -- no Python in the runtime.  `handle` is the \
integer returned by RadiaCodeNativeOpen[].  Options:\n\
  \"PollInterval\" -> seconds (default 1.0)\n\
  \"SpectrumEvery\" -> N (refresh the spectrum every N polls; default 5)";

Begin["`Private`"];

$streams = <||>;
$nextId = 0;

newId[] := (
  $nextId++;
  "rc-stream-" <> ToString[$nextId]);

freshState[id_, source_] := <|
  "Id"             -> id,
  "Source"         -> source,
  "Process"        -> None,
  "BufferFile"     -> None,
  "Position"       -> 0,
  "Task"           -> None,
  "Status"         -> "open",
  "RecordCount"    -> 0,
  "StartTime"      -> Now,
  "LastUpdate"     -> None,
  "Spectrum"       -> <|"Counts" -> {}, "Calibration" -> {0., 1., 0.},
                         "SerialNumber" -> "", "Duration" -> Missing[]|>,
  "Realtime"       -> {},
  "GPS"            -> {},
  "MaxRealtime"    -> 600   (* keep the last 10 minutes of realtime samples *)
|>;

(* ----- record dispatch ----- *)

(* rcmultispg has shipped (at least) two ndjson layouts:

     OLD (xray.ndjson):
       {"timestamp": ..., "serial_number": ...,
        "duration": ..., "calibration": [a0,a1,a2], "counts": [...]}
       {"timestamp": ..., "count_rate": ..., "dose_rate": ...}

     NEW (live rcmultispg --stdout):
       {"dt": ..., "serial_number": ...,
        "spectrum": {"duration": ..., "a0":, "a1":, "a2":, "counts":[]},
        "_type": "SpecData"}
       {"dt": ..., "count_rate": ..., "dose_rate": ..., "_type": "RtData"}

   Recognise either. *)

extractSpectrumPayload[rec_Association] :=
  Module[{nested},
    Which[
      KeyExistsQ[rec, "counts"] && KeyExistsQ[rec, "calibration"],
        <|"Counts" -> Lookup[rec, "counts"],
          "Calibration" -> Lookup[rec, "calibration"],
          "SerialNumber" -> Lookup[rec, "serial_number", ""],
          "Duration" -> Quantity[Lookup[rec, "duration", 0], "Seconds"]|>,

      AssociationQ[Lookup[rec, "spectrum", None]],
        nested = rec["spectrum"];
        <|"Counts" -> Lookup[nested, "counts", {}],
          "Calibration" -> {Lookup[nested, "a0", 0.],
                             Lookup[nested, "a1", 1.],
                             Lookup[nested, "a2", 0.]},
          "SerialNumber" -> Lookup[rec, "serial_number", ""],
          "Duration" -> Quantity[Lookup[nested, "duration", 0], "Seconds"]|>,

      True, None
    ]
  ];

dispatchRecord[id_, rec_Association] :=
  Module[{state = $streams[id], rt, payload},
    payload = extractSpectrumPayload[rec];
    Which[
      AssociationQ[payload],
        state["Spectrum"] = payload,

      KeyExistsQ[rec, "count_rate"],
        (* JSON null comes back as Null; map it to Missing[] so
           downstream consumers can use Cases[_?NumericQ] without
           tripping on Null in plot/Quantile/Mean calls. *)
        rt = #1 /. {Null -> Missing["NotAvailable"]} & @ <|
          "Time"         -> Now,
          "SerialNumber" -> Lookup[rec, "serial_number", ""],
          "CountRate"    -> Lookup[rec, "count_rate", Missing[]],
          "DoseRate"     -> Lookup[rec, "dose_rate",  Missing[]],
          "Charge"       -> Lookup[rec, "charge_level", Missing[]],
          "Temperature"  -> Lookup[rec, "temperature", Missing[]]
        |>;
        state["Realtime"] = Take[
          Append[state["Realtime"], rt],
          -Min[Length[state["Realtime"]] + 1, state["MaxRealtime"]]],

      KeyExistsQ[rec, "lat"],
        state["GPS"] = Append[state["GPS"], <|
          "Time" -> Now,
          "Lat"  -> Lookup[rec, "lat", 0.],
          "Lon"  -> Lookup[rec, "lon", 0.],
          "Alt"  -> Lookup[rec, "alt", Missing[]],
          "Mode" -> Lookup[rec, "mode", Missing[]]|>]
    ];
    state["RecordCount"] = state["RecordCount"] + 1;
    state["LastUpdate"]  = Now;
    $streams[id] = state;
  ];

dispatchRecord[id_, _] := Null;

processNewLines[id_, lines_List] :=
  Scan[
    Function[line,
      Module[{rec},
        rec = Quiet @ ImportString[line, "RawJSON"];
        If[AssociationQ[rec], dispatchRecord[id, rec]]]],
    lines];

(* ----- buffer-file polling ----- *)

PollRadiaCodeStream[id_String] :=
  Module[{state, file, pos, totalBytes, strm, bytes, txt, lines},
    state = Lookup[$streams, id, $Failed];
    If[state === $Failed || state["Status"] === "closed", Return[]];
    file = state["BufferFile"];
    If[!StringQ[file] || !FileExistsQ[file], Return[]];
    pos = state["Position"];
    totalBytes = FileByteCount[file];
    If[totalBytes <= pos, Return[]];
    strm = OpenRead[file, BinaryFormat -> True];
    SetStreamPosition[strm, pos];
    bytes = BinaryReadList[strm, "Byte", totalBytes - pos];
    Close[strm];
    state["Position"] = totalBytes;
    $streams[id] = state;
    If[!ListQ[bytes] || Length[bytes] === 0, Return[]];
    txt = FromCharacterCode[bytes, "UTF-8"];
    lines = Select[StringSplit[txt, "\n"], StringTrim[#] =!= "" &];
    processNewLines[id, lines];
  ];

(* Detect a dead subprocess and mark the stream finished. *)
checkProcessAlive[id_] :=
  Module[{state = $streams[id], proc, status},
    If[!AssociationQ[state], Return[]];
    proc = state["Process"];
    If[proc =!= None,
      status = Quiet @ ProcessStatus[proc];
      If[StringQ[status] && status =!= "Running",
        state["Status"] = If[status === "Finished", "exited", status];
        $streams[id] = state]]
  ];

(* ----- open / close ----- *)

Options[OpenRadiaCodeStream] = {"PollInterval" -> 0.5};

OpenRadiaCodeStream[command_List, OptionsPattern[]] :=
  Module[{id, bufFile, proc, task, interval, sourceDescr},
    id = newId[];
    bufFile = FileNameJoin[{$TemporaryDirectory, id <> ".ndjson"}];
    interval = OptionValue["PollInterval"];
    sourceDescr = StringRiffle[ToString /@ command, " "];
    (* Start the subprocess BEFORE creating the buffer file, so a
       StartProcess failure leaves no orphan files on disk and no
       partially-initialised stream entry.  `exec` replaces the sh
       with the target process so the PID we hold IS the python's;
       KillProcess then actually kills the rcmultispg subprocess. *)
    proc = Quiet @ Check[
      StartProcess[
        {"sh", "-c",
          "exec " <>
          StringRiffle[
            Function[c, "'" <> StringReplace[c, "'" -> "'\\''"] <> "'"] /@ command,
            " "] <> " >> '" <> bufFile <> "'"}],
      $Failed];
    If[proc === $Failed || !MatchQ[proc, _ProcessObject],
      Return[Failure["StartProcessFailed",
        <|"MessageTemplate" -> "Could not start subprocess for `1`.",
          "MessageParameters" -> {sourceDescr}|>]]];
    Export[bufFile, "", "Text"];
    $streams[id] = <|freshState[id, sourceDescr],
      "BufferFile" -> bufFile,
      "Process"    -> proc|>;
    task = RunScheduledTask[
      (PollRadiaCodeStream[id]; checkProcessAlive[id]),
      interval];
    (* $streams[id]["Task"] = ... is a no-op (assigns to a copy);
       merge through the top-level variable so the task handle
       persists and CloseRadiaCodeStream can actually remove it. *)
    $streams[id] = <|$streams[id], "Task" -> task|>;
    id];

OpenRadiaCodeStream[file_String, OptionsPattern[]] :=
  Module[{id, interval, task},
    If[!FileExistsQ[file],
      Return[Failure["NotFound",
        <|"MessageTemplate" -> "ndjson file `1` not found.",
          "MessageParameters" -> {file}|>]]];
    id = newId[];
    interval = OptionValue["PollInterval"];
    $streams[id] = <|freshState[id, file],
      "BufferFile" -> file,
      "Process"    -> None|>;
    task = RunScheduledTask[PollRadiaCodeStream[id], interval];
    $streams[id] = <|$streams[id], "Task" -> task|>;
    PollRadiaCodeStream[id];   (* immediate first read *)
    id];

PlaybackNDJson[file_String, opts:OptionsPattern[OpenRadiaCodeStream]] :=
  OpenRadiaCodeStream[file, opts];

(* ----- pure-Wolfram native stream (libusb LibraryLink, no Python) ----- *)

Options[OpenRadiaCodeNativeStream] = {
  "PollInterval"  -> 1.0,
  "SpectrumEvery" -> 5
};

OpenRadiaCodeNativeStream[handle_Integer, OptionsPattern[]] :=
  Module[{id, interval, spectrumEvery, task, tickCounter},
    id = newId[];
    interval = OptionValue["PollInterval"];
    spectrumEvery = Max[1, OptionValue["SpectrumEvery"]];
    tickCounter = 0;
    $streams[id] = <|freshState[id, "native libusb (handle " <> ToString[handle] <> ")"],
      "NativeHandle" -> handle|>;
    task = RunScheduledTask[
      pollNativeStream[id, spectrumEvery],
      interval];
    $streams[id] = <|$streams[id], "Task" -> task|>;
    (* Fetch one spectrum + realtime sample immediately so the
       dashboard isn't blank on first render. *)
    pollNativeStream[id, 1];
    id];

(* Internal: poll the device once, dispatch the result through the
   normal dispatchRecord path so the dashboard renders identically
   regardless of whether the producer is rcmultispg or libusb. *)
pollNativeStream[id_String, spectrumEvery_Integer] :=
  Module[{state, handle, count, spec, rt},
    state = Lookup[$streams, id, $Failed];
    If[state === $Failed || state["Status"] === "closed", Return[]];
    handle = Lookup[state, "NativeHandle", None];
    If[handle === None, Return[]];
    count = state["RecordCount"];
    (* Realtime: every poll. *)
    rt = Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeReadRealtime[handle];
    If[AssociationQ[rt] && KeyExistsQ[rt, "CountRate"],
      dispatchRecord[id, <|
        "count_rate"    -> rt["CountRate"],
        "dose_rate"     -> rt["DoseRate"],
        "serial_number" -> Lookup[state["Spectrum"], "SerialNumber", ""]|>]];
    (* Spectrum: every Nth poll, since it's a heavier USB transfer. *)
    If[Mod[count, spectrumEvery] === 0,
      spec = Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeReadSpectrum[handle];
      If[AssociationQ[spec] && KeyExistsQ[spec, "Counts"],
        dispatchRecord[id, <|
          "spectrum" -> <|
            "counts"   -> spec["Counts"],
            "a0"       -> spec["Calibration"][[1]],
            "a1"       -> spec["Calibration"][[2]],
            "a2"       -> spec["Calibration"][[3]],
            "duration" -> spec["Duration"]|>,
          "serial_number" -> spec["SerialNumber"]|>]]];
  ];

CloseRadiaCodeStream[id_String] :=
  Module[{state = Lookup[$streams, id, $Failed], status, handle},
    If[state === $Failed, Return[$Failed]];
    If[state["Task"] =!= None,
      Quiet @ RemoveScheduledTask[state["Task"]]];
    If[state["Process"] =!= None,
      status = Quiet @ ProcessStatus[state["Process"]];
      If[status === "Running",
        Quiet @ KillProcess[state["Process"]]]];
    (* Native libusb handle: release it. *)
    handle = Lookup[state, "NativeHandle", None];
    If[IntegerQ[handle],
      Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeClose[handle]];
    (* Only delete the buffer file if WE created it — i.e. a subprocess
       stream — never delete a file the user passed in directly. *)
    If[state["Process"] =!= None && StringQ[state["BufferFile"]] &&
       FileExistsQ[state["BufferFile"]],
      Quiet @ DeleteFile[state["BufferFile"]]];
    state["Status"] = "closed";
    $streams[id] = state;
    id];

(* ----- queries ----- *)

RadiaCodeStreamState[id_String] :=
  Lookup[$streams, id, Failure["NotFound",
    <|"MessageTemplate" -> "Unknown stream `1`",
      "MessageParameters" -> {id}|>]];

RadiaCodeListStreams[] := Keys[$streams];

(* ----- dashboard ----- *)

$accentColor   = RGBColor[0.20, 0.55, 0.85];
$warmColor     = RGBColor[0.85, 0.32, 0.20];
$coolColor     = RGBColor[0.20, 0.45, 0.75];
$labelStyle    = Directive[FontSize -> 12, FontFamily -> "Helvetica", GrayLevel[0.35]];
$valueStyle    = Directive[FontSize -> 14, FontFamily -> "Helvetica", Bold, GrayLevel[0.10]];
$panelTitleStyle = Directive[FontSize -> 14, FontFamily -> "Helvetica",
                              Bold, $accentColor];
$panelFrame    = Directive[GrayLevel[0.85], Thickness[Tiny]];

panelFrame[content_, title_:""] :=
  Framed[
    Column[{
      If[title === "",
        Sequence @@ {},
        Style[title, $panelTitleStyle]
      ],
      content
    }, Spacings -> 0.4],
    FrameStyle  -> $panelFrame,
    Background  -> GrayLevel[0.98],
    RoundingRadius -> 6,
    FrameMargins -> 8];

statusPanel[state_Association] :=
  Module[{age, runtime, latestRate, latestDose, formatRow},
    age = If[state["LastUpdate"] === None, "\[LongDash]",
       ToString @ Round @ QuantityMagnitude[
         UnitConvert[Now - state["LastUpdate"], "Seconds"]]] <> " s ago";
    runtime = ToString @ Round @ QuantityMagnitude[
       UnitConvert[Now - state["StartTime"], "Seconds"]] <> " s";
    latestRate = If[Length[state["Realtime"]] > 0,
      Lookup[Last[state["Realtime"]], "CountRate", Missing[]],
      Missing[]];
    latestDose = If[Length[state["Realtime"]] > 0,
      Lookup[Last[state["Realtime"]], "DoseRate", Missing[]],
      Missing[]];
    formatRow[label_, value_, color_:GrayLevel[0.10]] :=
      {Style[label, $labelStyle],
       Style[value, $valueStyle, color]};
    panelFrame[
      Grid[{
        formatRow["Status",
          ToUpperCase[state["Status"]],
          If[state["Status"] === "open", $accentColor, $warmColor]],
        formatRow["Serial",
          Lookup[state["Spectrum"], "SerialNumber", "\[LongDash]"]],
        formatRow["Records", state["RecordCount"]],
        formatRow["Updated", age],
        formatRow["Uptime",  runtime],
        formatRow["Count rate",
          If[NumericQ[latestRate],
            ToString[NumberForm[latestRate, {5, 2}]] <> " cps",
            "\[LongDash]"], $coolColor],
        formatRow["Dose rate",
          If[NumericQ[latestDose],
            ToString[NumberForm[latestDose * 10.^6, {5, 2}]] <> " \[Mu]Sv/h",
            "\[LongDash]"], $warmColor]
      }, Alignment -> {{Right, Left}}, Spacings -> {1, 0.7}],
      "RadiaCode live"]
  ];

spectrumPanel[state_Association] :=
  panelFrame[
    If[Length[state["Spectrum"]["Counts"]] > 0,
      RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[state["Spectrum"],
        "Background" -> False, "PlotLabel" -> None],
      Style["awaiting first spectrum\[Ellipsis]", Italic, Gray, FontSize -> 14]
    ],
    "Spectrum (counts vs energy)"];

ratePanel[state_Association, key_String, label_String, color_] :=
  Module[{rt = state["Realtime"], pairs, doseScale},
    doseScale = If[key === "DoseRate", 10.^6, 1.];   (* convert Sv/h to uSv/h *)
    pairs = Select[
      {Lookup[#, "Time"], doseScale * Lookup[#, key]} & /@ rt,
      MatchQ[#, {_DateObject, _?NumericQ}] &];
    panelFrame[
      If[Length[pairs] === 0,
        Style["no samples yet\[Ellipsis]", Italic, Gray, FontSize -> 14],
        DateListPlot[pairs,
          Joined        -> True,
          PlotStyle     -> Directive[color, Thickness[0.005]],
          Filling       -> Bottom,
          FillingStyle  -> Directive[color, Opacity[0.18]],
          Frame         -> True,
          FrameStyle    -> $panelFrame,
          FrameLabel    -> {None, label},
          LabelStyle    -> $labelStyle,
          ImageSize     -> 480,
          GridLines     -> Automatic,
          GridLinesStyle -> Directive[GrayLevel[0.92], Thickness[Tiny]],
          AspectRatio   -> 0.4]],
      label]
  ];

buildDashboard[id_String] :=
  Module[{state = Lookup[$streams, id, None]},
    If[!AssociationQ[state],
      Return[Style["Stream " <> id <> " is closed or unknown.", Red]]];
    Column[{
      Grid[{{statusPanel[state], spectrumPanel[state]}},
           Spacings -> {1.5, 0}, Alignment -> {Left, Top}],
      Grid[{{ratePanel[state, "CountRate", "Count rate / cps",      $coolColor],
             ratePanel[state, "DoseRate",  "Dose rate / \[Mu]Sv/h", $warmColor]}},
           Spacings -> {1.5, 0}, Alignment -> {Left, Top}]
    }, Spacings -> 1]
  ];

(* HoldFirst so that the Dynamic re-evaluates the symbol passed in
   (typically `streamId`) on every tick.  If the user closes a
   stream and re-opens, streamId is re-bound, and the dashboard
   automatically follows the new stream rather than freezing on the
   string id captured at first call.  Plain string ids still work
   because Dynamic just looks them up unchanged. *)
SetAttributes[RadiaCodeDashboard, HoldFirst];

RadiaCodeDashboard[idExpr_] :=
  Dynamic[Refresh[buildDashboard[idExpr], UpdateInterval -> 1]];

End[];
EndPackage[];
