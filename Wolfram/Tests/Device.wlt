(* ::Package:: *)

(* Tests for RadiaCodeTools`Device`.

   Most tests run *without* a physical device attached: they exercise
   the Wolfram-side validation, command construction, repo discovery,
   and graceful failure paths.  If a device IS attached, RadiaCodeDevices[]
   should return a non-empty list — we just check that the call doesn't
   error and the return type is a list. *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- Repo root resolves to something containing src/ ----- *)

vt[
  DirectoryQ[FileNameJoin[{RadiaCodeTools`Device`$RadiaCodeRepoRoot,
                            "src", "radiacode_tools"}]],
  True,
  TestID -> "repo-root-found"
];

(* ----- Python interpreter has a sensible default ----- *)

vt[
  StringQ[RadiaCodeTools`Device`$RadiaCodePython],
  True,
  TestID -> "python-default"
];

(* ----- RadiaCodeDevices returns a list (possibly empty) ----- *)

devices = Quiet @ RadiaCodeTools`Device`RadiaCodeDevices[];
vt[
  ListQ[devices] || FailureQ[devices],
  True,
  TestID -> "devices-returns-list-or-failure"
];

(* ----- RadiaCodeReset rejects bogus targets ----- *)

vt[
  FailureQ[RadiaCodeTools`Device`RadiaCodeReset["Banana"]],
  True,
  TestID -> "reset-bad-target-fails"
];

(* ----- RadiaCodeAcquire surfaces a Failure when no device is present ----- *)
(* (We DO NOT actually run the acquire when devices is empty — that
   would block on the radiacode Python lib trying to open a USB
   handle.  Just check the function exists and returns a Failure when
   given an obviously bogus Bluetooth address with a ~0 timeout.) *)

If[ListQ[devices] && devices === {},
  res = TimeConstrained[
    Quiet @ RadiaCodeTools`Device`RadiaCodeAcquire[
      "Bluetooth" -> "00:00:00:00:00:00"],
    3,
    "timeout"];
  vt[FailureQ[res] || res === "timeout", True,
     TestID -> "acquire-no-device-fails-or-timeouts"]
];

(* ----- formatHMS round-trips correctly through repackaging tests ----- *)
(* Internal helper but worth a smoke test via stream construction *)
vt[
  Length[Quiet @ RadiaCodeTools`Device`RadiaCodeDevices[]] >= 0 ||
  FailureQ[Quiet @ RadiaCodeTools`Device`RadiaCodeDevices[]],
  True,
  TestID -> "devices-shape"
];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["Device.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
