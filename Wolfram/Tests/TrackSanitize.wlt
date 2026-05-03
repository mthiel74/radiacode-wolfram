(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

walk = RadiaCodeTools`Formats`ImportRCTrack[
  FileNameJoin[{dataDir, "walk.rctrk"}]];

san = RadiaCodeTools`TrackSanitize`SanitizeTrack[walk];

(* ----- header rebased ----- *)

vt[
  san["Header"]["SerialNumber"],
  "RC-100-314159",
  TestID -> "serial-replaced"
];

vt[
  san["Header"]["Comment"],
  "And I ... was never here.",
  TestID -> "comment-replaced"
];

vt[
  StringStartsQ[san["Header"]["Name"], "sanitized_"],
  True,
  TestID -> "name-prefixed"
];

(* ----- coordinates rebased to base ----- *)

points = Normal[san["Points"]];
lats = Lookup[#, "Latitude"] & /@ points;
lons = Lookup[#, "Longitude"] & /@ points;

vt[
  N[Min[lats]],
  43.5833323,
  SameTest -> (Abs[#1 - #2] < 10^-5 &),
  TestID -> "min-lat-equals-base"
];

vt[
  N[Min[lons]],
  -55.9269664,
  SameTest -> (Abs[#1 - #2] < 10^-5 &),
  TestID -> "min-lon-equals-base"
];

(* ----- time rebased: first point becomes 1984-12-05 ----- *)

t0 = Lookup[First[points], "Time"];
vt[
  DateValue[t0, {"Year", "Month", "Day"}],
  {1984, 12, 5},
  TestID -> "start-date"
];

(* ----- relative shape preserved (deltas unchanged) ----- *)

origPoints = Normal[walk["Points"]];
origLats = Lookup[#, "Latitude"] & /@ origPoints;
origLatDelta = origLats[[2]] - origLats[[1]];
sanLatDelta = lats[[2]] - lats[[1]];

vt[
  Abs[origLatDelta - sanLatDelta] < 10^-6,
  True,
  TestID -> "lat-deltas-preserved"
];

(* ----- KeepSerial preserves the original ----- *)

san2 = RadiaCodeTools`TrackSanitize`SanitizeTrack[walk, "KeepSerial" -> True];
vt[
  san2["Header"]["SerialNumber"],
  "RC-102-999999",
  TestID -> "keep-serial"
];

(* ----- ReverseRoute reverses point order ----- *)

san3 = RadiaCodeTools`TrackSanitize`SanitizeTrack[walk, "ReverseRoute" -> True];
revPts = Normal[san3["Points"]];
vt[
  {Lookup[First[revPts], "Latitude"], Lookup[First[revPts], "Longitude"]},
  {Lookup[Last[points], "Latitude"], Lookup[Last[points], "Longitude"]},
  SameTest -> (Norm[#1 - #2] < 10^-6 &),
  TestID -> "reverse-first-equals-last"
];

(* ----- File round-trip ----- *)

tmp = CreateFile[];
DeleteFile[tmp];
res = RadiaCodeTools`TrackSanitize`SanitizeTrackFile[
        FileNameJoin[{dataDir, "walk.rctrk"}], tmp];
vt[FileExistsQ[tmp], True, TestID -> "file-out-written"];
readBack = RadiaCodeTools`Formats`ImportRCTrack[tmp];
vt[
  readBack["Header"]["SerialNumber"],
  "RC-100-314159",
  TestID -> "file-roundtrip-serial"
];
DeleteFile[tmp];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["TrackSanitize.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
