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
nAll = Length @ Normal[walk["Points"]];

(* ----- No filters returns same point count ----- *)

noFilter = RadiaCodeTools`TrackEdit`EditTrack[walk];
vt[Length @ Normal[noFilter["Points"]], nAll, TestID -> "no-filter-preserves-all"];

(* ----- Track name appended " (edit)" by default ----- *)
vt[
  StringEndsQ[noFilter["Header"]["Name"], " (edit)"],
  True,
  TestID -> "default-name-suffix"
];

(* ----- Custom track name preserved ----- *)
named = RadiaCodeTools`TrackEdit`EditTrack[walk, "TrackName" -> "Subset A"];
vt[named["Header"]["Name"], "Subset A", TestID -> "custom-track-name"];

(* ----- Time include filter ----- *)

allPts = Normal[walk["Points"]];
midTime = Lookup[allPts[[Floor[Length[allPts]/2]]], "Time"];
firstTime = Lookup[First[allPts], "Time"];

beforeMid = RadiaCodeTools`TrackEdit`EditTrack[walk,
  "IncludeTimeRanges" -> {{firstTime, midTime}}];

vt[
  Length @ Normal[beforeMid["Points"]] < nAll &&
  Length @ Normal[beforeMid["Points"]] > 0,
  True,
  TestID -> "time-include-cuts-some"
];

(* ----- Time exclude filter is the complement ----- *)

afterMid = RadiaCodeTools`TrackEdit`EditTrack[walk,
  "ExcludeTimeRanges" -> {{firstTime, midTime}}];

vt[
  Length @ Normal[beforeMid["Points"]] +
  Length @ Normal[afterMid["Points"]] === nAll,
  True,
  TestID -> "include-exclude-partition"
];

(* ----- Bounding box include ----- *)

(* Pick a tiny box around the first point to keep ~1 point. *)
firstLat = Lookup[First[allPts], "Latitude"];
firstLon = Lookup[First[allPts], "Longitude"];

tinyBox = RadiaCodeTools`TrackEdit`EditTrack[walk,
  "IncludeBoundingBoxes" -> {{{firstLat - 10^-7, firstLon - 10^-7},
                              {firstLat + 10^-7, firstLon + 10^-7}}}];
vt[
  Length @ Normal[tinyBox["Points"]] >= 1,
  True,
  TestID -> "bbox-include-has-points"
];

vt[
  Length @ Normal[tinyBox["Points"]] < nAll,
  True,
  TestID -> "bbox-include-trims"
];

(* ----- Radius include ----- *)

withinTen = RadiaCodeTools`TrackEdit`EditTrack[walk,
  "IncludeRadii" -> {{{firstLat, firstLon}, 10.}}];
vt[
  Length @ Normal[withinTen["Points"]] >= 1,
  True,
  TestID -> "radius-include-has-points"
];

(* ----- File round-trip ----- *)
tmp = CreateFile[];
DeleteFile[tmp];
RadiaCodeTools`TrackEdit`EditTrackFile[
  FileNameJoin[{dataDir, "walk.rctrk"}], tmp,
  "ExcludeTimeRanges" -> {{firstTime, midTime}}];
readBack = RadiaCodeTools`Formats`ImportRCTrack[tmp];
vt[
  Length @ Normal[readBack["Points"]] === Length @ Normal[afterMid["Points"]],
  True,
  TestID -> "file-roundtrip-point-count"
];
DeleteFile[tmp];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["TrackEdit.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
