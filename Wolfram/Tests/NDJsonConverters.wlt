(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- ndjson -> .rcspg ----- *)

xrayJson = FileNameJoin[{dataDir, "xray.ndjson"}];
spgOut = CreateFile[];
DeleteFile[spgOut];
res = RadiaCodeTools`RCSpgFromJson`ConvertNDJsonToRcspg[xrayJson, spgOut];

vt[FileExistsQ[spgOut], True, TestID -> "spg-output-exists"];

readBack = RadiaCodeTools`Formats`ImportRCSpectrogram[spgOut];

vt[
  AssociationQ[readBack],
  True,
  TestID -> "spg-readable"
];

vt[
  readBack["NumberOfChannels"] === 1024,
  True,
  TestID -> "spg-channels"
];

vt[
  readBack["Header"]["Device serial"],
  "RC-103-000070",
  TestID -> "spg-serial-from-json"
];

vt[
  Length[Normal[readBack["Samples"]]] >= 1,
  True,
  TestID -> "spg-has-samples"
];
DeleteFile[spgOut];

(* ----- ndjson -> .rctrk ----- *)

(* xray.ndjson has no GPS records, so this dataset won't produce points;
   we only smoke-test the converter on it without points. *)

trkOut = CreateFile[];
DeleteFile[trkOut];
RadiaCodeTools`RCTrkFromJson`ConvertNDJsonToRctrk[xrayJson, trkOut];
vt[FileExistsQ[trkOut], True, TestID -> "trk-output-exists-empty"];

(* Use the broken_arrow file which DOES have GPS records *)
ndjGps = FileNameJoin[{dataDir, "broken_arrow_errrr_airplane.ndjson"}];
log = RadiaCodeTools`Formats`ImportNDJsonLog[ndjGps];
hasGps = Length[log["GPS"]] > 0;

If[hasGps,
  trk2 = CreateFile[];
  DeleteFile[trk2];
  RadiaCodeTools`RCTrkFromJson`ConvertNDJsonToRctrk[ndjGps, trk2];
  trkRead = RadiaCodeTools`Formats`ImportRCTrack[trk2];
  vt[Head[trkRead], Association, TestID -> "trk-from-gps-readable"];
  DeleteFile[trk2]
];

DeleteFile[trkOut];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["NDJsonConverters.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
