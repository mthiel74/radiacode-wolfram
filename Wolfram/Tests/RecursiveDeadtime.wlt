(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data_deadtime"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

scan = RadiaCodeTools`RecursiveDeadtime`ScanDeadtime[dataDir,
         FileNameJoin[{dataDir, "bg.xml"}]];

vt[Head[scan], Dataset, TestID -> "scan-returns-dataset"];

rows = Normal[scan];
vt[Length[rows] >= 2, True, TestID -> "scan-finds-both-triplets"];

(* Each row should have a positive deadtime *)
vt[
  And @@ (NumericQ[#["TauUs"]] && #["TauUs"] > 0 & /@ rows),
  True,
  TestID -> "scan-all-tau-positive"
];

(* Saturated flag should be False for these reasonable rates *)
vt[
  Or @@ (#["Saturated"] === False & /@ rows),
  True,
  TestID -> "scan-some-not-saturated"
];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["RecursiveDeadtime.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
