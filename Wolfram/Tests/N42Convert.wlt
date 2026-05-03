(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- Convert AM-241 sample ----- *)

amIn  = FileNameJoin[{dataDir, "data_am241.xml"}];
amRef = FileNameJoin[{dataDir, "data_am241.n42"}];   (* upstream Python output *)
amOut = CreateFile[];
DeleteFile[amOut];   (* CreateFile makes an empty file; remove so Overwrite isn't needed *)

result = RadiaCodeTools`N42Convert`ConvertRCToN42[amIn, amOut];
vt[FileExistsQ[amOut], True, TestID -> "n42-out-written"];

(* Read back and compare semantic fields *)
ourN42 = RadiaCodeTools`Formats`ImportN42[amOut];
refN42 = RadiaCodeTools`Formats`ImportN42[amRef];

vt[ourN42["SerialNumber"], refN42["SerialNumber"],
   TestID -> "serial-matches-upstream"];

vt[
  Length[ourN42["Counts"]] === Length[refN42["Counts"]] &&
  ourN42["Counts"] === refN42["Counts"],
  True,
  TestID -> "counts-byte-equal"
];

vt[
  Norm[ourN42["Calibration"] - refN42["Calibration"]],
  0.,
  SameTest -> (#1 < 10^-4 &),
  TestID -> "calibration-matches"
];

DeleteFile[amOut];

(* ----- Auto-generated output name ----- *)
amOut2 = amIn <> ".n42";
If[FileExistsQ[amOut2], DeleteFile[amOut2]];
RadiaCodeTools`N42Convert`ConvertRCToN42[amIn];
vt[FileExistsQ[amOut2], True, TestID -> "auto-named-output"];
DeleteFile[amOut2];

(* ----- Overwrite refuses by default ----- *)
existing = CreateFile[];
res = RadiaCodeTools`N42Convert`ConvertRCToN42[amIn, existing];
vt[FailureQ[res], True, TestID -> "no-overwrite-returns-failure"];
res2 = RadiaCodeTools`N42Convert`ConvertRCToN42[amIn, existing, "Overwrite" -> True];
vt[FileExistsQ[existing] && FileByteCount[existing] > 0, True,
   TestID -> "overwrite-true-succeeds"];
DeleteFile[existing];

(* ----- Background merge ----- *)
thIn = FileNameJoin[{dataDir, "data_th232_plus_background.xml"}];
thOut = CreateFile[];
DeleteFile[thOut];
RadiaCodeTools`N42Convert`ConvertRCToN42[thIn, thOut];
thBack = RadiaCodeTools`Formats`ImportN42[thOut];
vt[AssociationQ[thBack["Background"]], True,
   TestID -> "background-carried-through"];
DeleteFile[thOut];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["N42Convert.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
