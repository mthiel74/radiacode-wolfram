(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- Valid sample passes ----- *)

valid = RadiaCodeTools`N42Validate`ValidateN42[
          FileNameJoin[{dataDir, "data_am241.n42"}]];
vt[valid["Valid"], True, TestID -> "valid-sample-passes"];
vt[valid["Issues"], {}, TestID -> "valid-sample-no-issues"];

(* ----- Invalid sample reports issues ----- *)

invalid = RadiaCodeTools`N42Validate`ValidateN42[
            FileNameJoin[{dataDir, "data_invalid.n42"}]];
vt[invalid["Valid"], False, TestID -> "invalid-sample-fails"];
vt[Length[invalid["Issues"]] > 0, True, TestID -> "invalid-sample-has-issues"];

(* ----- Recursive walk returns Dataset ----- *)

scan = RadiaCodeTools`N42Validate`ValidateN42Recursive[dataDir];
vt[Head[scan], Dataset, TestID -> "recursive-returns-dataset"];
vt[Length[Normal[scan]] >= 2, True, TestID -> "recursive-finds-files"];

(* ----- Schema option falls through gracefully on missing xsd ----- *)

withoutSchema = RadiaCodeTools`N42Validate`ValidateN42[
  FileNameJoin[{dataDir, "data_am241.n42"}],
  "Schema" -> "/nonexistent/path.xsd"];
vt[withoutSchema["Valid"], True, TestID -> "missing-schema-no-error"];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["N42Validate.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
