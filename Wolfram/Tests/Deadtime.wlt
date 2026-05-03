(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data_deadtime"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- Numeric inputs (no files) ----- *)

(* Two sources at 1000 cps and 1500 cps, combined at 2400 cps with 50 cps
   background means deadtime is positive and loss fraction is non-negative. *)
dt = RadiaCodeTools`Deadtime`ComputeDeadtime[1000., 1500., 2400., 50.];

vt[NumericQ[dt["TauMicroseconds"]], True, TestID -> "tau-numeric"];
vt[dt["TauMicroseconds"] > 0, True, TestID -> "tau-positive"];
vt[dt["LossFraction"] > 0, True, TestID -> "loss-positive"];
vt[dt["Saturated"], False, TestID -> "not-saturated"];

(* Negative background fails *)
res = RadiaCodeTools`Deadtime`ComputeDeadtime[100., 100., 180., -1.];
vt[FailureQ[res], True, TestID -> "negative-bg-fails"];

(* Zero source rates fail *)
res2 = RadiaCodeTools`Deadtime`ComputeDeadtime[0., 100., 90.];
vt[FailureQ[res2], True, TestID -> "zero-rate-fails"];

(* ----- File-based: Co-60 / Cs-137 ----- *)

bgFile  = FileNameJoin[{dataDir, "bg.xml"}];
co60    = FileNameJoin[{dataDir, "Co60_Cs137", "Co60_a.xml"}];
cs137   = FileNameJoin[{dataDir, "Co60_Cs137", "Cs137_b.xml"}];
co60cs137 = FileNameJoin[{dataDir, "Co60_Cs137", "Co60_a+Cs137_b.xml"}];

dtFiles = RadiaCodeTools`Deadtime`ComputeDeadtimeFromFiles[
            co60, cs137, co60cs137, bgFile];

vt[NumericQ[dtFiles["TauMicroseconds"]], True, TestID -> "files-tau-numeric"];
vt[dtFiles["A"]  > 0, True, TestID -> "files-rate-A"];
vt[dtFiles["B"]  > 0, True, TestID -> "files-rate-B"];
vt[dtFiles["AB"] > 0, True, TestID -> "files-rate-AB"];
vt[dtFiles["BG"] > 0, True, TestID -> "files-rate-BG"];

(* CountRateOfSpectrum sanity *)
spec = RadiaCodeTools`Formats`ImportRCSpectrum[co60];
rate = RadiaCodeTools`Deadtime`CountRateOfSpectrum[spec];
vt[rate > 0, True, TestID -> "count-rate-positive"];
vt[Abs[rate - dtFiles["A"]] < 10^-6, True,
   TestID -> "count-rate-matches-files-A"];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["Deadtime.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
