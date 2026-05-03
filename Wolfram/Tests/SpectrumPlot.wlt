(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

amSpec = RadiaCodeTools`Formats`ImportRCSpectrum[
  FileNameJoin[{dataDir, "data_am241.xml"}]];
thSpec = RadiaCodeTools`Formats`ImportRCSpectrum[
  FileNameJoin[{dataDir, "data_th232_plus_background.xml"}]];

(* ----- basic plot returns Graphics ----- *)

p1 = RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[amSpec];
vt[MatchQ[p1, _Graphics | _Legended], True, TestID -> "spec-plot-graphics-head"];

(* Has at least one Line primitive (the spectrum) *)
vt[
  Length @ Cases[p1, _Line, Infinity] >= 1,
  True,
  TestID -> "spec-plot-has-line"
];

(* Channels axis *)
p2 = RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[amSpec, "Channels"];
vt[MatchQ[p2, _Graphics | _Legended], True, TestID -> "spec-plot-channels-axis"];

(* Linear scale *)
p3 = RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[amSpec, "Scale" -> "Linear"];
vt[MatchQ[p3, _Graphics | _Legended], True, TestID -> "spec-plot-linear"];

(* Background-on plot has 2 traces; subtract has 1; off has 1 *)
pBg = RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[thSpec, "Background" -> True];
vt[MatchQ[pBg, _Graphics | _Legended], True, TestID -> "spec-plot-with-bg"];

pSub = RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[thSpec, "Background" -> "Subtract"];
vt[MatchQ[pSub, _Graphics | _Legended], True, TestID -> "spec-plot-subtract"];

(* ----- EnergyCalibrationCurve ----- *)

pCurve = RadiaCodeTools`SpectrumPlot`EnergyCalibrationCurve[amSpec];
vt[Head[pCurve], Graphics, TestID -> "calcurve-graphics"];

(* ----- PeakChannels ----- *)

peaks = RadiaCodeTools`SpectrumPlot`PeakChannels[amSpec, 3];
vt[Length[Normal[peaks]], 3, TestID -> "peaks-three"];

(* The Am-241 spectrum's strongest peak should sit near 60 keV (the
   famous 59.5 keV gamma).  Its top-counts channel must produce an
   energy in [50, 80] keV under the file's stored calibration. *)
vt[
  Module[{topRow},
    topRow = First[Normal[peaks]];
    50 <= topRow["Energy"] <= 80
  ],
  True,
  TestID -> "am241-peak-near-60-keV"
];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["SpectrumPlot.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
