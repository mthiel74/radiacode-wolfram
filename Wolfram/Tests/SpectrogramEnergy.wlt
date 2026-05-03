(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- DoseFromSpectrum direct call ----- *)

(* A flat 1-count-per-channel spectrum at 1 keV/channel: total energy is
   sum of channels = n*(n-1)/2 keV.  For n=1024 that's 523776 keV, mass
   default = 4.51e-3 kg, dose = 523776 * 1.60218e-16 / 4.51e-3 * 1e6
   ≈ 1.86e-2 uSv. *)

oneCounts = ConstantArray[1, 1024];
flatCal   = {0., 1., 0.};
dose0 = RadiaCodeTools`SpectrogramEnergy`DoseFromSpectrum[oneCounts, flatCal];
expected0 = 523776 * 1.60218*^-16 / 4.51*^-3 * 10^6;

vt[
  Abs[dose0 - expected0] < 10^-6,
  True,
  TestID -> "dose-flat-spectrum"
];

(* ----- SpectrogramEnergy on K40.rcspg ----- *)
(* Cross-checked against `python3 src/spectrogram_energy.py`:
     "tests/data/K40.rcspg: 2.12uSv in 43510s | 0.18uSv/hr | peak: 0.24uSv/hr"  *)

k40 = RadiaCodeTools`SpectrogramEnergy`SpectrogramEnergy[
        FileNameJoin[{dataDir, "K40.rcspg"}]];

vt[
  Abs[k40["TotalDose"] - 2.12] < 0.05,
  True,
  TestID -> "k40-total-dose-uSv"
];

vt[
  QuantityMagnitude[k40["Duration"]],
  43510,
  TestID -> "k40-duration"
];

vt[
  Abs[k40["AverageDoseRate"] - 0.18] < 0.02,
  True,
  TestID -> "k40-avg-rate-uSv-per-hr"
];

vt[
  Abs[k40["PeakDoseRate"] - 0.24] < 0.02,
  True,
  TestID -> "k40-peak-rate-uSv-per-hr"
];

(* ----- ScanSpectrogramEnergy ----- *)

scan = RadiaCodeTools`SpectrogramEnergy`ScanSpectrogramEnergy[dataDir];
vt[Head[scan], Dataset, TestID -> "scan-returns-dataset"];
vt[Length[Normal[scan]] >= 1, True, TestID -> "scan-finds-files"];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["SpectrogramEnergy.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
