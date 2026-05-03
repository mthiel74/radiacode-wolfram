#!/usr/bin/env wolframscript
(* ::Package:: *)

(* RadiaCodeTools demo
   ===================
   Walks through the four main tools end-to-end against the sample
   data shipped under tests/data/ in the parent repository.

   Run from the repo root with:
       wolframscript -file Wolfram/Notebooks/Demo.wls

   Or open in Mathematica and evaluate cell-by-cell. *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

(* ::Section:: *)
(* 1. Spectrum read & plot *)

amSpec = RadiaCodeTools`Formats`ImportRCSpectrum[
           FileNameJoin[{dataDir, "data_am241.xml"}]];
Print["Loaded Am-241 spectrum: ", amSpec["SpectrumName"],
      ", serial ", amSpec["SerialNumber"],
      ", duration ", amSpec["Duration"]];

specPlot = RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[amSpec];
Export[FileNameJoin[{DirectoryName[$InputFileName], "demo_spectrum.png"}],
       specPlot];
Print["  → wrote demo_spectrum.png"];

peaks = RadiaCodeTools`SpectrumPlot`PeakChannels[amSpec, 5];
Print["Top-5 peak channels:"];
Print[peaks];

(* ::Section:: *)
(* 2. Calibration fit *)

multiSource = {
  {9, 26}, {21, 60},
  {28, 80}, {59, 166}, {109, 303}, {128, 356},
  {14, 40}, {44, 122}, {88, 245}, {124, 344}, {395, 1098}, {507, 1408},
  {526, 1461},
  {106, 295}, {126, 352}, {219, 609}, {403, 1120}, {635, 1765}, {793, 2204},
  {184, 511}, {459, 1275},
  {121, 338}, {210, 583}, {328, 911}, {572, 1588}, {941, 2614}};

fit = RadiaCodeTools`Calibrate`FitCalibration[multiSource];
Print[];
Print[RadiaCodeTools`Calibrate`CalibrationSummary[fit]];

calCurve = RadiaCodeTools`SpectrumPlot`EnergyCalibrationCurve[
             <|"Calibration" -> fit["Coefficients"],
               "Counts" -> ConstantArray[0, 1024]|>];
Export[FileNameJoin[{DirectoryName[$InputFileName], "demo_calibration.png"}],
       calCurve];
Print["  → wrote demo_calibration.png"];

(* ::Section:: *)
(* 3. N42 conversion *)

n42Out = FileNameJoin[{DirectoryName[$InputFileName], "demo_am241.n42"}];
If[FileExistsQ[n42Out], DeleteFile[n42Out]];
RadiaCodeTools`N42Convert`ConvertRCToN42[
  FileNameJoin[{dataDir, "data_am241.xml"}], n42Out];
Print[];
Print["Wrote N42: ", n42Out, " (", FileByteCount[n42Out], " bytes)"];

readBack = RadiaCodeTools`Formats`ImportN42[n42Out];
Print["  Read-back: serial=", readBack["SerialNumber"],
      ", channels=", Length[readBack["Counts"]],
      ", calibration=", readBack["Calibration"]];

(* ::Section:: *)
(* 4. GPS track plot *)

walk = RadiaCodeTools`Formats`ImportRCTrack[
  FileNameJoin[{dataDir, "walk.rctrk"}]];
Print[];
Print["Loaded track ", walk["Header"]["Name"], " (",
      Length[Normal[walk["Points"]]], " points)"];

trackPlot = RadiaCodeTools`TrackPlot`RCTrackPlot[walk,
              "Color" -> "DoseRate",
              "AccuracyFilter" -> 10.0];
Export[FileNameJoin[{DirectoryName[$InputFileName], "demo_track.png"}],
       trackPlot];
Print["  → wrote demo_track.png"];

doseHist = RadiaCodeTools`TrackPlot`RCTrackHistogram[walk, "DoseRate"];
Export[FileNameJoin[{DirectoryName[$InputFileName], "demo_dose_histogram.png"}],
       doseHist];
Print["  → wrote demo_dose_histogram.png"];

Print[];
Print["Demo complete. Outputs in Wolfram/Notebooks/."];
