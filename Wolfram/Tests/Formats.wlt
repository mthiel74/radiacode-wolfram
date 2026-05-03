(* ::Package:: *)

(* Tests for RadiaCodeTools`Formats`. Run with:

     wolframscript -file Tests/Formats.wlt
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
AppendResult[r_] := AppendTo[results, r];

vt[expr_, expected_, opts___] := AppendResult[VerificationTest[expr, expected, opts]];

(* ----- FILETIME conversion round-trip ----- *)

vt[
  Module[{ft = 133486128000000000, d},
    d = RadiaCodeTools`Formats`fileTimeToDate[ft];
    Round[RadiaCodeTools`Formats`dateToFileTime[d]] - ft],
  0,
  TestID -> "filetime-roundtrip"
];

vt[
  DateValue[RadiaCodeTools`Formats`fileTimeToDate[133486128000000000],
            {"Year", "Month", "Day"}],
  {2024, 1, 1},
  TestID -> "filetime-known-date"
];

(* ----- RC XML spectrum ----- *)

amSpec = RadiaCodeTools`Formats`ImportRCSpectrum[
  FileNameJoin[{dataDir, "data_am241.xml"}]];

vt[amSpec["SerialNumber"],     "RC-102-000115", TestID -> "am241-serial"];
vt[amSpec["NumberOfChannels"], 1024,           TestID -> "am241-nchan"];
vt[Length[amSpec["Counts"]],   1024,           TestID -> "am241-counts-length"];
vt[amSpec["Counts"][[1]],      1429,           TestID -> "am241-first-count"];

vt[
  amSpec["Calibration"],
  {-6.2832313, 2.4383054, 0.0003818},
  SameTest -> (Norm[#1 - #2] < 10^-6 &),
  TestID -> "am241-calibration"
];

vt[
  DateValue[amSpec["StartTime"], {"Year", "Month", "Day"}],
  {2023, 6, 7},
  TestID -> "am241-start-date"
];

(* ----- spectrum with background ----- *)

thSpec = RadiaCodeTools`Formats`ImportRCSpectrum[
  FileNameJoin[{dataDir, "data_th232_plus_background.xml"}]];

vt[AssociationQ[thSpec["Background"]], True, TestID -> "th232-has-background"];

vt[Length[thSpec["Background", "Counts"]], 1024,
   TestID -> "th232-bg-counts-length"];

(* ----- applyCalibration ----- *)

vt[
  RadiaCodeTools`Formats`applyCalibration[{1, 2, 3}, {0, 1, 2}],
  {1, 6, 17},
  TestID -> "applyCal-basic"
];

(* ----- .rctrk ----- *)

walk = RadiaCodeTools`Formats`ImportRCTrack[
  FileNameJoin[{dataDir, "walk.rctrk"}]];

vt[walk["Header"]["SerialNumber"], "RC-102-999999", TestID -> "rctrk-serial"];
vt[Length[Normal @ walk["Points"]] >= 1, True, TestID -> "rctrk-has-points"];
vt[Normal[walk["Points"]][[1]]["FileTime"], 133486128000000000,
   TestID -> "rctrk-first-filetime"];

(* ----- .rcspg ----- *)

k40 = RadiaCodeTools`Formats`ImportRCSpectrogram[
  FileNameJoin[{dataDir, "K40.rcspg"}]];

vt[k40["NumberOfChannels"], 1024, TestID -> "rcspg-nchan"];
vt[Length[k40["Calibration"]], 3, TestID -> "rcspg-cal-length"];
vt[k40["Header"]["Device serial"], "RC-102-001272", TestID -> "rcspg-serial"];
vt[Length[Normal @ k40["Samples"]] >= 1, True, TestID -> "rcspg-has-samples"];

(* Round-trip: feed the imported spectrogram (whose Samples is a
   Dataset) straight into ExportRCSpectrogram and re-import.  Catches
   the Dataset/list type mismatch that the brutal-critic flagged. *)
vt[
  Module[{tmp, rb},
    tmp = CreateFile[];
    DeleteFile[tmp];
    RadiaCodeTools`Formats`ExportRCSpectrogram[tmp, <|
      "Name" -> "K40 round-trip",
      "SerialNumber" -> "RC-RT-TEST",
      "Comment" -> "round-trip", "Flags" -> "0",
      "Calibration" -> k40["Calibration"],
      "HistoricalCounts" -> k40["HistoricalSpectrum"]["Counts"],
      "HistoricalDuration" -> k40["HistoricalSpectrum"]["Duration"],
      "Timestamp" -> Now,
      "AccumulationTime" -> Quantity[60, "Seconds"],
      "Samples" -> k40["Samples"]   (* still a Dataset *)
    |>];
    rb = RadiaCodeTools`Formats`ImportRCSpectrogram[tmp];
    DeleteFile[tmp];
    AssociationQ[rb] && rb["NumberOfChannels"] === 1024],
  True,
  TestID -> "rcspg-roundtrip-with-dataset-samples"];

(* ----- ndjson log ----- *)

xrayLog = RadiaCodeTools`Formats`ImportNDJsonLog[
  FileNameJoin[{dataDir, "xray.ndjson"}]];

vt[Length[xrayLog["Spectrum"]] > 0, True, TestID -> "ndjson-has-spectra"];
vt[AssociationQ[First[xrayLog["Spectrum"]]], True,
   TestID -> "ndjson-record-shape"];
vt[Length[Lookup[First[xrayLog["Spectrum"]], "counts"]], 1024,
   TestID -> "ndjson-spectrum-channels"];

(* ----- N42 round-trip ----- *)

vt[
  Module[{tmp = CreateFile[]},
    RadiaCodeTools`Formats`ExportN42[tmp, amSpec];
    With[{rb = RadiaCodeTools`Formats`ImportN42[tmp]},
      DeleteFile[tmp];
      And[rb["SerialNumber"] === "RC-102-000115",
          Length[rb["Counts"]] === 1024,
          rb["Counts"][[1]] === 1429,
          Norm[rb["Calibration"] - amSpec["Calibration"]] < 10^-4]]],
  True,
  TestID -> "n42-roundtrip-am241"
];

(* ----- summary ----- *)

passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["Formats.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[
    Function[r,
      If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ",
              Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]
      ]],
    results];
  Exit[1]];
