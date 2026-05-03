(* ::Package:: *)

(* RadiaCodeTools`SpectrogramEnergy`
   Total dose / peak rate from spectrograms (port of spectrogram_energy.py). *)

BeginPackage["RadiaCodeTools`SpectrogramEnergy`",
  {"RadiaCodeTools`Formats`"}];

DoseFromSpectrum::usage =
  "DoseFromSpectrum[counts, {a0, a1, a2}] returns the energy deposited \
in microsieverts assuming a CsI:Tl crystal of 1 cm^3 (density 4.51 g/cm^3). \
Optional Density and Volume override.";

SpectrogramEnergy::usage =
  "SpectrogramEnergy[file] reads a .rcspg file and returns an Association \
with TotalDose (uSv), Duration (Quantity), AverageDoseRate (uSv/hour), \
and PeakDoseRate (uSv/hour).";

ScanSpectrogramEnergy::usage =
  "ScanSpectrogramEnergy[dir] walks a directory and applies \
SpectrogramEnergy to every .rcspg file found.  Returns a Dataset.";

Begin["`Private`"];

$joulesPerKev = 1.60218 * 10^-16;

Options[DoseFromSpectrum] = {
  "Density" -> 4.51,    (* CsI:Tl in g/cm^3 *)
  "Volume"  -> 1.0      (* cm^3 *)
};

DoseFromSpectrum[counts_List, cal_List, OptionsPattern[]] :=
  Module[{d, v, mass, channels, energies, totalKeV, gray, uSv},
    d = OptionValue["Density"];
    v = OptionValue["Volume"];
    mass = d * v * 10.^-3;   (* kg *)
    channels = Range[0, Length[counts] - 1];
    energies = RadiaCodeTools`Formats`applyCalibration[cal, channels];
    totalKeV = Total[energies * counts];
    gray = totalKeV * $joulesPerKev / mass;
    uSv = gray * 10.^6;
    uSv
  ];

SpectrogramEnergy[file_String, opts:OptionsPattern[DoseFromSpectrum]] :=
  Module[{spec, samples, cal, channels, sampleData, doses, accTimes,
          peakRate, totalDose, totalDuration, avgRate},
    spec = RadiaCodeTools`Formats`ImportRCSpectrogram[file];
    If[FailureQ[spec] || spec === $Failed, Return[spec]];
    cal = spec["Calibration"];
    channels = spec["NumberOfChannels"];
    samples = Normal[spec["Samples"]];
    If[Length[samples] === 0, Return[$Failed]];
    sampleData = Function[s,
      Module[{c = s["Deltas"]},
        c = PadRight[c, channels, 0];
        {DoseFromSpectrum[c, cal, opts], s["Duration"]}]
      ] /@ samples;
    {doses, accTimes} = Transpose[sampleData];
    accTimes = Replace[accTimes, x_ /; x === 0 -> 1, {1}];
    peakRate = Max[doses / accTimes];
    totalDose = Total[doses];
    totalDuration = Total[accTimes];
    avgRate = If[totalDuration > 0,
                  3600. * totalDose / totalDuration,
                  0.];
    <|
      "File" -> file,
      "TotalDose" -> totalDose,
      "Duration" -> Quantity[totalDuration, "Seconds"],
      "AverageDoseRate" -> avgRate,
      "PeakDoseRate" -> peakRate * 3600.,
      "SampleCount" -> Length[samples]
    |>
  ];

ScanSpectrogramEnergy[dir_String, opts:OptionsPattern[DoseFromSpectrum]] :=
  Module[{files, results},
    files = FileNames["*.rcspg", dir, Infinity];
    results = Map[Quiet @ SpectrogramEnergy[#, opts] &, files];
    Dataset @ Cases[results, _Association]
  ];

End[];
EndPackage[];
