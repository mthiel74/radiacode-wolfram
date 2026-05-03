(* ::Package:: *)

(* RadiaCodeTools`SpectrumPlot`
   Visualisation helpers for RadiaCode spectra. *)

BeginPackage["RadiaCodeTools`SpectrumPlot`", {"RadiaCodeTools`Formats`"}];

RCSpectrumPlot::usage =
  "RCSpectrumPlot[spec] plots counts vs. energy for a RadiaCode \
spectrum (Association as returned by ImportRCSpectrum).  Pass \
\"Channels\" as a second argument to use channel index on the x-axis.  \
Options: \"Background\" -> True | False | \"Subtract\", \"Scale\" -> \
\"Log\" | \"Linear\", \"Range\" -> Automatic | {emin, emax}.";

EnergyCalibrationCurve::usage =
  "EnergyCalibrationCurve[spec] plots the energy-calibration polynomial \
implied by the spectrum's calibration coefficients across the channel \
range.";

PeakChannels::usage =
  "PeakChannels[spec, n] returns the n highest-count channels (1-indexed) \
of the foreground spectrum together with their energies and counts.";

Begin["`Private`"];

countsToPoints[counts_List, cal_List, axis_String] :=
  Module[{n = Length[counts], chans, xs},
    chans = Range[0, n - 1];
    xs = If[axis === "Energy",
            RadiaCodeTools`Formats`applyCalibration[cal, chans],
            chans];
    Transpose[{xs, counts}]
  ];

Options[RCSpectrumPlot] = {
  "Background"  -> True,
  "Scale"       -> "Log",
  "Range"       -> Automatic,
  "Axis"        -> "Energy",
  "PlotLabel"   -> Automatic
};

RCSpectrumPlot[spec_Association, axis_String, opts:OptionsPattern[]] :=
  RCSpectrumPlot[spec, "Axis" -> axis, opts];

RCSpectrumPlot[spec_Association, OptionsPattern[]] :=
  Module[{counts, cal, bgSpec, bgCounts, bgCal, axis, scale, range,
          fgPoints, bgPoints, plotLabel, xLabel, yLabel, scaleFn,
          mode, plotOpts, plot, dataSets, legend},
    counts = Lookup[spec, "Counts", {}];
    cal = Lookup[spec, "Calibration", {0., 1., 0.}];
    bgSpec = Lookup[spec, "Background", None];
    axis = OptionValue["Axis"];
    scale = OptionValue["Scale"];
    range = OptionValue["Range"];
    mode = OptionValue["Background"];
    plotLabel = OptionValue["PlotLabel"];
    If[plotLabel === Automatic,
       plotLabel = Lookup[spec, "SpectrumName", "Spectrum"]];

    bgCounts = If[AssociationQ[bgSpec], Lookup[bgSpec, "Counts", {}], {}];
    bgCal    = If[AssociationQ[bgSpec], Lookup[bgSpec, "Calibration", cal], cal];

    Which[
      mode === "Subtract" && Length[bgCounts] === Length[counts],
        fgPoints = countsToPoints[counts - bgCounts, cal, axis];
        bgPoints = {};
        legend   = {"Foreground - Background"},

      mode === True && AssociationQ[bgSpec],
        fgPoints = countsToPoints[counts, cal, axis];
        bgPoints = countsToPoints[bgCounts, bgCal, axis];
        legend   = {"Foreground", "Background"},

      True,
        fgPoints = countsToPoints[counts, cal, axis];
        bgPoints = {};
        legend   = {"Foreground"}
    ];

    xLabel = If[axis === "Energy", "Energy / keV", "Channel"];
    yLabel = "Counts";
    scaleFn = If[scale === "Log", ListLogPlot, ListLinePlot];

    dataSets = If[bgPoints === {}, {fgPoints}, {fgPoints, bgPoints}];
    plotOpts = {
      Joined -> True,
      PlotLabel -> plotLabel,
      Frame -> True,
      FrameLabel -> {xLabel, yLabel},
      PlotLegends -> legend,
      ImageSize -> 600,
      GridLines -> Automatic
    };
    If[range =!= Automatic, AppendTo[plotOpts, PlotRange -> {range, All}]];

    plot = scaleFn[dataSets, Sequence @@ plotOpts];
    plot
  ];

EnergyCalibrationCurve[spec_Association] :=
  Module[{cal, n, chans, energies},
    cal = Lookup[spec, "Calibration", {0., 1., 0.}];
    n = Length[Lookup[spec, "Counts", Range[1024]]];
    chans = Range[0, n - 1];
    energies = RadiaCodeTools`Formats`applyCalibration[cal, chans];
    ListLinePlot[Transpose[{chans, energies}],
      Frame -> True,
      FrameLabel -> {"Channel", "Energy / keV"},
      PlotLabel -> "Energy calibration",
      ImageSize -> 500,
      GridLines -> Automatic]
  ];

PeakChannels[spec_Association, n_Integer : 5] :=
  Module[{counts, cal, sorted, chans, energies},
    counts = Lookup[spec, "Counts", {}];
    cal = Lookup[spec, "Calibration", {0., 1., 0.}];
    sorted = Reverse @ Ordering[counts];
    chans = Take[sorted, UpTo[n]];
    energies = RadiaCodeTools`Formats`applyCalibration[cal, chans - 1];
    Dataset @ MapThread[
      <|"Channel" -> #1 - 1, "Energy" -> #2, "Counts" -> counts[[#1]]|> &,
      {chans, energies}]
  ];

End[];
EndPackage[];
