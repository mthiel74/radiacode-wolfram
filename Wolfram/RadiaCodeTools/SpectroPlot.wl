(* ::Package:: *)

(* RadiaCodeTools`SpectroPlot`
   Spectrogram heatmap (channels x time) — port of rcspectroplot.py. *)

BeginPackage["RadiaCodeTools`SpectroPlot`",
  {"RadiaCodeTools`Formats`"}];

RCSpectroPlot::usage =
  "RCSpectroPlot[file] reads a .rcspg file (or accepts an Association \
returned by ImportRCSpectrogram) and renders an ArrayPlot of channel x \
sample intensity.  Options:\n\
  \"Scale\"          -> \"Log\" | \"Linear\"\n\
  \"Palette\"        -> \"TemperatureMap\" | any ColorData name\n\
  \"SampleRange\"    -> Automatic | {first, last}   (1-indexed)\n\
  \"DurationRange\"  -> Automatic | {tstart, tend}  (Quantities)\n\
  \"TimeRange\"      -> Automatic | {DateObject, DateObject}\n\
  \"ChannelRange\"   -> Automatic | {first, last}\n\
  \"ImageSize\"      -> 800";

Begin["`Private`"];

resolveSpectrogram[arg_String] :=
  RadiaCodeTools`Formats`ImportRCSpectrogram[arg];
resolveSpectrogram[arg_Association] := arg;

filterByRange[matrix_, samples_, sampleRange_, durRange_, timeRange_] :=
  Module[{n = Length[samples], idx = Range[Length[samples]], starts, durs},
    If[ListQ[sampleRange] && Length[sampleRange] === 2,
      idx = Range @@ sampleRange];
    If[ListQ[durRange] && Length[durRange] === 2 && Length[samples] > 0,
      starts = Accumulate[Lookup[#, "Duration"] & /@ samples];
      durs = QuantityMagnitude[durRange];
      idx = Select[idx, durs[[1]] <= starts[[#]] - First[Lookup[#, "Duration"] & /@ samples] <= durs[[2]] &]];
    If[ListQ[timeRange] && Length[timeRange] === 2,
      idx = Select[idx,
        AbsoluteTime[timeRange[[1]]] <= AbsoluteTime[samples[[#]]["Time"]] <= AbsoluteTime[timeRange[[2]]] &]];
    {matrix[[idx]], samples[[idx]]}
  ];

Options[RCSpectroPlot] = {
  "Scale"         -> "Log",
  "Palette"       -> "TemperatureMap",
  "SampleRange"   -> Automatic,
  "DurationRange" -> Automatic,
  "TimeRange"     -> Automatic,
  "ChannelRange"  -> Automatic,
  "ImageSize"     -> 800
};

RCSpectroPlot[arg_, OptionsPattern[]] :=
  Module[{spec, matrix, samples, scale, palette, sampleR, durR, timeR,
          chanR, plotMatrix, ticks, label, nSamples, nChannels},
    spec = resolveSpectrogram[arg];
    If[FailureQ[spec] || spec === $Failed, Return[spec]];
    samples = Normal[spec["Samples"]];
    matrix = spec["DeltaMatrix"];
    If[Length[matrix] === 0, Return[$Failed]];
    scale     = OptionValue["Scale"];
    palette   = OptionValue["Palette"];
    sampleR   = OptionValue["SampleRange"];
    durR      = OptionValue["DurationRange"];
    timeR     = OptionValue["TimeRange"];
    chanR     = OptionValue["ChannelRange"];

    {matrix, samples} = filterByRange[matrix, samples,
      If[sampleR === Automatic, Null, sampleR],
      If[durR    === Automatic, Null, durR],
      If[timeR   === Automatic, Null, timeR]];

    If[ListQ[chanR] && Length[chanR] === 2,
      matrix = #[[chanR[[1]] ;; chanR[[2]]]] & /@ matrix];

    nSamples  = Length[matrix];
    nChannels = If[nSamples > 0, Length[First[matrix]], 0];

    plotMatrix = If[scale === "Log",
                    Log[1 + matrix],
                    matrix];

    label = Lookup[spec["Header"], "Spectrogram", "Spectrogram"];

    ArrayPlot[plotMatrix,
      ColorFunction -> ColorData[palette],
      Frame -> True,
      FrameLabel -> {"Channel", "Sample / time"},
      PlotLabel -> label,
      ImageSize -> OptionValue["ImageSize"],
      AspectRatio -> 1/3,
      DataReversed -> False]
  ];

End[];
EndPackage[];
