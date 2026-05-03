(* ::Package:: *)

(* RadiaCodeTools`TrackPlot`
   GPS-track visualisation built on `GeoListPlot` and `Histogram`. *)

BeginPackage["RadiaCodeTools`TrackPlot`", {"RadiaCodeTools`Formats`"}];

RCTrackPlot::usage =
  "RCTrackPlot[track] renders a GeoListPlot of an .rctrk track \
(Association from ImportRCTrack, or its Points Dataset).  Colours \
points by dose rate by default.  Options: \"Color\" -> \"DoseRate\" | \
\"CountRate\" | None, \"AccuracyFilter\" -> n (drop points with \
accuracy > n metres), \"Downsample\" -> distance in metres, \"Palette\" \
-> built-in colour data name.";

RCTrackHistogram::usage =
  "RCTrackHistogram[track, field] returns a histogram of a numeric \
column of the track.  field defaults to \"DoseRate\".";

RCTrackPoints::usage =
  "RCTrackPoints[track] returns a list of {latitude, longitude} pairs \
for the track points, after the same accuracy / downsample filtering \
RCTrackPlot would apply.";

Begin["`Private`"];

extractRows[track_] := Which[
  AssociationQ[track] && KeyExistsQ[track, "Points"],
    Normal[track["Points"]],
  Head[track] === Dataset, Normal[track],
  ListQ[track], track,
  True, $Failed
];

(* Haversine via Wolfram's GeoDistance is exact-ish; use it for
   downsampling so we don't reinvent the wheel. *)
(* Distance-thinning downsampler.  Reap/Sow keeps the running
   "last accepted" point in a single mutable cell while emitting the
   accepted points to the Reap collector -- avoids the O(n^2)
   AppendTo + Last[kept] pattern.  Tracks are typically a few
   hundred to a few thousand points; this version stays linear even
   for multi-hour surveys. *)
downsampleByDistance[rows_List, minDistance_?NumericQ] :=
  Module[{last = First[rows], next, d, sown},
    sown = Reap[
      Sow[last];
      Do[
        next = rows[[i]];
        d = QuantityMagnitude @ GeoDistance[
              {last["Latitude"],  last["Longitude"]},
              {next["Latitude"],  next["Longitude"]},
              UnitSystem -> "Metric"];
        If[d >= minDistance, Sow[next]; last = next],
        {i, 2, Length[rows]}]
    ][[2]];
    If[sown === {}, {First[rows]}, First[sown]]
  ];

filterByAccuracy[rows_List, max_?NumericQ] :=
  Select[rows, NumericQ[#["Accuracy"]] && #["Accuracy"] <= max &];

Options[RCTrackPlot] = {
  "Color"          -> "DoseRate",
  "AccuracyFilter" -> Infinity,
  "Downsample"     -> None,
  "Palette"        -> "TemperatureMap",
  "ImageSize"      -> 600,
  "PlotLabel"      -> Automatic
};

RCTrackPlot[track_, OptionsPattern[]] :=
  Module[{rows, color, accFilter, ds, palette, label, lats, lons,
          geoPoints, values, plot, plotLabel,
          padLat, padLon, geoRange},
    rows = extractRows[track];
    If[rows === $Failed, Return[$Failed]];
    color     = OptionValue["Color"];
    accFilter = OptionValue["AccuracyFilter"];
    ds        = OptionValue["Downsample"];
    palette   = OptionValue["Palette"];
    plotLabel = OptionValue["PlotLabel"];
    If[NumericQ[accFilter] && accFilter < Infinity,
       rows = filterByAccuracy[rows, accFilter]];
    If[NumericQ[ds] && Length[rows] > 1,
       rows = downsampleByDistance[rows, ds]];
    If[Length[rows] === 0, Return[$Failed]];
    lats = Lookup[#, "Latitude"] & /@ rows;
    lons = Lookup[#, "Longitude"] & /@ rows;
    geoPoints = GeoPosition[{#["Latitude"], #["Longitude"]}] & /@ rows;
    If[plotLabel === Automatic,
       plotLabel = If[AssociationQ[track] && KeyExistsQ[track, "Header"],
                       Lookup[track["Header"], "Name", "Track"],
                       "Track"]];
    values = Switch[color,
      "DoseRate",  Lookup[#, "DoseRate", 0] & /@ rows,
      "CountRate", Lookup[#, "CountRate", 0] & /@ rows,
      _, None];
    If[values === None,
      Return[GeoListPlot[geoPoints,
        ImageSize -> OptionValue["ImageSize"],
        PlotLabel -> plotLabel,
        Joined -> True]]];

    (* Per-point colour styling.  Style[GeoPosition[...], colour]
       inside GeoListPlot is silently dropped on most Wolfram versions
       (every point comes back the same colour), so build the figure
       from explicit GeoGraphics primitives instead. *)
    Module[{vmin, vmax, scaled, colorFn, primitives},
      {vmin, vmax} = MinMax[values];
      scaled = If[vmax > vmin,
                   (values - vmin)/(vmax - vmin),
                   ConstantArray[0.5, Length[values]]];
      colorFn = ColorData[palette];
      primitives = MapThread[
        {colorFn[#2], Point[#1]} &, {geoPoints, scaled}];
      padLat = Max[0.0005, 0.05 (Max[lats] - Min[lats])];
      padLon = Max[0.0005, 0.05 (Max[lons] - Min[lons])];
      geoRange = {{Min[lats] - padLat, Max[lats] + padLat},
                  {Min[lons] - padLon, Max[lons] + padLon}};
      Legended[
        GeoGraphics[
          {PointSize[0.012], primitives},
          GeoRange       -> geoRange,
          GeoBackground  -> "StreetMap",
          ImageSize      -> OptionValue["ImageSize"],
          PlotLabel      -> plotLabel],
        BarLegend[{palette, {vmin, vmax}}, LegendLabel -> color]]
    ]
  ];

RCTrackHistogram[track_, field_String : "DoseRate"] :=
  Module[{rows, vals},
    rows = extractRows[track];
    If[rows === $Failed, Return[$Failed]];
    vals = Lookup[#, field, Missing[]] & /@ rows;
    vals = Cases[vals, _?NumericQ];
    Histogram[vals, Automatic, "PDF",
      Frame -> True,
      FrameLabel -> {field, "Density"},
      ImageSize -> 500]
  ];

RCTrackPoints[track_, opts:OptionsPattern[RCTrackPlot]] :=
  Module[{rows, accFilter, ds},
    rows = extractRows[track];
    If[rows === $Failed, Return[{}]];
    accFilter = OptionValue[RCTrackPlot, {opts}, "AccuracyFilter"];
    ds        = OptionValue[RCTrackPlot, {opts}, "Downsample"];
    If[NumericQ[accFilter] && accFilter < Infinity,
       rows = filterByAccuracy[rows, accFilter]];
    If[NumericQ[ds] && Length[rows] > 1,
       rows = downsampleByDistance[rows, ds]];
    {#["Latitude"], #["Longitude"]} & /@ rows
  ];

End[];
EndPackage[];
