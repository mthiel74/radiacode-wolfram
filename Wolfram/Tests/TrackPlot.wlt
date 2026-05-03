(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

walk = RadiaCodeTools`Formats`ImportRCTrack[
  FileNameJoin[{dataDir, "walk.rctrk"}]];

(* ----- RCTrackPlot returns a renderable graphic ----- *)
p1 = RadiaCodeTools`TrackPlot`RCTrackPlot[walk];
vt[MatchQ[p1, _Graphics | _Legended | HoldPattern[GeoGraphics[___]]],
   True, TestID -> "track-plot-renderable"];

(* ----- Color = None gives a Joined line plot ----- *)
p2 = RadiaCodeTools`TrackPlot`RCTrackPlot[walk, "Color" -> None];
vt[MatchQ[p2, _Graphics | _Legended | HoldPattern[GeoGraphics[___]]],
   True, TestID -> "track-plot-no-color"];

(* ----- Histogram is a Graphics ----- *)
h = RadiaCodeTools`TrackPlot`RCTrackHistogram[walk];
vt[MatchQ[h, _Graphics | _Legended], True, TestID -> "track-histogram"];

(* ----- Filter by accuracy ----- *)
nAll = Length @ RadiaCodeTools`TrackPlot`RCTrackPoints[walk];
nFiltered = Length @ RadiaCodeTools`TrackPlot`RCTrackPoints[walk,
              "AccuracyFilter" -> 5.0];
vt[nFiltered <= nAll, True, TestID -> "accuracy-filter-monotone"];

(* ----- Downsampling reduces or preserves point count ----- *)
nDs = Length @ RadiaCodeTools`TrackPlot`RCTrackPoints[walk,
        "Downsample" -> 1000.];
vt[nDs <= nAll, True, TestID -> "downsample-monotone"];

(* ----- Accept a plain Dataset (not the wrapping Association) ----- *)
p3 = RadiaCodeTools`TrackPlot`RCTrackPlot[walk["Points"]];
vt[MatchQ[p3, _Graphics | _Legended | HoldPattern[GeoGraphics[___]]],
   True, TestID -> "track-plot-dataset-input"];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["TrackPlot.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
