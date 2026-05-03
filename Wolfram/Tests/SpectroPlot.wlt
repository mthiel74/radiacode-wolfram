(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- File-arg form returns Graphics ----- *)

p1 = RadiaCodeTools`SpectroPlot`RCSpectroPlot[
       FileNameJoin[{dataDir, "K40.rcspg"}]];
vt[MatchQ[p1, _Graphics | _Legended], True, TestID -> "spectroplot-from-file"];

(* ----- Linear scale variant ----- *)

p2 = RadiaCodeTools`SpectroPlot`RCSpectroPlot[
       FileNameJoin[{dataDir, "K40.rcspg"}], "Scale" -> "Linear"];
vt[MatchQ[p2, _Graphics | _Legended], True, TestID -> "spectroplot-linear"];

(* ----- Sample range filter ----- *)

p3 = RadiaCodeTools`SpectroPlot`RCSpectroPlot[
       FileNameJoin[{dataDir, "K40.rcspg"}], "SampleRange" -> {1, 100}];
vt[MatchQ[p3, _Graphics | _Legended], True, TestID -> "spectroplot-samplerange"];

(* ----- Channel range filter ----- *)

p4 = RadiaCodeTools`SpectroPlot`RCSpectroPlot[
       FileNameJoin[{dataDir, "K40.rcspg"}], "ChannelRange" -> {200, 600}];
vt[MatchQ[p4, _Graphics | _Legended], True, TestID -> "spectroplot-channelrange"];

(* ----- Accept Association directly ----- *)

assoc = RadiaCodeTools`Formats`ImportRCSpectrogram[
          FileNameJoin[{dataDir, "K40.rcspg"}]];
p5 = RadiaCodeTools`SpectroPlot`RCSpectroPlot[assoc];
vt[MatchQ[p5, _Graphics | _Legended], True, TestID -> "spectroplot-from-assoc"];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["SpectroPlot.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
