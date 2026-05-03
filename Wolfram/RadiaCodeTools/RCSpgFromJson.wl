(* ::Package:: *)

(* RadiaCodeTools`RCSpgFromJson`
   Convert an rcmultispg ndjson log to a .rcspg spectrogram.
   Mirrors rcspg_from_json.py. *)

BeginPackage["RadiaCodeTools`RCSpgFromJson`",
  {"RadiaCodeTools`Formats`"}];

ConvertNDJsonToRcspg::usage =
  "ConvertNDJsonToRcspg[infile, outfile] reads an ndjson file produced \
by rcmultispg and writes a .rcspg spectrogram.  Options:\n\
  \"SerialNumber\" -> Automatic | filter to records of this device\n\
  \"Name\"         -> Automatic | display name\n\
  \"Comment\"      -> \"\"";

Begin["`Private`"];

Options[ConvertNDJsonToRcspg] = {
  "SerialNumber" -> Automatic,
  "Name"         -> Automatic,
  "Comment"      -> ""
};

ConvertNDJsonToRcspg[infile_String, outfile_String, OptionsPattern[]] :=
  Module[{log, specRecs, sn, snFilter, name, comment, first, prev,
          samples, deltaCounts, dt, prevDt, calibration, histCounts,
          histDur, ts, accSec, points},
    log = RadiaCodeTools`Formats`ImportNDJsonLog[infile];
    specRecs = log["Spectrum"];
    snFilter = OptionValue["SerialNumber"];
    If[snFilter =!= Automatic,
      specRecs = Select[specRecs,
        Lookup[#, "serial_number", ""] === snFilter &]];
    If[Length[specRecs] < 2,
       Return[Failure["NotEnoughSpectra",
         <|"MessageTemplate" -> "Need at least 2 spectrum records; got `1`.",
           "MessageParameters" -> {Length[specRecs]}|>]]];

    first = First[specRecs];
    sn = Lookup[first, "serial_number", ""];
    calibration = Lookup[first, "calibration", {0., 1., 0.}];
    histCounts = Lookup[first, "counts", {}];
    histDur = Lookup[first, "duration", 0];
    ts = FromUnixTime[Lookup[first, "timestamp", 0], TimeZone -> "UTC"];
    name = OptionValue["Name"];
    If[name === Automatic, name = "Spectrogram " <> DateString[ts]];
    comment = OptionValue["Comment"];

    prev = first;
    points = Function[r,
      Module[{currTs, currCounts, currDur, dts, td, deltas},
        currTs    = FromUnixTime[Lookup[r, "timestamp", 0], TimeZone -> "UTC"];
        currCounts = Lookup[r, "counts", {}];
        currDur   = Lookup[r, "duration", 0];
        td = AbsoluteTime[currTs] - AbsoluteTime[
               FromUnixTime[Lookup[prev, "timestamp", 0], TimeZone -> "UTC"]];
        deltas = currCounts - Lookup[prev, "counts", {}];
        prev = r;
        <|"Time" -> currTs, "Duration" -> td, "Deltas" -> deltas|>
      ]] /@ Rest[specRecs];

    accSec = If[Length[points] > 0,
      AbsoluteTime[Last[points]["Time"]] -
      AbsoluteTime[First[points]["Time"]],
      0];

    RadiaCodeTools`Formats`ExportRCSpectrogram[outfile, <|
      "Name" -> name,
      "SerialNumber" -> sn,
      "Comment" -> comment,
      "Flags" -> "0",
      "Calibration" -> calibration,
      "HistoricalCounts" -> histCounts,
      "HistoricalDuration" -> histDur,
      "Timestamp" -> ts,
      "AccumulationTime" -> Quantity[accSec, "Seconds"],
      "Samples" -> points
    |>]
  ];

End[];
EndPackage[];
