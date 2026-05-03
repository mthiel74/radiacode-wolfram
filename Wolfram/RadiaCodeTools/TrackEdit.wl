(* ::Package:: *)

(* RadiaCodeTools`TrackEdit`
   Geo / time filtering of .rctrk tracks (port of track_edit.py).

   Filter rules:
     - "Include*" rules act as AND across rules of different kinds and
       OR within a list of one kind: a point survives only if at least
       one element of EACH non-empty include kind matches it.
     - "Exclude*" rules drop a point if ANY of the elements of ANY
       exclude kind matches it.
     - With no rules at all, the track is returned unchanged. *)

BeginPackage["RadiaCodeTools`TrackEdit`", {"RadiaCodeTools`Formats`"}];

EditTrack::usage =
  "EditTrack[track] applies geo/time include/exclude filters to a \
.rctrk Association.  Options:\n\
  \"IncludeTimeRanges\"  -> {{startDate, endDate}, ...}\n\
  \"ExcludeTimeRanges\"  -> same shape\n\
  \"IncludeBoundingBoxes\" -> {{{lat1, lon1}, {lat2, lon2}}, ...}\n\
  \"ExcludeBoundingBoxes\" -> same shape\n\
  \"IncludeRadii\"        -> {{{lat, lon}, radiusMetres}, ...}\n\
  \"ExcludeRadii\"        -> same shape\n\
  \"TrackName\"           -> Automatic | string";

EditTrackFile::usage =
  "EditTrackFile[infile, outfile] reads, edits, and writes a .rctrk file.";

Begin["`Private`"];

inTimeRange[t_, {ts_, te_}] :=
  AbsoluteTime[ts] <= AbsoluteTime[t] < AbsoluteTime[te];

inBox[lat_, lon_, {{lat1_, lon1_}, {lat2_, lon2_}}] :=
  Min[lat1, lat2] <= lat <= Max[lat1, lat2] &&
  Min[lon1, lon2] <= lon <= Max[lon1, lon2];

inRadius[lat_, lon_, {{cLat_, cLon_}, r_}] :=
  QuantityMagnitude @ GeoDistance[{lat, lon}, {cLat, cLon},
    UnitSystem -> "Metric"] <= r;

pointMatches[point_, ranges_, predFn_] :=
  AnyTrue[ranges, predFn[point, #] &];

Options[EditTrack] = {
  "IncludeTimeRanges"    -> {},
  "ExcludeTimeRanges"    -> {},
  "IncludeBoundingBoxes" -> {},
  "ExcludeBoundingBoxes" -> {},
  "IncludeRadii"         -> {},
  "ExcludeRadii"         -> {},
  "TrackName"            -> Automatic
};

EditTrack[track_Association, OptionsPattern[]] :=
  Module[{points, header, itr, etr, ibb, ebb, irr, err, name,
          timeFn, boxFn, radFn, keep, kept},
    points = Normal[track["Points"]];
    header = track["Header"];
    itr = OptionValue["IncludeTimeRanges"];
    etr = OptionValue["ExcludeTimeRanges"];
    ibb = OptionValue["IncludeBoundingBoxes"];
    ebb = OptionValue["ExcludeBoundingBoxes"];
    irr = OptionValue["IncludeRadii"];
    err = OptionValue["ExcludeRadii"];
    name = OptionValue["TrackName"];

    timeFn[p_, r_] := inTimeRange[p["Time"], r];
    boxFn[p_, r_]  := inBox[p["Latitude"], p["Longitude"], r];
    radFn[p_, r_]  := inRadius[p["Latitude"], p["Longitude"], r];

    keep[p_] := And[
      Length[itr] === 0 || AnyTrue[itr, timeFn[p, #] &],
      Length[ibb] === 0 || AnyTrue[ibb, boxFn[p, #] &],
      Length[irr] === 0 || AnyTrue[irr, radFn[p, #] &],
      Length[etr] === 0 || NoneTrue[etr, timeFn[p, #] &],
      Length[ebb] === 0 || NoneTrue[ebb, boxFn[p, #] &],
      Length[err] === 0 || NoneTrue[err, radFn[p, #] &]
    ];
    kept = Select[points, keep];

    <|
      "Header" -> <|header,
        "Name" -> If[name === Automatic,
                      Lookup[header, "Name", ""] <> " (edit)",
                      name]|>,
      "Points" -> Dataset[kept]
    |>
  ];

Options[EditTrackFile] = Append[Options[EditTrack], "Overwrite" -> False];

EditTrackFile[infile_String, outfile_String, opts:OptionsPattern[]] :=
  Module[{track, edited},
    If[FileExistsQ[outfile] && !OptionValue["Overwrite"],
       Return[Failure["Exists",
         <|"MessageTemplate" -> "Output exists; pass \"Overwrite\" -> True."|>]]];
    track = RadiaCodeTools`Formats`ImportRCTrack[infile];
    If[FailureQ[track] || track === $Failed, Return[track]];
    edited = EditTrack[track, FilterRules[{opts}, Options[EditTrack]]];
    RadiaCodeTools`Formats`ExportRCTrack[outfile,
      edited["Header"], edited["Points"]]
  ];

End[];
EndPackage[];
