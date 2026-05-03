(* ::Package:: *)

(* RadiaCodeTools`RCTrkFromJson`
   Convert an rcmultispg ndjson log to a .rctrk track.
   Mirrors rctrk_from_json.py: stitches GPS records (lat/lon/time) with
   adjacent realtime records (dose/count rate) keyed by GPS timestamp. *)

BeginPackage["RadiaCodeTools`RCTrkFromJson`",
  {"RadiaCodeTools`Formats`"}];

ConvertNDJsonToRctrk::usage =
  "ConvertNDJsonToRctrk[infile, outfile] reads an ndjson log and \
writes a .rctrk track.  Options:\n\
  \"SerialNumber\" -> Automatic | filter to records of this device\n\
  \"Name\"         -> Automatic | display name\n\
  \"Comment\"      -> \"\"";

Begin["`Private`"];

Options[ConvertNDJsonToRctrk] = {
  "SerialNumber" -> Automatic,
  "Name"         -> Automatic,
  "Comment"      -> ""
};

parseGpsTime[s_String] :=
  Module[{stripped},
    stripped = StringReplace[s, ".000Z" -> "Z"];
    DateObject[stripped, TimeZone -> "UTC"]
  ];

ConvertNDJsonToRctrk[infile_String, outfile_String, OptionsPattern[]] :=
  Module[{lines, recs, sn, snFilter, name, comment, db, currKey, points,
          required, header, ts},
    lines = Select[ReadList[infile, String], StringTrim[#] =!= "" &];
    recs = Map[ImportString[#, "RawJSON"] &, lines];
    snFilter = OptionValue["SerialNumber"];
    sn = If[snFilter === Automatic,
            Module[{firstWith},
              firstWith = SelectFirst[recs, KeyExistsQ[#, "serial_number"] &, <||>];
              Lookup[firstWith, "serial_number", ""]],
            snFilter];

    db = <||>;
    currKey = None;
    Scan[
      Function[r,
        If[KeyExistsQ[r, "calibration"], Return[]];   (* skip spectrum *)
        If[sn =!= "" && KeyExistsQ[r, "serial_number"] &&
           Lookup[r, "serial_number", ""] =!= sn, Return[]];
        Which[
          KeyExistsQ[r, "gnss"],
            If[!TrueQ[Lookup[r, "gnss", False]], Return[]];
            ts = parseGpsTime[Lookup[r, "time", ""]];
            currKey = DateString[ts, "ISODateTime"];
            db[currKey] = <|r, "_dt" -> ts|>,
          currKey =!= None,
            db[currKey] = <|db[currKey], r|>
        ]
      ],
      recs];

    required = {"_dt", "lat", "lon", "dose_rate", "epc", "count_rate"};
    points = Function[entry,
      If[ContainsAll[Keys[entry], required],
        <|"Time" -> entry["_dt"],
          "FileTime" -> RadiaCodeTools`Formats`dateToFileTime[entry["_dt"]],
          "Latitude" -> entry["lat"],
          "Longitude" -> entry["lon"],
          "Accuracy" -> entry["epc"],
          "DoseRate" -> entry["dose_rate"],
          "CountRate" -> entry["count_rate"],
          "Comment" -> " "|>,
        Nothing]
      ] /@ SortBy[Values[db], Lookup[#, "_dt", Now] &];

    name = OptionValue["Name"];
    If[name === Automatic && Length[points] > 0,
      name = "Track " <> DateString[First[points]["Time"]]];
    If[name === Automatic, name = "Track"];

    header = <|
      "Name" -> name,
      "SerialNumber" -> sn,
      "Comment" -> OptionValue["Comment"],
      "Flags" -> "EC"
    |>;

    RadiaCodeTools`Formats`ExportRCTrack[outfile, header, points]
  ];

End[];
EndPackage[];
