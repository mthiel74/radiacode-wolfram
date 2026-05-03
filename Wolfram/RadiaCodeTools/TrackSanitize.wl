(* ::Package:: *)

(* RadiaCodeTools`TrackSanitize`
   Coordinate / time / serial rebasing for .rctrk privacy.
   Mirrors track_sanitize.py — defaults teleport to "Hunt for Red October"
   coordinates and 1984. *)

BeginPackage["RadiaCodeTools`TrackSanitize`", {"RadiaCodeTools`Formats`"}];

SanitizeTrack::usage =
  "SanitizeTrack[track] returns a privacy-rebased copy of an .rctrk \
Association.  Coords are shifted so their minimum equals BaseLatitude / \
BaseLongitude; times are shifted so the first point equals StartTime; \
serial number, comment, and track name get scrubbed.  Pass keep flags \
to preserve specific fields.  Options:\n\
  \"BaseLatitude\"   -> 43.5833323\n\
  \"BaseLongitude\"  -> -55.9269664\n\
  \"StartTime\"      -> DateObject[{1984,12,5,0,0,0}, TimeZone -> \"UTC\"]\n\
  \"SerialNumber\"   -> \"RC-100-314159\"\n\
  \"Comment\"        -> \"And I ... was never here.\"\n\
  \"NamePrefix\"     -> \"sanitized_\"\n\
  \"ReverseRoute\"   -> False\n\
  \"KeepComment\"    -> False\n\
  \"KeepName\"       -> False\n\
  \"KeepPosition\"   -> False\n\
  \"KeepSerial\"     -> False\n\
  \"KeepTime\"       -> False";

SanitizeTrackFile::usage =
  "SanitizeTrackFile[infile, outfile] reads, sanitizes, and writes a \
.rctrk file.  Same options as SanitizeTrack, plus \"Overwrite\" -> False.";

Begin["`Private`"];

Options[SanitizeTrack] = {
  "BaseLatitude"  -> 43.5833323,
  "BaseLongitude" -> -55.9269664,
  "StartTime"     -> Automatic,    (* resolved to 1984-12-05 below *)
  "SerialNumber"  -> "RC-100-314159",
  "Comment"       -> "And I ... was never here.",
  "NamePrefix"    -> "sanitized_",
  "ReverseRoute"  -> False,
  "KeepComment"   -> False,
  "KeepName"      -> False,
  "KeepPosition"  -> False,
  "KeepSerial"    -> False,
  "KeepTime"      -> False
};

defaultStartTime[] := DateObject[{1984, 12, 5, 0, 0, 0}, TimeZone -> "UTC"];

SanitizeTrack[track_Association, OptionsPattern[]] :=
  Module[{points, header, lats, lons, latMin, lonMin,
          startTime, baseLat, baseLon, headerText, hash, newName,
          deltaT, oldT0, newPoints, kc, kn, kp, ks, kt, ttz},
    points = Normal[track["Points"]];
    header = track["Header"];
    If[!ListQ[points] || Length[points] === 0, Return[track]];

    baseLat = OptionValue["BaseLatitude"];
    baseLon = OptionValue["BaseLongitude"];
    startTime = OptionValue["StartTime"];
    If[startTime === Automatic, startTime = defaultStartTime[]];
    kc = OptionValue["KeepComment"];
    kn = OptionValue["KeepName"];
    kp = OptionValue["KeepPosition"];
    ks = OptionValue["KeepSerial"];
    kt = OptionValue["KeepTime"];

    lats = Lookup[#, "Latitude"] & /@ points;
    lons = Lookup[#, "Longitude"] & /@ points;
    latMin = Min[lats];
    lonMin = Min[lons];

    If[kp,
      baseLat = latMin;
      baseLon = lonMin
    ];

    oldT0 = Lookup[First[points], "Time"];
    If[kt, startTime = oldT0];
    deltaT = AbsoluteTime[startTime] - AbsoluteTime[oldT0];

    headerText = StringJoin[
      Lookup[header, "Name", ""], " ",
      Lookup[header, "SerialNumber", ""], " ",
      Lookup[header, "Comment", ""], " ",
      DateString[oldT0],
      StringJoin[ToString[#] & /@ points]];
    hash = Hash[headerText, "SHA256", "HexString"];
    newName = If[kn, Lookup[header, "Name", ""],
                 OptionValue["NamePrefix"] <> StringTake[hash, 32]];

    newPoints = Map[
      Function[p,
        Module[{newTime, newLat, newLon},
          newTime = If[kt, p["Time"], DatePlus[p["Time"], {deltaT, "Second"}]];
          newLat = Round[p["Latitude"] - latMin + baseLat, 10^-7];
          newLon = Round[p["Longitude"] - lonMin + baseLon, 10^-7];
          ttz = <|p,
            "Time" -> newTime,
            "FileTime" -> RadiaCodeTools`Formats`dateToFileTime[newTime],
            "Latitude" -> newLat,
            "Longitude" -> newLon|>;
          ttz]],
      points];

    If[OptionValue["ReverseRoute"],
      Module[{origTimes = Lookup[#, "Time"] & /@ newPoints},
        newPoints = Reverse[newPoints];
        newPoints = MapThread[<|#1,
                                "Time" -> #2,
                                "FileTime" -> RadiaCodeTools`Formats`dateToFileTime[#2]|> &,
                              {newPoints, origTimes}]]];

    <|"Header" -> <|header,
        "Name" -> newName,
        "SerialNumber" -> If[ks, Lookup[header, "SerialNumber", ""],
                              OptionValue["SerialNumber"]],
        "Comment" -> If[kc, Lookup[header, "Comment", ""],
                          OptionValue["Comment"]]|>,
      "Points" -> Dataset[newPoints]|>
  ];

Options[SanitizeTrackFile] = Append[Options[SanitizeTrack], "Overwrite" -> False];

SanitizeTrackFile[infile_String, outfile_String, opts:OptionsPattern[]] :=
  Module[{track, sanitized},
    If[FileExistsQ[outfile] && !OptionValue["Overwrite"],
       Return[Failure["Exists", <|"MessageTemplate" -> "Output exists; pass \"Overwrite\" -> True."|>]]];
    track = RadiaCodeTools`Formats`ImportRCTrack[infile];
    If[FailureQ[track] || track === $Failed, Return[track]];
    sanitized = SanitizeTrack[track,
      FilterRules[{opts}, Options[SanitizeTrack]]];
    RadiaCodeTools`Formats`ExportRCTrack[outfile,
      sanitized["Header"], sanitized["Points"]]
  ];

End[];
EndPackage[];
