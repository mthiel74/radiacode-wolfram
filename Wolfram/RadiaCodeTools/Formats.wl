(* ::Package:: *)

(* RadiaCodeTools`Formats`
   File I/O for RadiaCode native and N42 formats, plus the multi-device
   ndjson log and the calibration JSON used by Calibrate.wl.
*)

BeginPackage["RadiaCodeTools`Formats`"];

ImportRCSpectrum::usage =
  "ImportRCSpectrum[file] parses a RadiaCode XML spectrum file and \
returns an Association with keys Device, SerialNumber, SpectrumName, \
StartTime, EndTime, Duration, NumberOfChannels, Calibration, Counts, \
and Background (Association or None).";

ImportRCTrack::usage =
  "ImportRCTrack[file] parses a RadiaCode .rctrk file. Returns an \
Association <|\"Header\" -> <|...|>, \"Points\" -> Dataset[...]|>.";

ExportRCTrack::usage =
  "ExportRCTrack[file, header, points] writes a .rctrk file. header is \
an Association with keys Name, SerialNumber, Comment, Flags. points \
is a Dataset or list of Associations with keys Time (DateObject), \
Latitude, Longitude, Accuracy, DoseRate, CountRate, and optionally \
Comment.";

ImportRCSpectrogram::usage =
  "ImportRCSpectrogram[file] parses a RadiaCode .rcspg file. Returns \
an Association with header fields, the historical spectrum, the \
calibration coefficients, and a Samples Dataset (one row per \
timestamp with cumulative channel counts).";

ExportRCSpectrogram::usage =
  "ExportRCSpectrogram[file, spec] writes a .rcspg file from an \
Association with keys: SerialNumber, Name, Comment, Flags (string), \
Calibration ({a0, a1, a2}), HistoricalCounts (cumulative counts at \
recording start), HistoricalDuration (seconds), Timestamp (DateObject), \
AccumulationTime (Quantity seconds), and Samples (list of \
<|\"Time\" -> DateObject, \"Duration\" -> seconds, \"Deltas\" -> {...}|>).";

ImportNDJsonLog::usage =
  "ImportNDJsonLog[file] parses a newline-delimited JSON log produced \
by rcmultispg. Returns an Association with keys Spectrum, Realtime, \
GPS (each a list of records).";

ImportCalibrationJSON::usage =
  "ImportCalibrationJSON[file] parses a calibrate.py JSON file and \
returns a flat list of {channel, energy} pairs across all sources.";

ImportN42::usage =
  "ImportN42[file] parses an ANSI N42 file (as produced by ExportN42 \
or by the upstream Python tool) and returns an Association mirroring \
ImportRCSpectrum.";

ExportN42::usage =
  "ExportN42[file, spec] writes an ANSI N42 XML file. spec is an \
Association with the same keys as returned by ImportRCSpectrum, \
plus optional UUID.";

fileTimeToDate::usage =
  "fileTimeToDate[ft] converts a Windows FILETIME integer (100-ns \
intervals since 1601-01-01 UTC) to a UTC DateObject.";

dateToFileTime::usage =
  "dateToFileTime[date] converts a DateObject to Windows FILETIME.";

applyCalibration::usage =
  "applyCalibration[{a0, a1, ...}, channels] returns the polynomial \
energy values for the given channel indices.";

Begin["`Private`"];

(* ===== Time helpers ===== *)

(* FILETIME at the Unix epoch, in 100-ns ticks since 1601-01-01 UTC. *)
$filetimeUnixOffset = 116444736000000000;

fileTimeToDate[ft_?NumericQ] :=
  FromUnixTime[(ft - $filetimeUnixOffset)/10^7, TimeZone -> "UTC"];

dateToFileTime[d_DateObject] :=
  Round[UnixTime[d] * 10^7 + $filetimeUnixOffset];

(* Parse a numeric string, tolerating "E"/"e" exponent notation that
   Wolfram's ToExpression would otherwise read as Euler's constant. *)
parseNumber[s_String] :=
  Module[{v},
    v = ToExpression[StringReplace[StringTrim[s], {"E" | "e" -> "*^"}]];
    If[NumericQ[v], v, $Failed]
  ];
parseNumber[s_] := s;

parseNumberList[s_String] :=
  parseNumber /@ Select[StringSplit[StringTrim[s]], # =!= "" &];

(* RadiaCode timestamps in XML look like "2023-06-07T05:52:00" with no
   timezone. Treat as UTC. *)
parseRCDateTime[s_String] :=
  Module[{parts, ymd, hms},
    parts = StringSplit[s, "T"];
    If[Length[parts] =!= 2, Return[$Failed]];
    ymd = ToExpression /@ StringSplit[parts[[1]], "-"];
    hms = ToExpression /@ StringSplit[parts[[2]], ":"];
    DateObject[Join[ymd, hms], TimeZone -> "UTC"]
  ];

formatRCDateTime[d_DateObject] :=
  DateString[d, {"Year", "-", "Month", "-", "Day", "T",
                  "Hour", ":", "Minute", ":", "Second"}];

(* ===== Calibration helpers ===== *)

(* Horner-form polynomial evaluation; avoids 0^0 ambiguity. *)
applyCalibration[coeffs_List, channel_?NumericQ] :=
  Fold[#1 * channel + #2 &, 0, Reverse[coeffs]];
applyCalibration[coeffs_List, channels_List] :=
  applyCalibration[coeffs, #] & /@ channels;

(* ===== XML helpers (SymbolicXML walking) ===== *)

(* Find first descendant XMLElement with the given tag name. *)
findElement[xml_, tag_String] :=
  FirstCase[xml, XMLElement[tag, _, _], Missing["NotFound", tag], Infinity];

(* Find all descendants with the given tag. *)
findAllElements[xml_, tag_String] :=
  Cases[xml, XMLElement[tag, _, _], Infinity];

(* Direct text content of an XMLElement (concatenated string children). *)
elementText[XMLElement[_, _, children_]] :=
  StringJoin @ Cases[children, _String];
elementText[other_] := "";

elementTextOrMissing[xml_, tag_String] :=
  Module[{el = findElement[xml, tag]},
    If[Head[el] === XMLElement, elementText[el], Missing["NotFound", tag]]
  ];

(* ===== RC XML spectrum parser ===== *)

parseEnergySpectrum[block_XMLElement] :=
  Module[{calEl, coeffEls, coeffs, dataPts, counts, mt, name, sn,
          numChans, channelPitch},
    calEl = findElement[block, "EnergyCalibration"];
    coeffEls = If[Head[calEl] === XMLElement,
                  findAllElements[calEl, "Coefficient"],
                  {}];
    coeffs = parseNumber /@ (elementText /@ coeffEls);
    dataPts = findAllElements[block, "DataPoint"];
    counts = parseNumber /@ (elementText /@ dataPts);
    mt = parseNumber @ elementTextOrMissing[block, "MeasurementTime"];
    name = elementTextOrMissing[block, "SpectrumName"];
    sn = elementTextOrMissing[block, "SerialNumber"];
    numChans = parseNumber @ elementTextOrMissing[block, "NumberOfChannels"];
    channelPitch = parseNumber @ elementTextOrMissing[block, "ChannelPitch"];
    <|
      "SpectrumName" -> name,
      "SerialNumber" -> sn,
      "NumberOfChannels" -> numChans,
      "ChannelPitch" -> channelPitch,
      "MeasurementTime" -> mt,
      "Duration" -> If[NumericQ[mt], Quantity[mt, "Seconds"], Missing[]],
      "Calibration" -> coeffs,
      "Counts" -> counts
    |>
  ];

ImportRCSpectrum[file_String] :=
  Module[{xml, root, rd, fg, bg, device, start, end, fgData, bgData},
    xml = Import[file, "XML"];
    If[xml === $Failed, Return[$Failed]];
    root = FirstCase[xml, XMLElement["ResultDataFile", _, _], $Failed, Infinity];
    If[root === $Failed,
      Return[Failure["BadXML",
        <|"MessageTemplate" -> "No ResultDataFile root in `1`",
          "MessageParameters" -> {file}|>]]];
    rd = findElement[root, "ResultData"];
    device = elementTextOrMissing[
      findElement[root, "DeviceConfigReference"], "Name"];
    start = elementTextOrMissing[rd, "StartTime"];
    end = elementTextOrMissing[rd, "EndTime"];
    fg = findElement[rd, "EnergySpectrum"];
    bg = findElement[rd, "BackgroundEnergySpectrum"];
    fgData = If[Head[fg] === XMLElement, parseEnergySpectrum[fg], Missing[]];
    bgData = If[Head[bg] === XMLElement, parseEnergySpectrum[bg], None];
    <|
      "Device" -> device,
      "StartTime" -> If[StringQ[start], parseRCDateTime[start], Missing[]],
      "EndTime"   -> If[StringQ[end],   parseRCDateTime[end],   Missing[]],
      "SpectrumName" -> fgData["SpectrumName"],
      "SerialNumber" -> fgData["SerialNumber"],
      "NumberOfChannels" -> fgData["NumberOfChannels"],
      "ChannelPitch" -> fgData["ChannelPitch"],
      "Duration" -> fgData["Duration"],
      "Calibration" -> fgData["Calibration"],
      "Counts" -> fgData["Counts"],
      "Background" -> bgData
    |>
  ];

(* ===== .rctrk parser ===== *)

ImportRCTrack[file_String] :=
  Module[{lines, headerLine, headerParts, header, columnsLine,
          dataLines, rows, rowAssoc, points},
    lines = ReadList[file, String];
    If[Length[lines] < 2, Return[$Failed]];
    headerLine = First[lines];
    If[!StringStartsQ[headerLine, "Track:"],
      Return[Failure["BadFormat",
        <|"MessageTemplate" -> "Not an .rctrk file: `1`",
          "MessageParameters" -> {file}|>]]];
    headerParts = StringSplit[
      StringTrim @ StringDelete[headerLine, StartOfString ~~ "Track:"],
      "\t"];
    header = <|
      "Name" -> If[Length[headerParts] >= 1, headerParts[[1]], ""],
      "SerialNumber" -> If[Length[headerParts] >= 2, headerParts[[2]], ""],
      "Comment" -> If[Length[headerParts] >= 3, headerParts[[3]], ""],
      "Flags" -> If[Length[headerParts] >= 4, headerParts[[4]], ""]
    |>;
    columnsLine = lines[[2]];  (* descriptive — we use fixed schema *)
    dataLines = Drop[lines, 2];
    rowAssoc[line_String] :=
      Module[{f = StringSplit[line, "\t"], ft},
        If[Length[f] < 7, Return[Nothing]];
        ft = parseNumber[f[[1]]];
        <|
          "FileTime" -> ft,
          "Time" -> fileTimeToDate[ft],
          "Latitude"  -> parseNumber[f[[3]]],
          "Longitude" -> parseNumber[f[[4]]],
          "Accuracy"  -> parseNumber[f[[5]]],
          "DoseRate"  -> parseNumber[f[[6]]],
          "CountRate" -> parseNumber[f[[7]]],
          "Comment"   -> If[Length[f] >= 8, StringTrim[f[[8]]], ""]
        |>
      ];
    points = rowAssoc /@ dataLines;
    points = DeleteCases[points, Nothing];
    <|"Header" -> header, "ColumnsLine" -> columnsLine,
      "Points" -> Dataset[points]|>
  ];

ExportRCTrack[file_String, header_Association, points_] :=
  Module[{rows, headerLine, columnsLine, body, point2Row},
    rows = If[Head[points] === Dataset, Normal[points], points];
    headerLine = "Track:\t" <> StringRiffle[
      {Lookup[header, "Name", ""],
       Lookup[header, "SerialNumber", ""],
       Lookup[header, "Comment", ""],
       Lookup[header, "Flags", "EC"]}, "\t"];
    columnsLine = "Timestamp\tTime\tLatitude\tLongitude\tAccuracy\tDoseRate\tCountRate\tComment";
    point2Row[p_Association] :=
      Module[{ft, dt, fmt},
        ft = If[KeyExistsQ[p, "FileTime"], p["FileTime"],
                dateToFileTime[p["Time"]]];
        dt = DateString[p["Time"], {"Year", "-", "Month", "-", "Day", " ",
                                     "Hour", ":", "Minute", ":", "Second"}];
        (* CForm gives single-line, parseable scientific notation
           ("7.4e-6") for tiny values; ToString[..] wraps small floats
           across multiple lines. *)
        fmt[x_] := ToString[N[x], CForm];
        StringRiffle[
          {ToString[ft],
           dt,
           fmt[p["Latitude"]],
           fmt[p["Longitude"]],
           fmt[p["Accuracy"]],
           fmt[p["DoseRate"]],
           fmt[p["CountRate"]],
           Lookup[p, "Comment", " "]},
          "\t"]
      ];
    body = StringRiffle[point2Row /@ rows, "\n"];
    Export[file, headerLine <> "\n" <> columnsLine <> "\n" <> body, "Text"]
  ];

(* ===== .rcspg parser ===== *)

(* parse "Spectrogram: ..." key:value tab-separated header line *)
parseRcspgHeader[line_String] :=
  Module[{trimmed, fields, kv},
    trimmed = StringTrim[line];
    fields = StringSplit[trimmed, "\t"];
    kv = Association @@ (
      Function[f,
        With[{p = StringSplit[f, ":", 2]},
          If[Length[p] === 2,
            StringTrim[p[[1]]] -> StringTrim[p[[2]]],
            Nothing]]] /@ fields);
    kv
  ];

(* Decode the "Spectrum: HH HH HH ..." historical spectrum line.
   Layout: <I3f{N}I> little-endian (uint32 duration, 3*float32 cal,
   N*uint32 counts).  Returns <|"Duration" -> ..., "Calibration" -> ...,
   "Counts" -> ...|>. *)
hexPairToByte[chars_List] := FromDigits[StringJoin[chars], 16];

decodeHistoricalSpectrum[line_String] :=
  Module[{hex, bytes, fileName, tmpStrm, strm, dur, cal, counts, c, sown},
    hex = StringDelete[
      StringTrim @ StringDrop[StringTrim[line], StringLength["Spectrum:"]],
      Whitespace];
    bytes = hexPairToByte /@ Partition[Characters[hex], 2];
    fileName = CreateFile[];
    tmpStrm = OpenWrite[fileName, BinaryFormat -> True];
    BinaryWrite[tmpStrm, bytes, "Byte"];
    Close[tmpStrm];
    strm = OpenRead[fileName, BinaryFormat -> True];
    dur = BinaryRead[strm, "UnsignedInteger32"];
    cal = Table[BinaryRead[strm, "Real32"], 3];
    counts = {};
    While[(c = BinaryRead[strm, "UnsignedInteger32"]) =!= EndOfFile,
      AppendTo[counts, c]];
    Close[strm];
    DeleteFile[fileName];
    <|"Duration" -> dur, "Calibration" -> cal, "Counts" -> counts|>
  ];

ImportRCSpectrogram[file_String] :=
  Module[{lines, headerLine, header, histLine, hist, dataLines,
          parseRow, samples, ns, channels, deltaMatrix, cumulative},
    lines = ReadList[file, String];
    If[Length[lines] < 2, Return[$Failed]];
    headerLine = First[lines];
    If[!StringStartsQ[headerLine, "Spectrogram:"],
      Return[Failure["BadFormat",
        <|"MessageTemplate" -> "Not an .rcspg file: `1`",
          "MessageParameters" -> {file}|>]]];
    header = parseRcspgHeader[headerLine];
    histLine = lines[[2]];
    hist = decodeHistoricalSpectrum[histLine];
    dataLines = Drop[lines, 2];
    parseRow[row_String] :=
      Module[{ints = parseNumberList[row]},
        If[Length[ints] < 2, Nothing,
          <|"FileTime" -> ints[[1]],
            "Time"     -> fileTimeToDate[ints[[1]]],
            "Duration" -> ints[[2]],
            "Deltas"   -> Drop[ints, 2]|>]
      ];
    samples = parseRow /@ dataLines;
    samples = DeleteCases[samples, Nothing];
    ns = Length[samples];
    channels = parseNumber @ Lookup[header, "Channels", "1024"];
    (* pad each delta vector to channel count, then accumulate *)
    deltaMatrix = PadRight[#["Deltas"], channels] & /@ samples;
    cumulative = If[ns > 0, Accumulate[deltaMatrix], {}];
    <|
      "Header" -> header,
      "Calibration" -> hist["Calibration"],
      "HistoricalSpectrum" -> hist,
      "NumberOfChannels" -> channels,
      "Samples" -> Dataset[samples],
      "DeltaMatrix" -> deltaMatrix,
      "CumulativeMatrix" -> cumulative
    |>
  ];

(* Build the historical Spectrum: line: hex-encoded
   <I3f{N}I> = uint32 duration, 3 float32 calibration, N uint32 counts. *)
encodeHistoricalSpectrum[duration_, cal_List, counts_List] :=
  Module[{fileName, strm, hex, bytes},
    fileName = CreateFile[];
    strm = OpenWrite[fileName, BinaryFormat -> True];
    BinaryWrite[strm, Round[duration], "UnsignedInteger32"];
    Scan[BinaryWrite[strm, N[#], "Real32"] &, PadRight[cal, 3, 0]];
    Scan[BinaryWrite[strm, Round[#], "UnsignedInteger32"] &, counts];
    Close[strm];
    bytes = BinaryReadList[fileName, "Byte"];
    DeleteFile[fileName];
    hex = StringJoin[
      Riffle[
        IntegerString[#, 16, 2] & /@ bytes,
        " "]];
    StringJoin["Spectrum: ", ToUpperCase[hex]]
  ];

ExportRCSpectrogram[file_String, spec_Association] :=
  Module[{name, sn, comment, flags, accSec, channels, ts, cal, hist,
          histDur, samples, headerLine, histLine, sampleLines, body, dt0,
          stripTrailingZeros},
    name     = Lookup[spec, "Name", "Spectrogram"];
    sn       = Lookup[spec, "SerialNumber", ""];
    comment  = Lookup[spec, "Comment", ""];
    flags    = Lookup[spec, "Flags", "0"];
    cal      = Lookup[spec, "Calibration", {0., 1., 0.}];
    histDur  = Lookup[spec, "HistoricalDuration", 0];
    hist     = Lookup[spec, "HistoricalCounts", {}];
    channels = Length[hist];
    samples  = Lookup[spec, "Samples", {}];
    (* ImportRCSpectrogram returns Samples as a Dataset; normalise so
       the per-sample Function map below works regardless of input. *)
    If[Head[samples] === Dataset, samples = Normal[samples]];
    ts       = Lookup[spec, "Timestamp",
                  If[samples =!= {}, Lookup[First[samples], "Time"],
                     Now]];
    accSec   = Lookup[spec, "AccumulationTime",
                Quantity[
                  If[samples =!= {} && Length[samples] >= 2,
                     AbsoluteTime[Lookup[Last[samples], "Time"]] -
                     AbsoluteTime[Lookup[First[samples], "Time"]],
                     0],
                  "Seconds"]];
    headerLine = StringRiffle[{
      "Spectrogram: " <> name,
      "Time: " <> DateString[ts, {"Year","-","Month","-","Day"," ",
                                   "Hour",":","Minute",":","Second"}],
      "Timestamp: " <> ToString[dateToFileTime[ts]],
      "Accumulation time: " <> ToString[Round @ QuantityMagnitude[accSec]],
      "Channels: " <> ToString[channels],
      "Device serial: " <> sn,
      "Flags: " <> ToString[flags],
      "Comment: " <> comment
    }, "\t"];
    histLine = encodeHistoricalSpectrum[histDur, cal, hist];
    stripTrailingZeros[s_String] := StringReplace[s, ("\t0")... ~~ EndOfString -> ""];
    sampleLines = Function[s,
      stripTrailingZeros @ StringRiffle[
        Prepend[ToString /@ s["Deltas"],
          ToString[Round @ s["Duration"]]] //
          Prepend[#, ToString[dateToFileTime[s["Time"]]]] &,
        "\t"]] /@ samples;
    body = StringRiffle[sampleLines, "\n"];
    Export[file,
      headerLine <> "\n" <> histLine <> "\n" <> body, "Text"]
  ];

(* ===== ndjson parser ===== *)

ImportNDJsonLog[file_String] :=
  Module[{lines, recs, classify, byKind},
    lines = Select[ReadList[file, String], StringTrim[#] =!= "" &];
    recs = Map[ImportString[#, "RawJSON"] &, lines];
    classify[r_Association] := Which[
      KeyExistsQ[r, "counts"],                      "Spectrum",
      AssociationQ[Lookup[r, "spectrum", None]],    "Spectrum",
      KeyExistsQ[r, "lat"],                         "GPS",
      KeyExistsQ[r, "count_rate"],                  "Realtime",
      True,                                         "Other"
    ];
    classify[_] := "Other";
    byKind = GroupBy[recs, classify];
    <|
      "Spectrum" -> Lookup[byKind, "Spectrum", {}],
      "Realtime" -> Lookup[byKind, "Realtime", {}],
      "GPS"      -> Lookup[byKind, "GPS", {}],
      "Other"    -> Lookup[byKind, "Other", {}]
    |>
  ];

(* ===== calibration JSON parser ===== *)

ImportCalibrationJSON[file_String] :=
  Module[{json, listVals, allPoints},
    json = Import[file, "RawJSON"];
    If[!AssociationQ[json], Return[$Failed]];
    (* Some keys (e.g. the "unobtainium" comment) hold a string, not a
       list of points.  Keep only list-valued entries. *)
    listVals = Select[Values[json], ListQ];
    allPoints = Flatten[
      Map[{Lookup[#, "channel"], Lookup[#, "energy"]} &, listVals, {2}],
      1];
    Select[allPoints, NumericQ[#[[1]]] && NumericQ[#[[2]]] &]
  ];

(* ===== N42 export / import =====

   The N42 schema we emit mirrors the Python n42convert.py output but
   formatted via Wolfram XML.  Element order follows the standard. *)

n42ChannelDataString[counts_List] :=
  StringRiffle[ToString /@ counts, " "];

ExportN42[file_String, spec_Association, OptionsPattern[{
  "UUID" -> Automatic,
  "Creator" -> "https://github.com/mthiel74/radiacode-wolfram (Wolfram port)"}]] :=
  Module[{uuid, creator, sn, model, fgCal, bgCal, fgCounts, bgCounts,
          startStr, durFg, durBg, root, hasBg},
    uuid = OptionValue["UUID"];
    If[uuid === Automatic, uuid = CreateUUID[]];
    creator = OptionValue["Creator"];
    sn = Lookup[spec, "SerialNumber", ""];
    model = Lookup[spec, "Device", "RadiaCode"];
    fgCal = Lookup[spec, "Calibration", {0., 1., 0.}];
    fgCounts = Lookup[spec, "Counts", {}];
    hasBg = AssociationQ[Lookup[spec, "Background", None]];
    bgCal = If[hasBg, Lookup[spec["Background"], "Calibration", fgCal], fgCal];
    bgCounts = If[hasBg, Lookup[spec["Background"], "Counts", {}], {}];
    startStr = If[Head[Lookup[spec, "StartTime"]] === DateObject,
                  formatRCDateTime[spec["StartTime"]],
                  Lookup[spec, "StartTime", ""]];
    durFg = QuantityMagnitude @ Lookup[spec, "Duration", Quantity[0, "Seconds"]];
    durBg = If[hasBg,
               QuantityMagnitude @ Lookup[spec["Background"], "Duration",
                                          Quantity[0, "Seconds"]], 0];

    (* Build XML as a string (Wolfram's XML export is finicky for
       namespaces; constructing the document as text gives us byte-level
       control to match the n42convert.py layout closely enough for
       semantic compatibility with InterSpec / N42 parsers). *)
    Module[{xml},
      xml = StringTemplate[
        "<?xml version=\"1.0\"?>
<RadInstrumentData xmlns=\"http://physics.nist.gov/N42/2011/N42\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://physics.nist.gov/N42/2011/N42 http://physics.nist.gov/N42/2011/n42.xsd\" n42DocUUID=\"`uuid`\">
  <RadInstrumentDataCreatorName>`creator`</RadInstrumentDataCreatorName>
  <RadInstrumentInformation id=\"rii-1\">
    <RadInstrumentManufacturerName>Radiacode</RadInstrumentManufacturerName>
    <RadInstrumentIdentifier>`sn`</RadInstrumentIdentifier>
    <RadInstrumentModelName>`model`</RadInstrumentModelName>
    <RadInstrumentClassCode>Spectroscopic Personal Radiation Detector</RadInstrumentClassCode>
  </RadInstrumentInformation>
  <RadDetectorInformation id=\"radiacode-csi-sipm\">
    <RadDetectorCategoryCode>Gamma</RadDetectorCategoryCode>
    <RadDetectorKindCode>CsI</RadDetectorKindCode>
    <RadDetectorDescription>CsI:Tl scintillator, coupled to SiPM</RadDetectorDescription>
    <RadDetectorLengthValue units=\"mm\">10</RadDetectorLengthValue>
    <RadDetectorWidthValue units=\"mm\">10</RadDetectorWidthValue>
    <RadDetectorDepthValue units=\"mm\">10</RadDetectorDepthValue>
  </RadDetectorInformation>
  <EnergyCalibration id=\"ec-fg\">
    <CoefficientValues>`fgCalStr`</CoefficientValues>
  </EnergyCalibration>
`bgCalBlock`  <RadMeasurement id=\"radmeas-fg\">
    <MeasurementClassCode>Foreground</MeasurementClassCode>
    <StartDateTime>`startStr`</StartDateTime>
    <RealTimeDuration>PT`durFg`S</RealTimeDuration>
    <Spectrum id=\"spec-fg\" energyCalibrationReference=\"ec-fg\" radDetectorInformationReference=\"radiacode-csi-sipm\">
      <ChannelData compressionCode=\"None\">`fgCountsStr`</ChannelData>
    </Spectrum>
  </RadMeasurement>
`bgMeasBlock`</RadInstrumentData>
"][<|
        "uuid" -> uuid,
        "creator" -> creator,
        "sn" -> sn,
        "model" -> model,
        "fgCalStr" -> StringRiffle[ToString[N[#], CForm] & /@ fgCal, " "],
        "bgCalBlock" -> If[hasBg,
          "  <EnergyCalibration id=\"ec-bg\">\n    <CoefficientValues>" <>
          StringRiffle[ToString[N[#], CForm] & /@ bgCal, " "] <>
          "</CoefficientValues>\n  </EnergyCalibration>\n", ""],
        "startStr" -> startStr,
        "durFg" -> ToString[durFg],
        "fgCountsStr" -> n42ChannelDataString[fgCounts],
        "bgMeasBlock" -> If[hasBg,
          "  <RadMeasurement id=\"radmeas-bg\">\n" <>
          "    <MeasurementClassCode>Background</MeasurementClassCode>\n" <>
          "    <StartDateTime>" <> startStr <> "</StartDateTime>\n" <>
          "    <RealTimeDuration>PT" <> ToString[durBg] <> "S</RealTimeDuration>\n" <>
          "    <Spectrum id=\"spec-bg\" energyCalibrationReference=\"ec-bg\" radDetectorInformationReference=\"radiacode-csi-sipm\">\n" <>
          "      <ChannelData compressionCode=\"None\">" <>
          n42ChannelDataString[bgCounts] <>
          "</ChannelData>\n    </Spectrum>\n  </RadMeasurement>\n", ""]
      |>];
      Export[file, xml, "Text"]
    ]
  ];

(* Read back an N42 file written by ExportN42 (or by upstream Python). *)
ImportN42[file_String] :=
  Module[{xml, root, sn, model, ecs, calsByRef, meas, fg, bg,
          parseMeas, fgAssoc, bgAssoc, uuid},
    xml = Import[file, "XML"];
    If[xml === $Failed, Return[$Failed]];
    root = FirstCase[xml, XMLElement[{_, "RadInstrumentData"} | "RadInstrumentData",
                                     _, _], $Failed, Infinity];
    If[root === $Failed, Return[Failure["BadXML", <||>]]];
    uuid = "n42DocUUID" /. Cases[root, _Rule, Infinity];
    sn = StringTrim @ elementTextOrMissing[root, "RadInstrumentIdentifier"];
    model = StringTrim @ elementTextOrMissing[root, "RadInstrumentModelName"];
    (* may be tag name with or without namespace prefix *)
    sn = StringTrim @ elementOrAlt[root, {"RadInstrumentIdentifier"}];
    model = StringTrim @ elementOrAlt[root, {"RadInstrumentModelName"}];
    ecs = findAllElements[root, "EnergyCalibration"];
    If[ecs === {}, ecs = Cases[root, XMLElement[{_, "EnergyCalibration"}, _, _],
                                Infinity]];
    calsByRef = Association[
      Function[ec,
        Module[{idAttr, calStr},
          idAttr = Lookup[Association @@ ec[[2]], "id", ""];
          calStr = StringTrim @ elementOrAlt[ec, {"CoefficientValues"}];
          idAttr -> parseNumberList[calStr]
        ]] /@ ecs];
    meas = findAllElements[root, "RadMeasurement"];
    If[meas === {}, meas = Cases[root, XMLElement[{_, "RadMeasurement"}, _, _],
                                  Infinity]];
    parseMeas[m_] :=
      Module[{cls, spec, calRef, ch, dur, start},
        cls = StringTrim @ elementOrAlt[m, {"MeasurementClassCode"}];
        start = StringTrim @ elementOrAlt[m, {"StartDateTime"}];
        dur = StringReplace[
          StringTrim @ elementOrAlt[m, {"RealTimeDuration"}],
          {"PT" -> "", "S" -> ""}];
        spec = FirstCase[m, XMLElement["Spectrum" | {_, "Spectrum"}, _, _],
                         $Failed, Infinity];
        calRef = If[Head[spec] === XMLElement,
          "energyCalibrationReference" /. Cases[spec, _Rule, Infinity], ""];
        ch = StringTrim @ elementOrAlt[m, {"ChannelData"}];
        <|
          "Class" -> cls,
          "StartDateTime" -> If[StringQ[start] && start =!= "",
            parseRCDateTime[start], Missing[]],
          "Duration" -> Quantity[parseNumber[dur], "Seconds"],
          "Calibration" -> Lookup[calsByRef, calRef, {}],
          "Counts" -> parseNumberList[ch]
        |>
      ];
    fg = SelectFirst[meas, StringContainsQ[
          elementOrAlt[#, {"MeasurementClassCode"}], "Foreground"] &, None];
    bg = SelectFirst[meas, StringContainsQ[
          elementOrAlt[#, {"MeasurementClassCode"}], "Background"] &, None];
    fgAssoc = If[fg =!= None, parseMeas[fg], <||>];
    bgAssoc = If[bg =!= None, parseMeas[bg], None];
    <|
      "UUID" -> uuid,
      "Device" -> model,
      "SerialNumber" -> sn,
      "StartTime" -> Lookup[fgAssoc, "StartDateTime", Missing[]],
      "EndTime" -> Missing[],
      "NumberOfChannels" -> Length[Lookup[fgAssoc, "Counts", {}]],
      "Duration" -> Lookup[fgAssoc, "Duration", Missing[]],
      "Calibration" -> Lookup[fgAssoc, "Calibration", {}],
      "Counts" -> Lookup[fgAssoc, "Counts", {}],
      "Background" -> bgAssoc
    |>
  ];

(* Helper: like elementTextOrMissing but tolerant of namespace-prefixed
   tag names (Wolfram returns {ns, tag} for namespaced elements). *)
elementOrAlt[xml_, tagAlts_List] :=
  Module[{el},
    el = FirstCase[xml,
      XMLElement[t : (Alternatives @@ tagAlts) |
                 {_, Alternatives @@ tagAlts}, _, _],
      Missing[], Infinity];
    If[Head[el] === XMLElement, elementText[el], ""]
  ];

End[];
EndPackage[];
