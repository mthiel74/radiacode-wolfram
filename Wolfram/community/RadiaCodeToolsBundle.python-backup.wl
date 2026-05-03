(* ::Package:: *)

(* RadiaCodeToolsBundle.wl â single-file distribution

   This is `Wolfram/build_bundle.wls`'s output: every package file
   under `Wolfram/RadiaCodeTools/` concatenated in load order.  It
   provides the same public API as Get["init.wl"] from a clone of
   the radiacode-tools repository, in a single attachment.

   Source:        https://github.com/mthiel74/radiacode-wolfram
   License:       MIT, Copyright (c) 2023 Chris Kuethe (upstream
                  Python toolkit + sample data) and subsequent
                  contributors to the Wolfram port (Marco Thiel
                  + Claude).
   Acknowledgement: derived from Chris Kuethe's Python radiacode-tools
                  (https://github.com/ckuethe/radiacode-tools, MIT) -
                  file format definitions, sample data, deadtime
                  semantics, N42 conversion conventions.

   What you can do offline with just this file:
     - Parse RadiaCode XML spectra, .rctrk tracks, .rcspg spectrograms,
       N42 files, and rcmultispg ndjson logs
     - Fit channelâenergy calibration polynomials
     - Plot spectra (ListLogPlot styled), tracks (GeoListPlot),
       spectrograms (ArrayPlot)
     - Identify isotopes by matching photopeaks against a built-in
       gamma-line library
     - Convert between formats (RC XML â N42, ndjson â .rcspg/.rctrk)

   What needs additional setup:
     - Live device acquisition via Device.wl: requires Python 3 +
       `pip install radiacode` plus a clone of the repo (for the
       upstream rcmultispg.py / radiacode_poll.py scripts)
     - Live device acquisition via DeviceNative.wl: requires libusb
       (`brew install libusb` on macOS) and the compiled
       radiacode_link.dylib from `Wolfram/RadiaCodeTools/clib/`
*)


(* ======================================================================
   Formats.wl
   ====================================================================== *)

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
(* ======================================================================
   Calibrate.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`Calibrate`
   Polynomial channel→energy calibration fitting (port of calibrate.py).
*)

BeginPackage["RadiaCodeTools`Calibrate`", {"RadiaCodeTools`Formats`"}];

FitCalibration::usage =
  "FitCalibration[points] fits a polynomial energy(channel) model to a \
list of {channel, energy} pairs. Returns an Association with keys \
Coefficients (least-significant first), RSquared, Range, NumberOfPoints, \
Order, ZeroStart, Precision, and Model (a FittedModel). Options: \
\"Order\" -> 2, \"ZeroStart\" -> False, \"Precision\" -> 8.";

ImportAndFitCalibration::usage =
  "ImportAndFitCalibration[file] reads a calibration JSON file and \
fits the polynomial in one step. Same options as FitCalibration.";

WriteCalibrationTemplate::usage =
  "WriteCalibrationTemplate[file] writes a template JSON file with \
representative isotope sources, mirroring `calibrate.py -W`.";

CalibrationSummary::usage =
  "CalibrationSummary[fit] prints a multi-line text summary mirroring \
the Python CLI output.";

Begin["`Private`"];

(* Build the Vandermonde-style design matrix [1 c c^2 ... c^order].
   Avoids 0^0 = Indeterminate by handling the constant column directly. *)
designMatrix[chans_List, order_Integer] :=
  Transpose @ Prepend[
    Table[chans^k, {k, 1, order}],
    ConstantArray[1., Length[chans]]];

Options[FitCalibration] = {
  "Order"     -> 2,
  "ZeroStart" -> False,
  "Precision" -> 8
};

FitCalibration[rawPoints_List, OptionsPattern[]] :=
  Module[{order, zero, prec, points, x, y, A, coeffs, predicted,
          ssRes, ssTot, r2, chMin, chMax, eMin, eMax},
    order = OptionValue["Order"];
    zero  = OptionValue["ZeroStart"];
    prec  = OptionValue["Precision"];
    points = SortBy[rawPoints, First];
    points = DeleteDuplicates[points];
    If[zero && First[points] =!= {0, 0},
       points = Prepend[points, {0, 0}]];
    If[Length[points] < order + 1,
       Return[Failure["NotEnoughPoints",
         <|"MessageTemplate" -> "Need at least `1` points for order-`2` fit; got `3`.",
           "MessageParameters" -> {order + 1, order, Length[points]}|>]]];
    x = N[points[[All, 1]]];
    y = N[points[[All, 2]]];
    A = designMatrix[x, order];
    coeffs = LeastSquares[A, y];
    predicted = A . coeffs;
    ssRes = Total[(y - predicted)^2];
    ssTot = Total[(y - Mean[y])^2];
    r2 = If[ssTot > 0, 1 - ssRes/ssTot, 1.];
    coeffs = Round[coeffs, 10.^-prec];
    {chMin, chMax} = MinMax[points[[All, 1]]];
    {eMin,  eMax}  = MinMax[points[[All, 2]]];
    <|
      "Coefficients"    -> coeffs,
      "RSquared"        -> r2,
      "Range"           -> {{chMin, eMin}, {chMax, eMax}},
      "NumberOfPoints"  -> Length[points],
      "Order"           -> order,
      "ZeroStart"       -> zero,
      "Precision"       -> prec,
      "Points"          -> points,
      "Predicted"       -> predicted
    |>
  ];

ImportAndFitCalibration[file_String, opts : OptionsPattern[FitCalibration]] :=
  Module[{pts},
    pts = RadiaCodeTools`Formats`ImportCalibrationJSON[file];
    If[pts === $Failed, Return[$Failed]];
    FitCalibration[pts, opts]
  ];

WriteCalibrationTemplate[file_String] :=
  Module[{template},
    template = "{
  \"unobtainium\": \"Remove this line after filling in actual calibration measurements. The channel mapping below is a rough (aka. wrong) linear model...\",
  \"americium\": [
    { \"energy\": 26, \"channel\": 9 },
    { \"energy\": 60, \"channel\": 21 }
  ],
  \"barium\": [
    { \"energy\": 80, \"channel\": 28 },
    { \"energy\": 166, \"channel\": 59 },
    { \"energy\": 303, \"channel\": 109 },
    { \"energy\": 356, \"channel\": 128 }
  ],
  \"europium\": [
    { \"energy\": 40, \"channel\": 14 },
    { \"energy\": 122, \"channel\": 44 },
    { \"energy\": 245, \"channel\": 88 },
    { \"energy\": 344, \"channel\": 124 },
    { \"energy\": 1098, \"channel\": 395 },
    { \"energy\": 1408, \"channel\": 507 }
  ],
  \"potassium\": [
    { \"energy\": 1461, \"channel\": 526 }
  ],
  \"radium\": [
    { \"energy\": 295, \"channel\": 106 },
    { \"energy\": 352, \"channel\": 126 },
    { \"energy\": 609, \"channel\": 219 },
    { \"energy\": 1120, \"channel\": 403 },
    { \"energy\": 1765, \"channel\": 635 },
    { \"energy\": 2204, \"channel\": 793 }
  ],
  \"sodium\": [
    { \"energy\": 511, \"channel\": 184 },
    { \"energy\": 1275, \"channel\": 459 }
  ],
  \"thorium\": [
    { \"energy\": 338, \"channel\": 121 },
    { \"energy\": 583, \"channel\": 210 },
    { \"energy\": 911, \"channel\": 328 },
    { \"energy\": 1588, \"channel\": 572 },
    { \"energy\": 2614, \"channel\": 941 }
  ]
}
";
    Export[file, template, "Text"]
  ];

CalibrationSummary[fit_Association] :=
  Module[{r, c, p1, p2},
    {p1, p2} = fit["Range"];
    StringJoin[
      "data range: (", ToString[p1[[1]]], ", ", ToString[p1[[2]]], ") - (",
      ToString[p2[[1]]], ", ", ToString[p2[[2]]], ")\n",
      "x^0 .. x^", ToString[fit["Order"]], ": ",
      ToString[fit["Coefficients"]], "\n",
      "R^2: ", ToString[NumberForm[fit["RSquared"], {6, 5}]], "\n"
    ]
  ];

End[];
EndPackage[];
(* ======================================================================
   SpectrumPlot.wl
   ====================================================================== *)

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
(* ======================================================================
   N42Convert.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`N42Convert`
   Convert RadiaCode XML spectra to ANSI N42 format. *)

BeginPackage["RadiaCodeTools`N42Convert`", {"RadiaCodeTools`Formats`"}];

ConvertRCToN42::usage =
  "ConvertRCToN42[infile, outfile] reads a RadiaCode XML spectrum and \
writes an ANSI N42 file.  Options:\n\
  \"Background\" -> file | None — pull background from a separate file\n\
  \"UUID\"       -> Automatic | uuid string\n\
  \"Overwrite\"  -> False | True\n\
ConvertRCToN42[dir] with \"Recursive\" -> True walks a directory tree, \
converting every *.xml in place to *.xml.n42.";

Begin["`Private`"];

mergeBackground[fgSpec_Association, bgSpec_Association] :=
  Module[{result = fgSpec},
    Which[
      (* prefer the explicit BG file's background layer *)
      AssociationQ[Lookup[bgSpec, "Background", None]],
        result["Background"] = bgSpec["Background"],
      (* otherwise its foreground layer *)
      KeyExistsQ[bgSpec, "Counts"] && Length[bgSpec["Counts"]] > 0,
        result["Background"] = <|
          "Calibration" -> bgSpec["Calibration"],
          "Counts"      -> bgSpec["Counts"],
          "Duration"    -> bgSpec["Duration"]|>
    ];
    result
  ];

Options[ConvertRCToN42] = {
  "Background" -> None,
  "UUID"       -> Automatic,
  "Overwrite"  -> False,
  "Recursive"  -> False
};

ConvertRCToN42[infile_String, outfile_String, OptionsPattern[]] :=
  Module[{bgArg, uuid, overwrite, fgSpec, bgSpec, finalSpec},
    bgArg     = OptionValue["Background"];
    uuid      = OptionValue["UUID"];
    overwrite = OptionValue["Overwrite"];

    If[FileExistsQ[outfile] && !overwrite,
      Return[Failure["Exists",
        <|"MessageTemplate" -> "Output file `1` exists; pass \"Overwrite\" -> True.",
          "MessageParameters" -> {outfile}|>]]];
    fgSpec = RadiaCodeTools`Formats`ImportRCSpectrum[infile];
    If[FailureQ[fgSpec] || fgSpec === $Failed, Return[fgSpec]];
    finalSpec = fgSpec;
    If[StringQ[bgArg] && FileExistsQ[bgArg],
      bgSpec = RadiaCodeTools`Formats`ImportRCSpectrum[bgArg];
      If[!FailureQ[bgSpec] && bgSpec =!= $Failed,
        finalSpec = mergeBackground[fgSpec, bgSpec]]];
    RadiaCodeTools`Formats`ExportN42[outfile, finalSpec, "UUID" -> uuid]
  ];

(* Auto-generated output name *)
ConvertRCToN42[infile_String, opts:OptionsPattern[]] :=
  ConvertRCToN42[infile, infile <> ".n42", opts];

(* Recursive directory mode *)
ConvertRCToN42[dir_String, "Recursive" -> True, opts:OptionsPattern[]] :=
  ConvertRCToN42[dir, opts, "Recursive" -> True];
ConvertRCToN42[dir_String, opts:OptionsPattern[]] /;
  TrueQ[OptionValue[ConvertRCToN42, {opts}, "Recursive"]] &&
  DirectoryQ[dir] :=
  Module[{xmls},
    xmls = FileNames["*.xml", dir, Infinity];
    Map[ConvertRCToN42[#, # <> ".n42",
                        FilterRules[{opts}, Except["Recursive"]]] &, xmls]
  ];

End[];
EndPackage[];
(* ======================================================================
   TrackPlot.wl
   ====================================================================== *)

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
(* ======================================================================
   Deadtime.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`Deadtime`
   Two-source deadtime calculation (Knoll Ch. 4 Eq. 4.32-4.33). *)

BeginPackage["RadiaCodeTools`Deadtime`", {"RadiaCodeTools`Formats`"}];

ComputeDeadtime::usage =
  "ComputeDeadtime[a, b, ab] computes detector deadtime tau (seconds) \
from three count rates: source A alone, source B alone, both together. \
ComputeDeadtime[a, b, ab, bg] subtracts a background rate. Returns an \
Association: Tau (Quantity), LossFraction, LostCps, CombinedRate, \
A, B, AB, BG, Saturated.";

ComputeDeadtimeFromFiles::usage =
  "ComputeDeadtimeFromFiles[fa, fb, fab] reads three RadiaCode XML \
spectrum files, derives count rates as totalCounts/duration, and runs \
ComputeDeadtime. Optional 4th arg = background file.";

CountRateOfSpectrum::usage =
  "CountRateOfSpectrum[spec] returns the foreground count rate \
(counts/second) for a spec Association. Pass \"Background\" -> True \
to use the background layer instead.";

Begin["`Private`"];

CountRateOfSpectrum[spec_Association, OptionsPattern[
  {"Background" -> False}]] :=
  Module[{counts, duration, layer},
    layer = If[OptionValue["Background"], spec["Background"], spec];
    If[!AssociationQ[layer], Return[$Failed]];
    counts = Lookup[layer, "Counts", {}];
    duration = QuantityMagnitude @
                 Lookup[layer, "Duration", Quantity[1, "Seconds"]];
    If[duration <= 0, Return[$Failed]];
    Total[counts] / duration
  ];

ComputeDeadtime[a_?NumericQ, b_?NumericQ, ab_?NumericQ,
                bg_:0] :=
  Module[{X, Y, Z, tau, lostCps, lossFrac, saturated},
    If[bg < 0, Return[Failure["BadInput",
      <|"MessageTemplate" -> "Background cannot be negative."|>]]];
    If[a <= 0 || b <= 0 || ab <= 0, Return[Failure["BadInput",
      <|"MessageTemplate" -> "Source rates must be > 0."|>]]];

    X = a * b - bg * ab;
    Y = a * b * (ab + bg) - bg * ab * (a + b);
    Z = Y * (a + b - ab - bg) / X^2;
    saturated = Z >= 1;
    tau = If[saturated,
             Indeterminate,
             X * (1 - Sqrt[1 - Z]) / Y];

    lostCps = a + b - ab;
    lossFrac = 1 - ab / (a + b);

    <|
      "Tau"           -> If[NumericQ[tau], Quantity[tau, "Seconds"], tau],
      "TauMicroseconds" -> If[NumericQ[tau], tau * 10^6, tau],
      "LossFraction"  -> lossFrac,
      "LostCps"       -> lostCps,
      "CombinedRate"  -> ab,
      "A"             -> a,
      "B"             -> b,
      "AB"            -> ab,
      "BG"            -> bg,
      "Saturated"     -> saturated
    |>
  ];

ComputeDeadtimeFromFiles[fa_String, fb_String, fab_String] :=
  ComputeDeadtimeFromFiles[fa, fb, fab, None];

ComputeDeadtimeFromFiles[fa_String, fb_String, fab_String, fbg_] :=
  Module[{specA, specB, specAB, specBG, ra, rb, rab, rbg},
    specA  = RadiaCodeTools`Formats`ImportRCSpectrum[fa];
    specB  = RadiaCodeTools`Formats`ImportRCSpectrum[fb];
    specAB = RadiaCodeTools`Formats`ImportRCSpectrum[fab];
    ra  = CountRateOfSpectrum[specA];
    rb  = CountRateOfSpectrum[specB];
    rab = CountRateOfSpectrum[specAB];
    rbg = If[StringQ[fbg] && FileExistsQ[fbg],
             specBG = RadiaCodeTools`Formats`ImportRCSpectrum[fbg];
             CountRateOfSpectrum[specBG],
             0];
    ComputeDeadtime[ra, rb, rab, rbg]
  ];

End[];
EndPackage[];
(* ======================================================================
   RecursiveDeadtime.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`RecursiveDeadtime`
   Walk a directory looking for *_a.xml / *_b.xml / *_a+*_b.xml triplets
   and run ComputeDeadtimeFromFiles on each. *)

BeginPackage["RadiaCodeTools`RecursiveDeadtime`",
  {"RadiaCodeTools`Formats`", "RadiaCodeTools`Deadtime`"}];

ScanDeadtime::usage =
  "ScanDeadtime[dir] walks a directory tree and applies the Knoll \
two-source method to each subdirectory whose XML filenames match the \
*_a.xml, *_b.xml, *_a+*_b.xml convention.  Returns a Dataset, one row \
per matched triplet.  Optional second argument is a background XML file.";

Begin["`Private`"];

processTriplet[dir_String, files_List, rateBg_:0.] :=
  Module[{aFile, bFile, abFile, source, dt},
    aFile  = SelectFirst[files,
      StringMatchQ[#, ___ ~~ "_a.xml"] && !StringContainsQ[#, "+"] &, None];
    bFile  = SelectFirst[files,
      StringMatchQ[#, ___ ~~ "_b.xml"] && !StringContainsQ[#, "+"] &, None];
    abFile = SelectFirst[files,
      StringContainsQ[#, "_a+"] && StringEndsQ[#, "_b.xml"] &, None];
    If[aFile === None || bFile === None || abFile === None,
       Return[Missing["IncompleteTriplet", dir]]];
    dt = RadiaCodeTools`Deadtime`ComputeDeadtimeFromFiles[
           FileNameJoin[{dir, aFile}],
           FileNameJoin[{dir, bFile}],
           FileNameJoin[{dir, abFile}]];
    If[FailureQ[dt], Return[dt]];
    (* Override the BG rate post-hoc; recompute deadtime with bg. *)
    If[rateBg > 0,
      dt = RadiaCodeTools`Deadtime`ComputeDeadtime[
             dt["A"], dt["B"], dt["AB"], rateBg]];
    <|
      "Directory" -> dir,
      "A"         -> aFile,
      "B"         -> bFile,
      "AB"        -> abFile,
      "RateA"     -> dt["A"],
      "RateB"     -> dt["B"],
      "RateAB"    -> dt["AB"],
      "RateBG"    -> dt["BG"],
      "TauUs"     -> dt["TauMicroseconds"],
      "LossFraction" -> dt["LossFraction"],
      "Saturated" -> dt["Saturated"]
    |>
  ];

ScanDeadtime[dir_String] := ScanDeadtime[dir, None];

ScanDeadtime[dir_String, bgFile_] :=
  Module[{rateBg = 0., subdirs, results},
    If[StringQ[bgFile] && FileExistsQ[bgFile],
      rateBg = RadiaCodeTools`Deadtime`CountRateOfSpectrum[
                 RadiaCodeTools`Formats`ImportRCSpectrum[bgFile]]];
    subdirs = Select[
      Prepend[FileNames[All, dir, Infinity], dir],
      DirectoryQ];
    results = Function[d,
      With[{xmls = FileNameTake /@ FileNames["*.xml", d]},
        If[Length[xmls] === 3, processTriplet[d, xmls, rateBg], Nothing]]
      ] /@ subdirs;
    results = DeleteCases[results, Nothing | _Missing | _?FailureQ];
    Dataset[results]
  ];

End[];
EndPackage[];
(* ======================================================================
   TrackSanitize.wl
   ====================================================================== *)

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
(* ======================================================================
   TrackEdit.wl
   ====================================================================== *)

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
(* ======================================================================
   SpectrogramEnergy.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`SpectrogramEnergy`
   Total dose / peak rate from spectrograms (port of spectrogram_energy.py). *)

BeginPackage["RadiaCodeTools`SpectrogramEnergy`",
  {"RadiaCodeTools`Formats`"}];

DoseFromSpectrum::usage =
  "DoseFromSpectrum[counts, {a0, a1, a2}] returns the energy deposited \
in microsieverts assuming a CsI:Tl crystal of 1 cm^3 (density 4.51 g/cm^3). \
Optional Density and Volume override.";

SpectrogramEnergy::usage =
  "SpectrogramEnergy[file] reads a .rcspg file and returns an Association \
with TotalDose (uSv), Duration (Quantity), AverageDoseRate (uSv/hour), \
and PeakDoseRate (uSv/hour).";

ScanSpectrogramEnergy::usage =
  "ScanSpectrogramEnergy[dir] walks a directory and applies \
SpectrogramEnergy to every .rcspg file found.  Returns a Dataset.";

Begin["`Private`"];

$joulesPerKev = 1.60218 * 10^-16;

Options[DoseFromSpectrum] = {
  "Density" -> 4.51,    (* CsI:Tl in g/cm^3 *)
  "Volume"  -> 1.0      (* cm^3 *)
};

DoseFromSpectrum[counts_List, cal_List, OptionsPattern[]] :=
  Module[{d, v, mass, channels, energies, totalKeV, gray, uSv},
    d = OptionValue["Density"];
    v = OptionValue["Volume"];
    mass = d * v * 10.^-3;   (* kg *)
    channels = Range[0, Length[counts] - 1];
    energies = RadiaCodeTools`Formats`applyCalibration[cal, channels];
    totalKeV = Total[energies * counts];
    gray = totalKeV * $joulesPerKev / mass;
    uSv = gray * 10.^6;
    uSv
  ];

SpectrogramEnergy[file_String, opts:OptionsPattern[DoseFromSpectrum]] :=
  Module[{spec, samples, cal, channels, sampleData, doses, accTimes,
          peakRate, totalDose, totalDuration, avgRate},
    spec = RadiaCodeTools`Formats`ImportRCSpectrogram[file];
    If[FailureQ[spec] || spec === $Failed, Return[spec]];
    cal = spec["Calibration"];
    channels = spec["NumberOfChannels"];
    samples = Normal[spec["Samples"]];
    If[Length[samples] === 0, Return[$Failed]];
    sampleData = Function[s,
      Module[{c = s["Deltas"]},
        c = PadRight[c, channels, 0];
        {DoseFromSpectrum[c, cal, opts], s["Duration"]}]
      ] /@ samples;
    {doses, accTimes} = Transpose[sampleData];
    accTimes = Replace[accTimes, x_ /; x === 0 -> 1, {1}];
    peakRate = Max[doses / accTimes];
    totalDose = Total[doses];
    totalDuration = Total[accTimes];
    avgRate = If[totalDuration > 0,
                  3600. * totalDose / totalDuration,
                  0.];
    <|
      "File" -> file,
      "TotalDose" -> totalDose,
      "Duration" -> Quantity[totalDuration, "Seconds"],
      "AverageDoseRate" -> avgRate,
      "PeakDoseRate" -> peakRate * 3600.,
      "SampleCount" -> Length[samples]
    |>
  ];

ScanSpectrogramEnergy[dir_String, opts:OptionsPattern[DoseFromSpectrum]] :=
  Module[{files, results},
    files = FileNames["*.rcspg", dir, Infinity];
    results = Map[Quiet @ SpectrogramEnergy[#, opts] &, files];
    Dataset @ Cases[results, _Association]
  ];

End[];
EndPackage[];
(* ======================================================================
   SpectroPlot.wl
   ====================================================================== *)

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
(* ======================================================================
   RCSpgFromJson.wl
   ====================================================================== *)

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
(* ======================================================================
   RCTrkFromJson.wl
   ====================================================================== *)

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
(* ======================================================================
   N42Validate.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`N42Validate`
   Best-effort structural validation of ANSI N42 files.

   Wolfram doesn't ship a full XSD validator, so we run two layers:
     1. A built-in structural check (root tag, required children,
        non-empty channel data, calibration sanity).
     2. If `xmllint` is installed, optionally call it with an XSD path
        for full schema validation.
*)

BeginPackage["RadiaCodeTools`N42Validate`",
  {"RadiaCodeTools`Formats`"}];

ValidateN42::usage =
  "ValidateN42[file] runs a structural check on an N42 file.  Returns \
an Association with keys Valid (Boolean), Issues (list of strings), and \
File. Pass \"Schema\" -> path to an n42.xsd to additionally invoke \
xmllint for full XSD validation.";

ValidateN42Recursive::usage =
  "ValidateN42Recursive[dir] walks a directory tree and validates every \
*.n42 file. Returns a Dataset of validation results.";

Begin["`Private`"];

structuralCheck[file_String] :=
  Module[{xml, root, issues = {}, ns = "http://physics.nist.gov/N42/2011/N42",
          rootTag, rii, rdi, ec, meas, spec, chData, calStr, calNums,
          countStr, counts},
    xml = Quiet @ Import[file, "XML"];
    If[xml === $Failed,
      Return[<|"Valid" -> False, "File" -> file,
                "Issues" -> {"could not parse XML"}|>]];
    root = FirstCase[xml,
      XMLElement[{ns, "RadInstrumentData"} | "RadInstrumentData", _, _],
      Missing[], Infinity];
    If[Head[root] =!= XMLElement,
      AppendTo[issues, "no <RadInstrumentData> root element"];
      Return[<|"Valid" -> False, "File" -> file, "Issues" -> issues|>]];

    rii  = FirstCase[root, XMLElement[{ns, "RadInstrumentInformation"} | "RadInstrumentInformation", _, _], None, Infinity];
    If[rii === None, AppendTo[issues, "missing <RadInstrumentInformation>"]];
    rdi  = FirstCase[root, XMLElement[{ns, "RadDetectorInformation"} | "RadDetectorInformation", _, _], None, Infinity];
    If[rdi === None, AppendTo[issues, "missing <RadDetectorInformation>"]];
    ec   = FirstCase[root, XMLElement[{ns, "EnergyCalibration"} | "EnergyCalibration", _, _], None, Infinity];
    If[ec === None, AppendTo[issues, "missing <EnergyCalibration>"]];
    meas = FirstCase[root, XMLElement[{ns, "RadMeasurement"} | "RadMeasurement", _, _], None, Infinity];
    If[meas === None, AppendTo[issues, "missing <RadMeasurement>"]];

    If[ec =!= None,
      calStr = StringTrim @ FirstCase[ec,
        XMLElement[{ns, "CoefficientValues"} | "CoefficientValues", _, ch_] :>
          StringJoin[Cases[ch, _String]],
        "", Infinity];
      calNums = ToExpression /@ StringSplit[calStr];
      If[!ListQ[calNums] || Length[calNums] < 2 ||
         !AllTrue[calNums, NumericQ],
        AppendTo[issues, "calibration coefficients missing or non-numeric"]]
    ];

    If[meas =!= None,
      spec = FirstCase[meas,
        XMLElement[{ns, "Spectrum"} | "Spectrum", _, _], None, Infinity];
      If[spec === None,
        AppendTo[issues, "<RadMeasurement> has no <Spectrum>"],
        chData = FirstCase[spec,
          XMLElement[{ns, "ChannelData"} | "ChannelData", _, ch_] :>
            StringJoin[Cases[ch, _String]],
          "", Infinity];
        countStr = StringTrim[chData];
        If[countStr === "",
          AppendTo[issues, "<ChannelData> empty"],
          counts = ToExpression /@ StringSplit[countStr];
          If[!ListQ[counts] || !AllTrue[counts, IntegerQ],
            AppendTo[issues, "channel counts non-integer"]]]
      ]
    ];

    <|"Valid" -> issues === {}, "File" -> file, "Issues" -> issues|>
  ];

xmllintCheck[file_String, xsd_String] :=
  Module[{out, success, lint},
    lint = "xmllint";
    out = RunProcess[{lint, "--noout", "--schema", xsd, file},
                     ProcessEnvironment -> <||>];
    success = (out["ExitCode"] === 0);
    <|"Valid" -> success,
      "Output" -> StringTrim[out["StandardError"]]|>
  ];

Options[ValidateN42] = {"Schema" -> None};

ValidateN42[file_String, OptionsPattern[]] :=
  Module[{result, schema, lintResult},
    result = structuralCheck[file];
    schema = OptionValue["Schema"];
    If[StringQ[schema] && FileExistsQ[schema] &&
       FileType["xmllint" /. {"xmllint" -> Quiet @ FindFile["xmllint"]}] =!= File,
      (* fall through if xmllint not on PATH *)
      Null];
    If[StringQ[schema] && FileExistsQ[schema],
      lintResult = Quiet @ Check[xmllintCheck[file, schema],
                                  <|"Valid" -> "unavailable"|>];
      result = <|result,
                  "XmllintValid"  -> lintResult["Valid"],
                  "XmllintOutput" -> Lookup[lintResult, "Output", ""]|>];
    result
  ];

ValidateN42Recursive[dir_String, opts:OptionsPattern[ValidateN42]] :=
  Module[{files},
    files = FileNames["*.n42", dir, Infinity];
    Dataset[ValidateN42[#, opts] & /@ files]
  ];

End[];
EndPackage[];
(* ======================================================================
   Spectroscopy.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`Spectroscopy`
   Photopeak detection, isotope identification against a built-in
   gamma-line library, single-peak Gaussian fitting and the
   FWHM/E energy-resolution metric.
*)

BeginPackage["RadiaCodeTools`Spectroscopy`",
  {"RadiaCodeTools`Formats`"}];

FindPhotopeaks::usage =
  "FindPhotopeaks[spec] returns a list of associations describing \
candidate photopeaks in a RadiaCode spectrum (as returned by \
ImportRCSpectrum).  Each row carries the channel index, the calibrated \
energy in keV, the raw counts at the peak channel, and a \
prominence-style score relative to a smoothed background.  Options:\n\
  \"MinEnergy\"   -> 30        (* keV; ignore the very low end *)\n\
  \"Smoothing\"   -> 9         (* moving-average window for the trend *)\n\
  \"MinProminence\" -> 0.05    (* fractional excess over local trend *)\n\
  \"MaxPeaks\"    -> 12        (* upper bound on returned peaks *)";

IsotopeLibrary::usage =
  "IsotopeLibrary[] returns the built-in association of common \
gamma-emitting isotopes -> list of <|\"Energy\" -> keV, \"Intensity\" \
-> branching fraction|>.  The library covers calibration / survey \
favourites: Am-241, Cs-137, Co-60, K-40, Na-22, Ba-133, Eu-152, \
Mn-54, Co-57, Bi-214 (Ra-226 series) and Tl-208 (Th-232 series).";

IdentifyIsotopes::usage =
  "IdentifyIsotopes[spec] looks at the photopeaks in a RadiaCode \
spectrum and ranks library isotopes by how many of their characteristic \
gamma lines match an observed peak.  Returns a Dataset, best match \
first.  Options:\n\
  \"Tolerance\"   -> 8         (* keV; match window per peak *)\n\
  \"MinIntensity\" -> 0.05     (* drop very weak library lines *)\n\
  \"MaxResults\"  -> 8         (* truncate the ranked list *)\n\
  \"MinEnergy\"   -> 30        (* same as FindPhotopeaks *)\n\
  IdentifyIsotopes accepts the same options as FindPhotopeaks plus \
the tolerance; FindPhotopeaks options are forwarded.";

FitGaussianPeak::usage =
  "FitGaussianPeak[spec, energy] fits A Exp[-(E-mu)^2/(2 sigma^2)] + b + \
m (E - energy) to the channel counts in a window around the requested \
energy and returns an Association with Mean, Sigma, FWHM, Amplitude, \
Background, Slope, FitWindow (in keV) and FitObject (the underlying \
NonlinearModelFit).  Option \"Window\" -> Automatic | dE in keV.";

EnergyResolution::usage =
  "EnergyResolution[spec, energy] fits a Gaussian to the photopeak \
nearest the requested energy and returns an Association with \
Centroid, FWHM, Resolution (FWHM/E, dimensionless), and ResolutionPercent. \
For a CsI(Tl) scintillator like the RadiaCode the resolution at \
662 keV (Cs-137) is typically 7\[Dash]10 %.  Option \"Window\" -> \
Automatic | dE in keV is forwarded to FitGaussianPeak.";

PlotPeakFit::usage =
  "PlotPeakFit[spec, energy] fits a Gaussian + linear background to \
the photopeak near the requested energy and returns a Show[] of the \
data points and fitted curve, labelled with the FWHM / E result.  \
Same options as FitGaussianPeak (\"Window\") plus \"PlotLabel\" and \
\"ImageSize\".";

Begin["`Private`"];

(* ===== Built-in gamma-line library ============================ *)

(* Energies in keV, intensity = absolute branching ratio
   (gammas per decay).  Sources: Lawrence Berkeley Lab "Table of
   Radioactive Isotopes" / IAEA Nuclear Data Sheets.  Where multiple
   close lines exist (e.g. Eu-152 121-122 keV doublet) we keep the
   strongest representative in this lookup table. *)
$isotopeLibrary = <|
  "Am-241" -> {<|"Energy" -> 26.34,  "Intensity" -> 0.024|>,
               <|"Energy" -> 59.54,  "Intensity" -> 0.359|>},
  "Co-57"  -> {<|"Energy" -> 122.06, "Intensity" -> 0.856|>,
               <|"Energy" -> 136.47, "Intensity" -> 0.107|>},
  "Ba-133" -> {<|"Energy" -> 80.997, "Intensity" -> 0.329|>,
               <|"Energy" -> 276.40, "Intensity" -> 0.0716|>,
               <|"Energy" -> 302.85, "Intensity" -> 0.1834|>,
               <|"Energy" -> 356.01, "Intensity" -> 0.6205|>,
               <|"Energy" -> 383.85, "Intensity" -> 0.0894|>},
  "Cs-137" -> {<|"Energy" -> 661.66, "Intensity" -> 0.851|>},
  "Mn-54"  -> {<|"Energy" -> 834.85, "Intensity" -> 0.99976|>},
  "Co-60"  -> {<|"Energy" -> 1173.23,"Intensity" -> 0.9985|>,
               <|"Energy" -> 1332.49,"Intensity" -> 0.9998|>},
  (* Na-22 emits ~1.798 photons per decay at 511 keV (positron-
     annihilation pair, with ~89.8 % beta+ branch).  Capped at 1.0
     here so the Score/PeakWeight normalisation in matchIsotope is
     on the same per-decay scale as the other entries. *)
  "Na-22"  -> {<|"Energy" -> 511.0,  "Intensity" -> 1.0|>,
               <|"Energy" -> 1274.54,"Intensity" -> 0.9994|>},
  "K-40"   -> {<|"Energy" -> 1460.82,"Intensity" -> 0.1066|>},
  "Eu-152" -> {<|"Energy" -> 121.78, "Intensity" -> 0.2858|>,
               <|"Energy" -> 244.70, "Intensity" -> 0.0759|>,
               <|"Energy" -> 344.28, "Intensity" -> 0.2658|>,
               <|"Energy" -> 778.90, "Intensity" -> 0.1296|>,
               <|"Energy" -> 964.08, "Intensity" -> 0.1462|>,
               <|"Energy" -> 1112.08,"Intensity" -> 0.1354|>,
               <|"Energy" -> 1408.01,"Intensity" -> 0.2085|>},
  (* Bi-214: dominant gamma emitter in the Ra-226/U-238 chain *)
  "Bi-214" -> {<|"Energy" -> 609.31, "Intensity" -> 0.4549|>,
               <|"Energy" -> 768.36, "Intensity" -> 0.0489|>,
               <|"Energy" -> 1120.29,"Intensity" -> 0.1491|>,
               <|"Energy" -> 1238.11,"Intensity" -> 0.0586|>,
               <|"Energy" -> 1764.49,"Intensity" -> 0.1531|>,
               <|"Energy" -> 2204.21,"Intensity" -> 0.0489|>},
  (* Pb-214: also part of the Ra-226 series, soft lines *)
  "Pb-214" -> {<|"Energy" -> 295.22, "Intensity" -> 0.184|>,
               <|"Energy" -> 351.93, "Intensity" -> 0.356|>},
  (* Tl-208: end of the Th-232 chain; the 2614 keV line is the
     strongest natural terrestrial gamma. *)
  "Tl-208" -> {<|"Energy" -> 277.36, "Intensity" -> 0.0625|>,
               <|"Energy" -> 510.77, "Intensity" -> 0.226|>,
               <|"Energy" -> 583.19, "Intensity" -> 0.846|>,
               <|"Energy" -> 763.13, "Intensity" -> 0.0162|>,
               <|"Energy" -> 860.56, "Intensity" -> 0.1242|>,
               <|"Energy" -> 2614.51,"Intensity" -> 0.9975|>},
  (* Ac-228: Th-232 series, in equilibrium *)
  "Ac-228" -> {<|"Energy" -> 338.32, "Intensity" -> 0.1127|>,
               <|"Energy" -> 911.20, "Intensity" -> 0.258|>,
               <|"Energy" -> 968.97, "Intensity" -> 0.158|>},
  (* Pb-212: Th-232 series, soft *)
  "Pb-212" -> {<|"Energy" -> 238.63, "Intensity" -> 0.434|>,
               <|"Energy" -> 300.09, "Intensity" -> 0.033|>}
|>;

IsotopeLibrary[] := $isotopeLibrary;

(* ===== Peak finding =========================================== *)

(* Centred moving average; pads to keep length.  Uses MovingAverage
   on the bulk of the series (running-sum O(n)) and only falls back
   to manual slicing for the half-window pad at each end, vs. the
   previous full O(n*w) Table+Mean. *)
movingMean[xs_List, w_Integer] :=
  Module[{n = Length[xs], half, body, head, tail, xsN},
    xsN = N @ xs;
    half = Quotient[w, 2];
    body = MovingAverage[xsN, w];
    head = Table[Mean[xsN[[1 ;; Min[n, i + half]]]], {i, 1, half}];
    tail = Table[Mean[xsN[[Max[1, i - half] ;; n]]],
                  {i, n - half + 1, n}];
    Join[head, body, tail]];

Options[FindPhotopeaks] = {
  "MinEnergy"     -> 30.,
  "Smoothing"     -> 9,
  "MinProminence" -> 0.05,
  "MaxPeaks"      -> 12
};

FindPhotopeaks[spec_Association, OptionsPattern[]] :=
  Module[{counts, cal, n, energies, smoothed, excess, peaks,
          minE, win, minP, maxN, indices, ranked},
    counts = N @ Lookup[spec, "Counts", {}];
    cal = Lookup[spec, "Calibration", {0., 1., 0.}];
    n = Length[counts];
    If[n === 0, Return[{}]];
    energies = RadiaCodeTools`Formats`applyCalibration[cal, Range[0, n - 1]];
    minE = OptionValue["MinEnergy"];
    win  = Max[3, OptionValue["Smoothing"]];
    minP = OptionValue["MinProminence"];
    maxN = OptionValue["MaxPeaks"];

    (* Smooth the spectrum with a window 5x the per-peak smoothing
       width to estimate a slow Compton-continuum trend; the 5x
       factor keeps the trend from absorbing the peaks themselves
       while still tracking the broad continuum shape. *)
    smoothed = movingMean[counts, 5 win];
    excess = counts - smoothed;

    (* Local maxima in the excess: c[i] strictly greater than its
       immediate neighbours (within +/- 2 channels), and prominence
       (excess / smoothed-trend) above the threshold. *)
    indices =
      Select[Range[3, n - 2],
        Function[i,
          And[
            energies[[i]] >= minE,
            counts[[i]] > 0,
            counts[[i]] >= counts[[i - 1]],
            counts[[i]] >= counts[[i + 1]],
            counts[[i]] > counts[[i - 2]],
            counts[[i]] > counts[[i + 2]],
            excess[[i]] > minP * Max[1., smoothed[[i]]]
          ]]];

    (* Suppress neighbours within +/- (smoothing window) channels of
       a stronger peak so a single physical peak is reported once. *)
    indices = SortBy[indices, -counts[[#]] &];
    (* Non-max suppression with Reap/Sow instead of AppendTo so this
       stays O(n) rather than O(n^2). *)
    indices = Module[{kept = {}},
      Reap[
        Do[
          If[NoneTrue[kept, Abs[# - i] <= win &],
            Sow[i]; AppendTo[kept, i]],
          {i, indices}]
      ][[2]] /. {{x_List} :> x, {} -> {}}];

    ranked = SortBy[indices, -counts[[#]] &];
    ranked = Take[ranked, UpTo[maxN]];

    Map[
      Function[i,
        <|"Channel"     -> i - 1,
          "Energy"      -> energies[[i]],
          "Counts"      -> counts[[i]],
          "Prominence"  -> excess[[i]] / Max[1., smoothed[[i]]]|>],
      ranked]
  ];

(* ===== Isotope identification ================================= *)

Options[IdentifyIsotopes] = Join[
  {"Tolerance"    -> 8.,
   "MinIntensity" -> 0.05,
   "MaxResults"   -> 8},
  Options[FindPhotopeaks]];

(* For each library line find the best-matching observed peak within
   `tol` keV.  Two complementary scores:
     "Score"        = sum(matched intensity) / sum(library intensity)
                      \[Dash] fraction of the isotope's expected lines
                      that we actually see.
     "PeakWeight"   = sum over matched lines of (intensity * peak counts)
                      \[Dash] high when matched lines also dominate the
                      observed spectrum.  This is what discriminates a
                      real Cs-137 (one strong line, big peak) from a
                      coincidental match against a small noise peak. *)
matchIsotope[isotope_String, lines_List, peaks_List, tol_?NumericQ] :=
  Module[{matchedLines = {}, totalI = 0., matchedI = 0.,
          unmatchedLines = {}, peakEnergies, peakCounts,
          maxPeakCounts, peakWeight = 0.},
    peakEnergies = Lookup[#, "Energy"] & /@ peaks;
    peakCounts   = Lookup[#, "Counts"] & /@ peaks;
    maxPeakCounts = If[Length[peakCounts] > 0, Max[peakCounts], 1.];
    totalI = Total[Lookup[#, "Intensity"] & /@ lines];

    Do[
      Module[{e = lines[[k, "Energy"]], inten = lines[[k, "Intensity"]],
              best = None, bestDelta = Infinity, j, pc},
        Do[
          With[{d = Abs[peakEnergies[[j]] - e]},
            If[d <= tol && d < bestDelta,
               best = j; bestDelta = d]],
          {j, Length[peakEnergies]}];
        If[best =!= None,
          pc = peakCounts[[best]];
          AppendTo[matchedLines,
            <|"Energy"      -> e,
              "Intensity"   -> inten,
              "PeakEnergy"  -> peakEnergies[[best]],
              "PeakCounts"  -> pc,
              "Delta"       -> bestDelta|>];
          matchedI += inten;
          peakWeight += inten * (pc / maxPeakCounts),
          AppendTo[unmatchedLines, lines[[k]]]
        ]],
      {k, Length[lines]}];

    <|"Isotope"    -> isotope,
      "Score"      -> If[totalI > 0, matchedI / totalI, 0.],
      "PeakWeight" -> peakWeight,
      "Matched"    -> Length[matchedLines],
      "Total"      -> Length[lines],
      "Lines"      -> matchedLines,
      "Missed"     -> unmatchedLines|>
  ];

IdentifyIsotopes[spec_Association, opts:OptionsPattern[]] :=
  Module[{peaks, tol, minI, maxR, lib, scored, fpOpts},
    fpOpts = FilterRules[{opts}, Options[FindPhotopeaks]];
    peaks = FindPhotopeaks[spec, Sequence @@ fpOpts];
    tol  = OptionValue["Tolerance"];
    minI = OptionValue["MinIntensity"];
    maxR = OptionValue["MaxResults"];
    lib = Association @ KeyValueMap[
      Function[{name, lines},
        name -> Select[lines, #["Intensity"] >= minI &]],
      $isotopeLibrary];
    lib = Select[lib, Length[#] > 0 &];

    scored = KeyValueMap[
      Function[{name, lines}, matchIsotope[name, lines, peaks, tol]],
      lib];

    (* Rank by peak-weighted score (gives priority to candidates whose
       matched lines explain the strongest observed peaks), then by
       fraction matched, then by raw match count. *)
    scored = ReverseSortBy[scored,
      {#["PeakWeight"], #["Score"], #["Matched"]} &];
    scored = Take[Select[scored, #["Matched"] > 0 &], UpTo[maxR]];

    Dataset @ Map[
      <|"Isotope"      -> #["Isotope"],
        "Matched"      -> #["Matched"],
        "TotalLines"   -> #["Total"],
        "Score"        -> Round[#["Score"], 0.001],
        "PeakWeight"   -> Round[#["PeakWeight"], 0.001],
        "MatchedLines" -> Dataset[#["Lines"]]|> &,
      scored]
  ];

(* ===== Single-peak Gaussian fit =============================== *)

(* Take a window of points around a target energy, fit
   counts(E) = A Exp[-(E-mu)^2/(2 sigma^2)] + b + m (E - E0).  *)
Options[FitGaussianPeak] = {"Window" -> Automatic};

FitGaussianPeak[spec_Association, energy_?NumericQ,
                opts:OptionsPattern[]] :=
  Module[{counts, cal, n, energies, dE, xs, ys, peakIdx,
          mu0, sig0, A0, b0, fit, bestRules,
          fitMu, fitSig, fitAmp, fitBg, fitSlope, fitFwhm,
          eMin, eMax, lo, hi, optWin},
    counts = N @ Lookup[spec, "Counts", {}];
    cal = Lookup[spec, "Calibration", {0., 1., 0.}];
    n = Length[counts];
    If[n === 0, Return[$Failed]];
    energies = RadiaCodeTools`Formats`applyCalibration[cal, Range[0, n - 1]];
    optWin = OptionValue["Window"];
    dE = If[NumericQ[optWin],
            optWin,
            (* Auto window: roughly +/- 3 sigma assuming a 10 % energy
               resolution at the requested energy, capped so we always
               have enough samples for a 5-parameter fit even at low
               energies where the calibration channel pitch limits us. *)
            Max[12., 0.15 * energy]];
    eMin = energy - dE; eMax = energy + dE;
    lo = LengthWhile[energies, # < eMin &] + 1;
    hi = LengthWhile[energies, # < eMax &];
    (* Need enough samples in the fit window for a 5-parameter
       Levenberg-Marquardt fit (amp, mu, sigma, baseline, slope).
       Anything under ~8 samples is underdetermined in practice. *)
    If[hi - lo < 8, Return[$Failed]];
    xs = energies[[lo ;; hi]];
    ys = counts[[lo ;; hi]];

    peakIdx = First[Reverse @ Ordering[ys]];
    mu0 = xs[[peakIdx]];
    A0  = Max[ys];
    b0  = Quantile[ys, 0.1];
    sig0 = Max[1., 0.05 * energy];

    (* FindFit instead of NonlinearModelFit: the latter triggers the
       Statistics`LinearRegression`/Statistics`NonlinearRegression`
       autoload chain that goes through GeneralizedLinearModelFit /
       LogitModelFit / ProbitModelFit, and on at least one Wolfram
       front-end build that chain leaves the call unevaluated.  When
       that happens, `fit["BestFitParameters"]` evaluates to the
       literal `NonlinearModelFit[...][BestFitParameters]` which then
       trips ReplaceAll downstream.  FindFit returns the parameter
       rules directly with no FittedModel wrapper or autoload chain. *)
    bestRules = Quiet @ Check[
      FindFit[
        Transpose[{xs, ys}],
        ah Exp[-(x - mu)^2 / (2 sg^2)] + bg + sl (x - mu0),
        {{ah, Max[1., A0 - b0]}, {mu, mu0}, {sg, sig0},
         {bg, Max[0., b0]}, {sl, 0.}},
        x,
        Method -> "LevenbergMarquardt",
        MaxIterations -> 400],
      $Failed];
    If[!ListQ[bestRules] || bestRules === $Failed, Return[$Failed]];

    fitAmp   = ah /. bestRules;
    fitMu    = mu /. bestRules;
    fitSig   = Abs[sg /. bestRules];
    fitBg    = bg /. bestRules;
    fitSlope = sl /. bestRules;
    fitFwhm  = 2. Sqrt[2. Log[2.]] fitSig;
    fit = bestRules;

    <|"Mean"         -> fitMu,
      "Sigma"        -> fitSig,
      "FWHM"         -> fitFwhm,
      "Amplitude"    -> fitAmp,
      "Background"   -> fitBg,
      "Slope"        -> fitSlope,
      "WindowCenter" -> mu0,
      "FitWindow"    -> {eMin, eMax},
      "DataPoints"   -> Length[xs],
      "FitObject"    -> fit|>
  ];

(* ===== Energy resolution ====================================== *)

Options[EnergyResolution] = Options[FitGaussianPeak];

EnergyResolution[spec_Association, energy_?NumericQ,
                 opts:OptionsPattern[]] :=
  Module[{fit, mu, fwhm, res},
    fit = FitGaussianPeak[spec, energy, opts];
    If[fit === $Failed || !AssociationQ[fit], Return[$Failed]];
    mu = fit["Mean"]; fwhm = fit["FWHM"];
    If[!NumericQ[mu] || !NumericQ[fwhm] || mu <= 0, Return[$Failed]];
    res = fwhm / mu;
    <|"Centroid"          -> mu,
      "FWHM"              -> fwhm,
      "Resolution"        -> res,
      "ResolutionPercent" -> 100. res|>
  ];

(* ===== Helper: visualise a peak fit =========================== *)

Options[PlotPeakFit] = Join[Options[FitGaussianPeak],
  {"PlotLabel" -> Automatic, "ImageSize" -> 600}];

PlotPeakFit[spec_Association, energy_?NumericQ,
            opts:OptionsPattern[]] :=
  Module[{fit, lo, hi, mu, sigma, amp, bg, slope, mu0, counts, cal,
          energies, pts, label, fitOpts, modelFn},
    fitOpts = FilterRules[{opts}, Options[FitGaussianPeak]];
    fit = FitGaussianPeak[spec, energy, Sequence @@ fitOpts];
    If[!AssociationQ[fit], Return[$Failed]];
    {lo, hi} = fit["FitWindow"];
    mu     = fit["Mean"];
    sigma  = fit["Sigma"];
    amp    = fit["Amplitude"];
    bg     = fit["Background"];
    slope  = fit["Slope"];
    mu0    = fit["WindowCenter"];
    counts = Lookup[spec, "Counts", {}];
    cal    = Lookup[spec, "Calibration", {0., 1., 0.}];
    energies = RadiaCodeTools`Formats`applyCalibration[
                 cal, Range[0, Length[counts] - 1]];
    pts = Select[Transpose[{energies, counts}], lo <= First[#] <= hi &];
    label = OptionValue["PlotLabel"];
    If[label === Automatic,
       label = Row[{NumberForm[mu, {5, 1}], " keV: ",
                    NumberForm[100. fit["FWHM"]/mu, 3], " % FWHM"}]];
    (* Reconstruct the fitted curve from explicit numeric parameters
       so Plot does not have to evaluate the FittedModel object;
       avoids the FE recursion/recursion-depth issue and is also
       faster. *)
    modelFn[en_?NumericQ] :=
      amp Exp[-(en - mu)^2 / (2 sigma^2)] + bg + slope (en - mu0);
    Show[
      ListPlot[pts, PlotStyle -> {Black, PointSize[Medium]},
        PlotMarkers -> Automatic],
      Plot[modelFn[en], {en, lo, hi},
        PlotStyle -> {Red, Thick}, PlotPoints -> 200],
      Frame -> True, FrameLabel -> {"Energy / keV", "Counts"},
      PlotLabel -> label,
      ImageSize -> OptionValue["ImageSize"],
      GridLines -> Automatic]
  ];

End[];
EndPackage[];
(* ======================================================================
   LiveViewer.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`LiveViewer`
   Live consumption of an rcmultispg ndjson stream — live spectrum,
   count-rate trace, and dose-rate trace as a Dynamic dashboard.

   Architecture (see Wolfram/README.md): RadiaCode is USB-HID, not
   serial.  The Python `rcmultispg.py --stdout` tool drives the device
   and writes one JSON record per line; this package tails that stream
   from a file or via a subprocess and feeds a Dynamic[] view. *)

BeginPackage["RadiaCodeTools`LiveViewer`",
  {"RadiaCodeTools`Formats`", "RadiaCodeTools`SpectrumPlot`"}];

OpenRadiaCodeStream::usage =
  "OpenRadiaCodeStream[command] starts a subprocess (a list of \
arguments suitable for StartProcess) whose stdout is an ndjson stream \
and tails it.  OpenRadiaCodeStream[file_String] tails an existing \
file.  Both forms return a stream id (string) that can be passed to \
RadiaCodeDashboard / CloseRadiaCodeStream / RadiaCodeStreamState.  \
Options:\n\
  \"PollInterval\" -> seconds (default 0.5)";

CloseRadiaCodeStream::usage =
  "CloseRadiaCodeStream[id] stops the polling task, terminates the \
subprocess if any, and deletes the buffer file.";

RadiaCodeDashboard::usage =
  "RadiaCodeDashboard[id] returns a Dynamic[] dashboard showing the \
latest spectrum, count-rate trace, and dose-rate trace for the named \
stream.";

RadiaCodeStreamState::usage =
  "RadiaCodeStreamState[id] returns the current Association of \
accumulated stream state (spectrum, realtime history, gps history, \
record count, status).";

RadiaCodeListStreams::usage =
  "RadiaCodeListStreams[] returns the ids of all open streams.";

PlaybackNDJson::usage =
  "PlaybackNDJson[file] is a convenience wrapper that opens an ndjson \
file as a stream for offline playback / development.";

PollRadiaCodeStream::usage =
  "PollRadiaCodeStream[id] forces an immediate read of any new lines \
in the buffer.  Normally called automatically by the scheduled task, \
but useful for testing or for manually-driven dashboards.";

Begin["`Private`"];

$streams = <||>;
$nextId = 0;

newId[] := (
  $nextId++;
  "rc-stream-" <> ToString[$nextId]);

freshState[id_, source_] := <|
  "Id"             -> id,
  "Source"         -> source,
  "Process"        -> None,
  "BufferFile"     -> None,
  "Position"       -> 0,
  "Task"           -> None,
  "Status"         -> "open",
  "RecordCount"    -> 0,
  "StartTime"      -> Now,
  "LastUpdate"     -> None,
  "Spectrum"       -> <|"Counts" -> {}, "Calibration" -> {0., 1., 0.},
                         "SerialNumber" -> "", "Duration" -> Missing[]|>,
  "Realtime"       -> {},
  "GPS"            -> {},
  "MaxRealtime"    -> 600   (* keep the last 10 minutes of realtime samples *)
|>;

(* ----- record dispatch ----- *)

(* rcmultispg has shipped (at least) two ndjson layouts:

     OLD (xray.ndjson):
       {"timestamp": ..., "serial_number": ...,
        "duration": ..., "calibration": [a0,a1,a2], "counts": [...]}
       {"timestamp": ..., "count_rate": ..., "dose_rate": ...}

     NEW (live rcmultispg --stdout):
       {"dt": ..., "serial_number": ...,
        "spectrum": {"duration": ..., "a0":, "a1":, "a2":, "counts":[]},
        "_type": "SpecData"}
       {"dt": ..., "count_rate": ..., "dose_rate": ..., "_type": "RtData"}

   Recognise either. *)

extractSpectrumPayload[rec_Association] :=
  Module[{nested},
    Which[
      KeyExistsQ[rec, "counts"] && KeyExistsQ[rec, "calibration"],
        <|"Counts" -> Lookup[rec, "counts"],
          "Calibration" -> Lookup[rec, "calibration"],
          "SerialNumber" -> Lookup[rec, "serial_number", ""],
          "Duration" -> Quantity[Lookup[rec, "duration", 0], "Seconds"]|>,

      AssociationQ[Lookup[rec, "spectrum", None]],
        nested = rec["spectrum"];
        <|"Counts" -> Lookup[nested, "counts", {}],
          "Calibration" -> {Lookup[nested, "a0", 0.],
                             Lookup[nested, "a1", 1.],
                             Lookup[nested, "a2", 0.]},
          "SerialNumber" -> Lookup[rec, "serial_number", ""],
          "Duration" -> Quantity[Lookup[nested, "duration", 0], "Seconds"]|>,

      True, None
    ]
  ];

dispatchRecord[id_, rec_Association] :=
  Module[{state = $streams[id], rt, payload},
    payload = extractSpectrumPayload[rec];
    Which[
      AssociationQ[payload],
        state["Spectrum"] = payload,

      KeyExistsQ[rec, "count_rate"],
        (* JSON null comes back as Null; map it to Missing[] so
           downstream consumers can use Cases[_?NumericQ] without
           tripping on Null in plot/Quantile/Mean calls. *)
        rt = #1 /. {Null -> Missing["NotAvailable"]} & @ <|
          "Time"         -> Now,
          "SerialNumber" -> Lookup[rec, "serial_number", ""],
          "CountRate"    -> Lookup[rec, "count_rate", Missing[]],
          "DoseRate"     -> Lookup[rec, "dose_rate",  Missing[]],
          "Charge"       -> Lookup[rec, "charge_level", Missing[]],
          "Temperature"  -> Lookup[rec, "temperature", Missing[]]
        |>;
        state["Realtime"] = Take[
          Append[state["Realtime"], rt],
          -Min[Length[state["Realtime"]] + 1, state["MaxRealtime"]]],

      KeyExistsQ[rec, "lat"],
        state["GPS"] = Append[state["GPS"], <|
          "Time" -> Now,
          "Lat"  -> Lookup[rec, "lat", 0.],
          "Lon"  -> Lookup[rec, "lon", 0.],
          "Alt"  -> Lookup[rec, "alt", Missing[]],
          "Mode" -> Lookup[rec, "mode", Missing[]]|>]
    ];
    state["RecordCount"] = state["RecordCount"] + 1;
    state["LastUpdate"]  = Now;
    $streams[id] = state;
  ];

dispatchRecord[id_, _] := Null;

processNewLines[id_, lines_List] :=
  Scan[
    Function[line,
      Module[{rec},
        rec = Quiet @ ImportString[line, "RawJSON"];
        If[AssociationQ[rec], dispatchRecord[id, rec]]]],
    lines];

(* ----- buffer-file polling ----- *)

PollRadiaCodeStream[id_String] :=
  Module[{state, file, pos, totalBytes, strm, bytes, txt, lines},
    state = Lookup[$streams, id, $Failed];
    If[state === $Failed || state["Status"] === "closed", Return[]];
    file = state["BufferFile"];
    If[!StringQ[file] || !FileExistsQ[file], Return[]];
    pos = state["Position"];
    totalBytes = FileByteCount[file];
    If[totalBytes <= pos, Return[]];
    strm = OpenRead[file, BinaryFormat -> True];
    SetStreamPosition[strm, pos];
    bytes = BinaryReadList[strm, "Byte", totalBytes - pos];
    Close[strm];
    state["Position"] = totalBytes;
    $streams[id] = state;
    If[!ListQ[bytes] || Length[bytes] === 0, Return[]];
    txt = FromCharacterCode[bytes, "UTF-8"];
    lines = Select[StringSplit[txt, "\n"], StringTrim[#] =!= "" &];
    processNewLines[id, lines];
  ];

(* Detect a dead subprocess and mark the stream finished. *)
checkProcessAlive[id_] :=
  Module[{state = $streams[id], proc, status},
    If[!AssociationQ[state], Return[]];
    proc = state["Process"];
    If[proc =!= None,
      status = Quiet @ ProcessStatus[proc];
      If[StringQ[status] && status =!= "Running",
        state["Status"] = If[status === "Finished", "exited", status];
        $streams[id] = state]]
  ];

(* ----- open / close ----- *)

Options[OpenRadiaCodeStream] = {"PollInterval" -> 0.5};

OpenRadiaCodeStream[command_List, OptionsPattern[]] :=
  Module[{id, bufFile, proc, task, interval, sourceDescr},
    id = newId[];
    bufFile = FileNameJoin[{$TemporaryDirectory, id <> ".ndjson"}];
    interval = OptionValue["PollInterval"];
    sourceDescr = StringRiffle[ToString /@ command, " "];
    (* Start the subprocess BEFORE creating the buffer file, so a
       StartProcess failure leaves no orphan files on disk and no
       partially-initialised stream entry.  `exec` replaces the sh
       with the target process so the PID we hold IS the python's;
       KillProcess then actually kills the rcmultispg subprocess. *)
    proc = Quiet @ Check[
      StartProcess[
        {"sh", "-c",
          "exec " <>
          StringRiffle[
            Function[c, "'" <> StringReplace[c, "'" -> "'\\''"] <> "'"] /@ command,
            " "] <> " >> '" <> bufFile <> "'"}],
      $Failed];
    If[proc === $Failed || !MatchQ[proc, _ProcessObject],
      Return[Failure["StartProcessFailed",
        <|"MessageTemplate" -> "Could not start subprocess for `1`.",
          "MessageParameters" -> {sourceDescr}|>]]];
    Export[bufFile, "", "Text"];
    $streams[id] = <|freshState[id, sourceDescr],
      "BufferFile" -> bufFile,
      "Process"    -> proc|>;
    task = RunScheduledTask[
      (PollRadiaCodeStream[id]; checkProcessAlive[id]),
      interval];
    (* $streams[id]["Task"] = ... is a no-op (assigns to a copy);
       merge through the top-level variable so the task handle
       persists and CloseRadiaCodeStream can actually remove it. *)
    $streams[id] = <|$streams[id], "Task" -> task|>;
    id];

OpenRadiaCodeStream[file_String, OptionsPattern[]] :=
  Module[{id, interval, task},
    If[!FileExistsQ[file],
      Return[Failure["NotFound",
        <|"MessageTemplate" -> "ndjson file `1` not found.",
          "MessageParameters" -> {file}|>]]];
    id = newId[];
    interval = OptionValue["PollInterval"];
    $streams[id] = <|freshState[id, file],
      "BufferFile" -> file,
      "Process"    -> None|>;
    task = RunScheduledTask[PollRadiaCodeStream[id], interval];
    $streams[id] = <|$streams[id], "Task" -> task|>;
    PollRadiaCodeStream[id];   (* immediate first read *)
    id];

PlaybackNDJson[file_String, opts:OptionsPattern[OpenRadiaCodeStream]] :=
  OpenRadiaCodeStream[file, opts];

CloseRadiaCodeStream[id_String] :=
  Module[{state = Lookup[$streams, id, $Failed], status},
    If[state === $Failed, Return[$Failed]];
    If[state["Task"] =!= None,
      Quiet @ RemoveScheduledTask[state["Task"]]];
    If[state["Process"] =!= None,
      status = Quiet @ ProcessStatus[state["Process"]];
      If[status === "Running",
        Quiet @ KillProcess[state["Process"]]]];
    (* Only delete the buffer file if WE created it — i.e. a subprocess
       stream — never delete a file the user passed in directly. *)
    If[state["Process"] =!= None && StringQ[state["BufferFile"]] &&
       FileExistsQ[state["BufferFile"]],
      Quiet @ DeleteFile[state["BufferFile"]]];
    state["Status"] = "closed";
    $streams[id] = state;
    id];

(* ----- queries ----- *)

RadiaCodeStreamState[id_String] :=
  Lookup[$streams, id, Failure["NotFound",
    <|"MessageTemplate" -> "Unknown stream `1`",
      "MessageParameters" -> {id}|>]];

RadiaCodeListStreams[] := Keys[$streams];

(* ----- dashboard ----- *)

$accentColor   = RGBColor[0.20, 0.55, 0.85];
$warmColor     = RGBColor[0.85, 0.32, 0.20];
$coolColor     = RGBColor[0.20, 0.45, 0.75];
$labelStyle    = Directive[FontSize -> 12, FontFamily -> "Helvetica", GrayLevel[0.35]];
$valueStyle    = Directive[FontSize -> 14, FontFamily -> "Helvetica", Bold, GrayLevel[0.10]];
$panelTitleStyle = Directive[FontSize -> 14, FontFamily -> "Helvetica",
                              Bold, $accentColor];
$panelFrame    = Directive[GrayLevel[0.85], Thickness[Tiny]];

panelFrame[content_, title_:""] :=
  Framed[
    Column[{
      If[title === "",
        Sequence @@ {},
        Style[title, $panelTitleStyle]
      ],
      content
    }, Spacings -> 0.4],
    FrameStyle  -> $panelFrame,
    Background  -> GrayLevel[0.98],
    RoundingRadius -> 6,
    FrameMargins -> 8];

statusPanel[state_Association] :=
  Module[{age, runtime, latestRate, latestDose, formatRow},
    age = If[state["LastUpdate"] === None, "\[LongDash]",
       ToString @ Round @ QuantityMagnitude[
         UnitConvert[Now - state["LastUpdate"], "Seconds"]]] <> " s ago";
    runtime = ToString @ Round @ QuantityMagnitude[
       UnitConvert[Now - state["StartTime"], "Seconds"]] <> " s";
    latestRate = If[Length[state["Realtime"]] > 0,
      Lookup[Last[state["Realtime"]], "CountRate", Missing[]],
      Missing[]];
    latestDose = If[Length[state["Realtime"]] > 0,
      Lookup[Last[state["Realtime"]], "DoseRate", Missing[]],
      Missing[]];
    formatRow[label_, value_, color_:GrayLevel[0.10]] :=
      {Style[label, $labelStyle],
       Style[value, $valueStyle, color]};
    panelFrame[
      Grid[{
        formatRow["Status",
          ToUpperCase[state["Status"]],
          If[state["Status"] === "open", $accentColor, $warmColor]],
        formatRow["Serial",
          Lookup[state["Spectrum"], "SerialNumber", "\[LongDash]"]],
        formatRow["Records", state["RecordCount"]],
        formatRow["Updated", age],
        formatRow["Uptime",  runtime],
        formatRow["Count rate",
          If[NumericQ[latestRate],
            ToString[NumberForm[latestRate, {5, 2}]] <> " cps",
            "\[LongDash]"], $coolColor],
        formatRow["Dose rate",
          If[NumericQ[latestDose],
            ToString[NumberForm[latestDose * 10.^6, {5, 2}]] <> " \[Mu]Sv/h",
            "\[LongDash]"], $warmColor]
      }, Alignment -> {{Right, Left}}, Spacings -> {1, 0.7}],
      "RadiaCode live"]
  ];

spectrumPanel[state_Association] :=
  panelFrame[
    If[Length[state["Spectrum"]["Counts"]] > 0,
      RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[state["Spectrum"],
        "Background" -> False, "PlotLabel" -> None],
      Style["awaiting first spectrum\[Ellipsis]", Italic, Gray, FontSize -> 14]
    ],
    "Spectrum (counts vs energy)"];

ratePanel[state_Association, key_String, label_String, color_] :=
  Module[{rt = state["Realtime"], pairs, doseScale},
    doseScale = If[key === "DoseRate", 10.^6, 1.];   (* convert Sv/h to uSv/h *)
    pairs = Select[
      {Lookup[#, "Time"], doseScale * Lookup[#, key]} & /@ rt,
      MatchQ[#, {_DateObject, _?NumericQ}] &];
    panelFrame[
      If[Length[pairs] === 0,
        Style["no samples yet\[Ellipsis]", Italic, Gray, FontSize -> 14],
        DateListPlot[pairs,
          Joined        -> True,
          PlotStyle     -> Directive[color, Thickness[0.005]],
          Filling       -> Bottom,
          FillingStyle  -> Directive[color, Opacity[0.18]],
          Frame         -> True,
          FrameStyle    -> $panelFrame,
          FrameLabel    -> {None, label},
          LabelStyle    -> $labelStyle,
          ImageSize     -> 480,
          GridLines     -> Automatic,
          GridLinesStyle -> Directive[GrayLevel[0.92], Thickness[Tiny]],
          AspectRatio   -> 0.4]],
      label]
  ];

buildDashboard[id_String] :=
  Module[{state = Lookup[$streams, id, None]},
    If[!AssociationQ[state],
      Return[Style["Stream " <> id <> " is closed or unknown.", Red]]];
    Column[{
      Grid[{{statusPanel[state], spectrumPanel[state]}},
           Spacings -> {1.5, 0}, Alignment -> {Left, Top}],
      Grid[{{ratePanel[state, "CountRate", "Count rate / cps",      $coolColor],
             ratePanel[state, "DoseRate",  "Dose rate / \[Mu]Sv/h", $warmColor]}},
           Spacings -> {1.5, 0}, Alignment -> {Left, Top}]
    }, Spacings -> 1]
  ];

(* HoldFirst so that the Dynamic re-evaluates the symbol passed in
   (typically `streamId`) on every tick.  If the user closes a
   stream and re-opens, streamId is re-bound, and the dashboard
   automatically follows the new stream rather than freezing on the
   string id captured at first call.  Plain string ids still work
   because Dynamic just looks them up unchanged. *)
SetAttributes[RadiaCodeDashboard, HoldFirst];

RadiaCodeDashboard[idExpr_] :=
  Dynamic[Refresh[buildDashboard[idExpr], UpdateInterval -> 1]];

End[];
EndPackage[];
(* ======================================================================
   Device.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`Device`
   Wolfram-native API for talking to a connected RadiaCode device.

   Implementation note
   -------------------
   RadiaCode is a vendor-class USB device with bulk endpoints
   (0x01 / 0x81) using a custom command set; Wolfram
   ships no libusb bindings, so a *truly* native implementation would
   require LibraryLink + hidapi (or libusb).  As a pragmatic stand-in
   we drive the device through the upstream Python tools — the same
   `radiacode` library `radiacode_poll.py` and `rcmultispg.py` use —
   and parse the standard outputs back into Wolfram structures.  The
   call signatures below are the Wolfram API surface; the bridge is an
   implementation detail, and a future C-level rewrite via LibraryLink
   would not change them.
*)

BeginPackage["RadiaCodeTools`Device`",
  {"RadiaCodeTools`Formats`",
   "RadiaCodeTools`LiveViewer`"}];

RadiaCodeDevices::usage =
  "RadiaCodeDevices[] returns a list of serial numbers of every \
RadiaCode device currently attached over USB.";

RadiaCodeAcquire::usage =
  "RadiaCodeAcquire[] reads the device's current cumulative spectrum \
and returns it as an Association compatible with ImportN42 / \
ImportRCSpectrum.  Options:\n\
  \"AccumulationTime\" -> Quantity | seconds (default 0 — instantaneous \
read)\n\
  \"AccumulationDose\" -> uSv (mutually exclusive with time)\n\
  \"Bluetooth\"        -> Mac address string | Automatic\n\
  \"BackgroundSubtract\" -> True | False\n\
  \"OutputFile\"       -> Automatic | path (default: temp; deleted on return)";

RadiaCodeStream::usage =
  "RadiaCodeStream[] starts an rcmultispg subprocess and returns a \
LiveViewer stream id which can be passed to RadiaCodeDashboard / \
RadiaCodeStreamState / CloseRadiaCodeStream.  Options:\n\
  \"Devices\"        -> list of serial numbers | Automatic (all)\n\
  \"PollingInterval\" -> seconds between device polls (default 5)\n\
  \"GpsdURL\"        -> URL string | None\n\
  \"PollInterval\"   -> Wolfram-side stream poll period (default 0.5)";

RadiaCodeReset::usage =
  "RadiaCodeReset[\"Spectrum\"] resets the device's accumulated \
spectrum.  RadiaCodeReset[\"Dose\"] resets the cumulative dose.  Both \
are destructive — confirm intent first.";

$RadiaCodePython::usage =
  "$RadiaCodePython holds the path or name of the Python interpreter \
used by Device.wl.  Default: \"python3\".";

$RadiaCodeRepoRoot::usage =
  "$RadiaCodeRepoRoot is the directory holding the upstream repo's \
src/ subfolder (which provides the `radiacode_tools` package).  \
Auto-detected from this file's location.";

Begin["`Private`"];

(* ----- locate the repo ----- *)

resolveRepoRoot[] :=
  Module[{here, candidates, trial},
    here = DirectoryName[$InputFileName /. "" :> NotebookFileName[]];
    candidates = NestList[ParentDirectory, here, 6];
    trial = SelectFirst[candidates,
      DirectoryQ[FileNameJoin[{#, "src", "radiacode_tools"}]] &,
      None];
    If[trial === None, ParentDirectory[ParentDirectory[here]], trial]
  ];

(* The Mathematica front-end on macOS gets a stripped-down PATH that
   typically excludes Homebrew, anaconda, and `~/.local/bin`, so a bare
   "python3" RunProcess call fails with "Program python3 not found".
   Resolve to an absolute path at load time. *)
detectPython[] :=
  Module[{candidates, found, which, home},
    home = $HomeDirectory;
    candidates = {
      FileNameJoin[{home, "anaconda3", "bin", "python3"}],
      FileNameJoin[{home, "miniconda3", "bin", "python3"}],
      FileNameJoin[{home, ".local", "bin", "python3"}],
      "/opt/homebrew/bin/python3",
      "/opt/anaconda3/bin/python3",
      "/usr/local/bin/python3",
      "/usr/bin/python3"
    };
    found = SelectFirst[candidates, FileExistsQ];
    If[StringQ[found], Return[found]];
    (* Fallback: ask the login shell, which loads the user's profile *)
    which = Quiet @ RunProcess[
      {"/bin/bash", "-l", "-c", "command -v python3"}, "StandardOutput"];
    If[StringQ[which] && StringTrim[which] =!= "" &&
       FileExistsQ[StringTrim[which]],
       StringTrim[which],
       "python3"]
  ];

(* Re-evaluate on every Get so a stale value left over from an older
   version of this package doesn't pin us to a broken interpreter.
   Preserve the user's explicit override iff it points to a real file. *)
If[!StringQ[$RadiaCodeRepoRoot] || !DirectoryQ[$RadiaCodeRepoRoot],
   $RadiaCodeRepoRoot = resolveRepoRoot[]];
If[!StringQ[$RadiaCodePython] || !FileExistsQ[$RadiaCodePython],
   $RadiaCodePython = detectPython[]];

repoSrc[] := FileNameJoin[{$RadiaCodeRepoRoot, "src"}];

scriptPath[name_String] :=
  FileNameJoin[{repoSrc[], name <> ".py"}];

(* Merge PYTHONPATH into the inherited environment rather than
   replacing it; the child still needs HOME, LANG, USER, etc. *)
buildPythonEnv[] :=
  Module[{env, current},
    env = Quiet @ GetEnvironment[];
    If[!ListQ[env], env = {}];
    current = Lookup[Association @@ env, "PYTHONPATH", ""];
    Append[
      Association @@ env,
      "PYTHONPATH" -> If[current === "" || current === None,
                          repoSrc[],
                          repoSrc[] <> ":" <> current]]
  ];

runPython[args_List] :=
  RunProcess[
    Prepend[args, $RadiaCodePython],
    All,
    "",
    ProcessEnvironment -> buildPythonEnv[]
  ];

(* ----- device enumeration ----- *)

RadiaCodeDevices[] :=
  Module[{out, lines},
    out = runPython[{"-c",
      "from radiacode_tools.rc_utils import find_radiacode_devices\n" <>
      "import sys\n" <>
      "for sn in find_radiacode_devices(): print(sn)"}];
    If[!AssociationQ[out] || out["ExitCode"] =!= 0,
      Return[Failure["PythonBridge",
        <|"MessageTemplate" -> "Could not enumerate devices: `1`",
          "MessageParameters" -> {Lookup[out, "StandardError", "?"]}|>]]];
    lines = Select[StringSplit[Lookup[out, "StandardOutput", ""], "\n"],
                    StringTrim[#] =!= "" &];
    StringTrim /@ lines
  ];

(* ----- single-shot acquisition (radiacode_poll.py wrapper) ----- *)

formatHMS[seconds_?NumericQ] :=
  Module[{s = Round[seconds], h, m},
    h = Quotient[s, 3600];
    m = Quotient[Mod[s, 3600], 60];
    s = Mod[s, 60];
    StringJoin[
      IntegerString[h, 10, 2], ":",
      IntegerString[m, 10, 2], ":",
      IntegerString[s, 10, 2]]
  ];

durationToSeconds[q_Quantity] := QuantityMagnitude[UnitConvert[q, "Seconds"]];
durationToSeconds[n_?NumericQ] := n;

Options[RadiaCodeAcquire] = {
  "AccumulationTime"    -> Automatic,
  "AccumulationDose"    -> Automatic,
  "Bluetooth"           -> None,
  "BackgroundSubtract"  -> False,
  "OutputFile"          -> Automatic
};

RadiaCodeAcquire[OptionsPattern[]] :=
  Module[{accT, accD, bt, bgSub, outFile, args, proc, result, cleanup},
    accT   = OptionValue["AccumulationTime"];
    accD   = OptionValue["AccumulationDose"];
    bt     = OptionValue["Bluetooth"];
    bgSub  = OptionValue["BackgroundSubtract"];
    outFile = OptionValue["OutputFile"];
    cleanup = (outFile === Automatic);
    If[cleanup,
      outFile = FileNameJoin[{$TemporaryDirectory,
        "rcacquire-" <> ToString[RandomInteger[10^9]] <> ".n42"}];
      If[FileExistsQ[outFile], DeleteFile[outFile]]];

    args = {scriptPath["radiacode_poll"]};
    If[StringQ[bt], args = Join[args, {"-b", bt}]];
    Which[
      accT =!= Automatic && accT =!= None,
        args = Join[args, {"--accumulate-time", formatHMS[durationToSeconds[accT]]}],
      accD =!= Automatic && accD =!= None,
        args = Join[args, {"--accumulate-dose", ToString[accD]}]];
    If[bgSub, AppendTo[args, "-B"]];
    args = Append[args, outFile];

    proc = runPython[args];
    If[!AssociationQ[proc] || proc["ExitCode"] =!= 0,
      If[cleanup && FileExistsQ[outFile], DeleteFile[outFile]];
      Return[Failure["AcquireFailed",
        <|"MessageTemplate" -> "radiacode_poll exited `1`: `2`",
          "MessageParameters" -> {Lookup[proc, "ExitCode", "?"],
                                   Lookup[proc, "StandardError", ""]}|>]]];
    If[!FileExistsQ[outFile],
      Return[Failure["AcquireFailed",
        <|"MessageTemplate" -> "radiacode_poll wrote no output to `1`",
          "MessageParameters" -> {outFile}|>]]];
    result = RadiaCodeTools`Formats`ImportN42[outFile];
    If[cleanup, Quiet @ DeleteFile[outFile]];
    result
  ];

(* ----- streaming (rcmultispg.py wrapper) ----- *)

Options[RadiaCodeStream] = {
  "Devices"         -> Automatic,
  "PollingInterval" -> 5.0,
  "GpsdURL"         -> None,
  "PollInterval"    -> 0.5
};

RadiaCodeStream[OptionsPattern[]] :=
  Module[{devices, interval, gpsd, command, pollI, env, envPrefix},
    devices  = OptionValue["Devices"];
    interval = OptionValue["PollingInterval"];
    gpsd     = OptionValue["GpsdURL"];
    pollI    = OptionValue["PollInterval"];

    (* The subprocess inherits PATH from `sh`, but we still need
       PYTHONPATH set so radiacode_tools is importable from src/. *)
    envPrefix = {"env", "PYTHONPATH=" <> repoSrc[]};
    command = Join[envPrefix,
      {$RadiaCodePython, scriptPath["rcmultispg"], "--stdout",
       "-i", ToString[interval]}];
    If[ListQ[devices],
      Scan[(command = Join[command, {"-d", #}]) &, devices]];
    If[StringQ[gpsd], command = Join[command, {"-g", gpsd}]];

    RadiaCodeTools`LiveViewer`OpenRadiaCodeStream[command,
      "PollInterval" -> pollI]
  ];

(* ----- reset ----- *)

RadiaCodeReset[what_String] :=
  Module[{flag, args, proc, tmp},
    flag = Switch[what,
      "Spectrum", "--reset-spectrum",
      "Dose",     "--reset-dose",
      _,          Return[Failure["BadArgument",
                    <|"MessageTemplate" ->
                       "RadiaCodeReset target must be \"Spectrum\" or \"Dose\""|>]]];
    tmp = FileNameJoin[{$TemporaryDirectory,
      "rcreset-" <> ToString[RandomInteger[10^9]] <> ".n42"}];
    If[FileExistsQ[tmp], DeleteFile[tmp]];
    args = {scriptPath["radiacode_poll"], flag, tmp};
    proc = runPython[args];
    If[FileExistsQ[tmp], Quiet @ DeleteFile[tmp]];
    If[!AssociationQ[proc] || proc["ExitCode"] =!= 0,
      Return[Failure["ResetFailed",
        <|"MessageTemplate" -> "reset exited `1`: `2`",
          "MessageParameters" -> {Lookup[proc, "ExitCode", "?"],
                                   Lookup[proc, "StandardError", ""]}|>]]];
    True
  ];

End[];
EndPackage[];
(* ======================================================================
   DeviceNative.wl
   ====================================================================== *)

(* ::Package:: *)

(* RadiaCodeTools`DeviceNative`
   Native Wolfram driver for RadiaCode USB devices via LibraryLink +
   libusb.  No Python at runtime.

   Public API (compatible with Device.wl conventions):

     RadiaCodeNativeAvailableQ[]
       True iff the compiled library was found and loaded successfully.

     RadiaCodeNativeDevices[]
       List of attached RadiaCode serial number strings.

     RadiaCodeNativeOpen[serial]   -> handle (non-negative integer)
     RadiaCodeNativeClose[handle]  -> True / Failure

     RadiaCodeNativeReadSpectrum[handle]
       Association compatible with ImportRCSpectrum:
         "SerialNumber"    -> "RC-103-..."
         "Calibration"     -> {a0, a1, a2}
         "Counts"          -> packed integer array
         "NumberOfChannels"-> Length[Counts]
         "Duration"        -> seconds (Integer)

     RadiaCodeNativeReadRealtime[handle]
       Latest RealTimeData record decoded out of the device's data
       buffer, as <|"CountRate" -> ..., "DoseRate" -> ..., ...|>, or
       Missing["NoData"] if the buffer didn't contain any RealTimeData
       record this poll.

     RadiaCodeNativeReset[handle, "Spectrum" | "Dose"]
       Resets the cumulative spectrum or accumulated dose.  Returns True.

   The compiled library is built by clib/build.wls.  If it isn't built,
   loading this package leaves $RadiaCodeNativeLibrary === None and the
   public functions return Failure[...] without crashing.
*)

BeginPackage["RadiaCodeTools`DeviceNative`"];

RadiaCodeNativeAvailableQ::usage =
  "RadiaCodeNativeAvailableQ[] returns True if the native LibraryLink \
shim is loaded and ready, False otherwise.";

RadiaCodeNativeDevices::usage =
  "RadiaCodeNativeDevices[] returns a list of RadiaCode serial numbers \
attached over USB, talking directly to the device via libusb.";

RadiaCodeNativeOpen::usage =
  "RadiaCodeNativeOpen[serial] opens the RadiaCode whose USB serial \
matches `serial` and returns an integer handle.";

RadiaCodeNativeClose::usage =
  "RadiaCodeNativeClose[handle] closes a handle returned by \
RadiaCodeNativeOpen.";

RadiaCodeNativeReadSpectrum::usage =
  "RadiaCodeNativeReadSpectrum[handle] reads the cumulative spectrum \
and returns an Association compatible with ImportRCSpectrum.";

RadiaCodeNativeReadRealtime::usage =
  "RadiaCodeNativeReadRealtime[handle] returns the latest realtime \
record (count rate, dose rate, errors, flags) from the device data \
buffer, or Missing[\"NoData\"] if no record was available this poll.";

RadiaCodeNativeReset::usage =
  "RadiaCodeNativeReset[handle, \"Spectrum\"|\"Dose\"] resets the \
device's accumulated spectrum or dose.";

$RadiaCodeNativeLibrary::usage =
  "$RadiaCodeNativeLibrary is the path to the compiled radiacode_link \
shared library, or None if it could not be located/loaded.";

Begin["`Private`"];

(* ---- locate / load the compiled library ---- *)

libBaseDir := DirectoryName[$InputFileName /. "" :> NotebookFileName[]];
libCandidates[] := With[{base = libBaseDir},
  FileNameJoin[{base, "clib", "radiacode_link." <> #}] & /@
    {"dylib", "so", "dll"}];

resolveLib[] := SelectFirst[libCandidates[], FileExistsQ, None];

$RadiaCodeNativeLibrary = resolveLib[];

(* function handles -- populated lazily once we've found the .dylib *)
$rcEnumerate = Null;
$rcOpen      = Null;
$rcClose     = Null;
$rcSerial    = Null;
$rcExecute   = Null;

loadFunctions[lib_String] :=
  Module[{ok = True},
    $rcEnumerate = Quiet @ LibraryFunctionLoad[lib, "RC_Enumerate",
                                                {}, "UTF8String"];
    If[!MatchQ[$rcEnumerate, _LibraryFunction], ok = False];
    $rcOpen = Quiet @ LibraryFunctionLoad[lib, "RC_Open",
                                           {"UTF8String"}, Integer];
    If[!MatchQ[$rcOpen, _LibraryFunction], ok = False];
    $rcClose = Quiet @ LibraryFunctionLoad[lib, "RC_Close",
                                            {Integer}, Integer];
    If[!MatchQ[$rcClose, _LibraryFunction], ok = False];
    $rcSerial = Quiet @ LibraryFunctionLoad[lib, "RC_Serial",
                                             {Integer}, "UTF8String"];
    If[!MatchQ[$rcSerial, _LibraryFunction], ok = False];
    $rcExecute = Quiet @ LibraryFunctionLoad[lib, "RC_Execute",
        {Integer, {NumericArray, "Constant"}}, {NumericArray, "Constant"}];
    If[!MatchQ[$rcExecute, _LibraryFunction], ok = False];
    ok
  ];

If[StringQ[$RadiaCodeNativeLibrary] && FileExistsQ[$RadiaCodeNativeLibrary],
  If[!loadFunctions[$RadiaCodeNativeLibrary],
    $RadiaCodeNativeLibrary = None]];

RadiaCodeNativeAvailableQ[] := StringQ[$RadiaCodeNativeLibrary] &&
                                MatchQ[$rcEnumerate, _LibraryFunction];

unavailable[fn_] := Failure["RadiaCodeNativeUnavailable",
  <|"MessageTemplate" -> "Native library not loaded; rebuild via " <>
                         "Wolfram/RadiaCodeTools/clib/build.wls.  " <>
                         "Function: `1`",
    "MessageParameters" -> {fn}|>];

(* ---- enumerate ---- *)

RadiaCodeNativeDevices[] :=
  If[!RadiaCodeNativeAvailableQ[],
    unavailable["RadiaCodeNativeDevices"],
    Module[{s},
      s = Quiet @ $rcEnumerate[];
      If[!StringQ[s], Return[{}]];
      If[StringTrim[s] === "", {},
        Select[StringSplit[s, "\n"], StringTrim[#] =!= "" &]
      ]
    ]
  ];

(* ---- open / close ---- *)

RadiaCodeNativeOpen[serial_String] :=
  If[!RadiaCodeNativeAvailableQ[],
    unavailable["RadiaCodeNativeOpen"],
    Module[{h},
      h = Quiet @ $rcOpen[serial];
      If[IntegerQ[h] && h >= 0,
        initDevice[h];
        h,
        Failure["DeviceOpenFailed",
          <|"MessageTemplate" -> "Could not open RadiaCode `1`",
            "MessageParameters" -> {serial}|>]]
    ]
  ];
RadiaCodeNativeOpen[] := With[{lst = RadiaCodeNativeDevices[]},
  If[ListQ[lst] && Length[lst] >= 1, RadiaCodeNativeOpen[First[lst]],
    Failure["NoDevices", <|"MessageTemplate" -> "No RadiaCode devices found"|>]]
];

RadiaCodeNativeClose[handle_Integer] :=
  If[!RadiaCodeNativeAvailableQ[],
    unavailable["RadiaCodeNativeClose"],
    Quiet @ $rcClose[handle]; True
  ];

(* ---- protocol layer (mirrors radiacode/radiacode.py) ---- *)

(* per-handle state: sequence counter, base time, format version *)
$state = <||>;

initState[h_Integer] := ($state[h] = <|
  "Seq" -> 0,
  "BaseTime" -> AbsoluteTime[],
  "FormatVersion" -> 0|>);

bumpSeq[h_Integer] :=
  Module[{s = Lookup[$state, h, <||>], v},
    v = Lookup[s, "Seq", 0];
    s["Seq"] = Mod[v + 1, 32];
    $state[h] = s;
    v
  ];

(* low-level: send a command code + payload, return BytesBuffer-style
   list of UInt8 ints (the payload AFTER the 4-byte echo of the request
   header has been stripped). *)
exec[h_Integer, cmd_Integer, args_List] :=
  Module[{seq, header, full, lenPrefix, naIn, naOut, raw, header4, payload},
    seq = bumpSeq[h];
    (* header: <HBB = u16 cmd LE, u8 0, u8 0x80|seq *)
    header = Join[{Mod[cmd, 256], Quotient[cmd, 256] (* LE u16 *),
                   0, BitOr[16^^80, seq]}, args];
    (* wrap with 4-byte LE length prefix *)
    Module[{n = Length[header]},
      lenPrefix = {Mod[n, 256], Mod[Quotient[n, 256], 256],
                   Mod[Quotient[n, 65536], 256],
                   Mod[Quotient[n, 16777216], 256]}];
    full = Join[lenPrefix, header];
    naIn = NumericArray[full, "UnsignedInteger8"];
    naOut = $rcExecute[h, naIn];
    raw = Normal[naOut];
    If[!ListQ[raw] || Length[raw] < 4,
      Return[Failure["TransportError",
        <|"MessageTemplate" -> "Empty response from device"|>]]];
    (* first 4 bytes are echo of req header; verify *)
    header4 = Take[raw, 4];
    If[header4 =!= {Mod[cmd, 256], Quotient[cmd, 256], 0, BitOr[16^^80, seq]},
      Return[Failure["BadEcho",
        <|"MessageTemplate" -> "Header mismatch: req=`1` resp=`2`",
          "MessageParameters" -> {{Mod[cmd, 256], Quotient[cmd, 256], 0,
                                    BitOr[16^^80, seq]}, header4}|>]]];
    payload = Drop[raw, 4];
    payload
  ];

(* helpers for unpacking ints/floats out of a UInt8 list, mutating an
   AssociationVar that holds {"data" -> bytes, "pos" -> int} *)
takeBytes[buf_, n_Integer] := Module[{p = buf["pos"], data = buf["data"]},
  buf["pos"] = p + n;
  buf,
  data[[p + 1 ;; p + n]]];

(* return {value, newpos} *)
readU32[bytes_List, pos_Integer] := Module[{b = bytes[[pos + 1 ;; pos + 4]]},
  {b[[1]] + 256 b[[2]] + 65536 b[[3]] + 16777216 b[[4]], pos + 4}];
readU16[bytes_List, pos_Integer] := Module[{b = bytes[[pos + 1 ;; pos + 2]]},
  {b[[1]] + 256 b[[2]], pos + 2}];
readU8 [bytes_List, pos_Integer] := {bytes[[pos + 1]], pos + 1};
readS8 [bytes_List, pos_Integer] :=
  Module[{v = bytes[[pos + 1]]}, {If[v >= 128, v - 256, v], pos + 1}];
readS16[bytes_List, pos_Integer] :=
  Module[{v = First[readU16[bytes, pos]]},
    {If[v >= 32768, v - 65536, v], pos + 2}];
readS32[bytes_List, pos_Integer] :=
  Module[{v = First[readU32[bytes, pos]]},
    {If[v >= 2147483648, v - 4294967296, v], pos + 4}];

(* IEEE-754 binary32 little-endian float decode *)
readF32[bytes_List, pos_Integer] := Module[
  {b = bytes[[pos + 1 ;; pos + 4]], u32, sign, exp, mant, val},
  u32 = b[[1]] + 256 b[[2]] + 65536 b[[3]] + 16777216 b[[4]];
  sign = If[BitAnd[u32, 16^^80000000] != 0, -1, 1];
  exp  = BitAnd[BitShiftRight[u32, 23], 16^^FF];
  mant = BitAnd[u32, 16^^7FFFFF];
  val = Which[
    exp == 0,
      sign * mant * 2.^-149,                  (* subnormal / zero *)
    exp == 16^^FF,
      If[mant == 0, sign * Infinity, Indeterminate],
    True,
      sign * (1 + mant * 2.^-23) * 2.^(exp - 127)
  ];
  {val, pos + 4}
];

(* ---- protocol commands (subset) ---- *)

cmdSetExchange     = 16^^0007;
cmdGetVersion      = 16^^000A;
cmdRdVirtSfr       = 16^^0824;
cmdWrVirtSfr       = 16^^0825;
cmdRdVirtString    = 16^^0826;
cmdWrVirtString    = 16^^0827;
cmdSetTime         = 16^^0A04;

vsConfiguration    = 2;
vsSerialNumber     = 8;
vsSpectrum         = 16^^200;
vsEnergyCalib      = 16^^202;
vsSpecAccum        = 16^^205;
vsDataBuf          = 16^^100;

vsfrDeviceTime     = 16^^0504;
vsfrDoseReset      = 16^^8007;

(* read VS string: RD_VIRT_STRING with VS id, response: <retcode u32>,
   <flen u32>, then `flen` payload bytes *)
readRequest[h_Integer, vsId_Integer] :=
  Module[{p, retcode, flen, p2, payload},
    p = exec[h, cmdRdVirtString,
      {Mod[vsId, 256], Mod[Quotient[vsId, 256], 256],
       Mod[Quotient[vsId, 65536], 256], Mod[Quotient[vsId, 16777216], 256]}];
    If[FailureQ[p], Return[p]];
    {retcode, p2} = readU32[p, 0];
    {flen, p2}    = readU32[p, p2];
    If[retcode =!= 1, Return[Failure["BadRetcode",
      <|"MessageTemplate" -> "VS read returned retcode=`1`",
        "MessageParameters" -> {retcode}|>]]];
    payload = p[[p2 + 1 ;; p2 + flen]];
    (* HACK from python: trailing 0x00 sometimes added by new firmware *)
    If[Length[p] - p2 == flen + 1 && p[[-1]] === 0,
       payload = p[[p2 + 1 ;; -2]]];
    payload
  ];

(* write VSFR: WR_VIRT_SFR with id, then optional data *)
writeVSFR[h_Integer, vsfrId_Integer, data_List : {}] :=
  Module[{idBytes, p, retcode, p2},
    idBytes = {Mod[vsfrId, 256], Mod[Quotient[vsfrId, 256], 256],
               Mod[Quotient[vsfrId, 65536], 256],
               Mod[Quotient[vsfrId, 16777216], 256]};
    p = exec[h, cmdWrVirtSfr, Join[idBytes, data]];
    If[FailureQ[p], Return[p]];
    {retcode, p2} = readU32[p, 0];
    retcode == 1
  ];

(* ---- init ---- *)

initDevice[h_Integer] :=
  Module[{p, conf, lines, line},
    initState[h];
    (* SET_EXCHANGE 01 ff 12 ff *)
    exec[h, cmdSetExchange, {16^^01, 16^^FF, 16^^12, 16^^FF}];
    (* SET_TIME — just send something so device's monotonic clock is ok *)
    Module[{dt = DateObject[]},
      Module[{day = DateValue[dt, "Day"], mon = DateValue[dt, "Month"],
              yr = DateValue[dt, "Year"] - 2000,
              sec = DateValue[dt, "Second"], min = DateValue[dt, "Minute"],
              hr = DateValue[dt, "Hour"]},
        sec = Round[sec];
        exec[h, cmdSetTime,
          {day, mon, yr, 0, sec, min, hr, 0}]
      ]
    ];
    writeVSFR[h, vsfrDeviceTime, {0, 0, 0, 0}];
    (* configuration: parse SpecFormatVersion *)
    conf = readRequest[h, vsConfiguration];
    If[ListQ[conf],
      (* config is cp1251 in Python; non-ASCII bytes only appear in
         translated strings.  We only care about ASCII keys here, so
         strip non-ASCII for the parse. *)
      lines = StringSplit[
        FromCharacterCode[Select[conf, # < 128 &]], "\n"];
      Do[
        If[StringStartsQ[line, "SpecFormatVersion"],
          With[{v = ToExpression[StringTrim[Last[StringSplit[line, "="]]]]},
            If[IntegerQ[v], $state[h]["FormatVersion"] = v]
          ]
        ], {line, lines}]];
  ];

(* ---- spectrum decode (mirrors decoders/spectrum.py) ---- *)

decodeCountsV0[bytes_List] :=
  Module[{n = Length[bytes], i = 0, out = Internal`Bag[]},
    While[i + 4 <= n,
      Internal`StuffBag[out,
        bytes[[i + 1]] + 256 bytes[[i + 2]] +
         65536 bytes[[i + 3]] + 16777216 bytes[[i + 4]]];
      i += 4];
    Internal`BagPart[out, All]
  ];

decodeCountsV1[bytes_List] :=
  Module[{n = Length[bytes], pos = 0, out = Internal`Bag[],
          last = 0, u16, cnt, vlen, v, k, p2, a, b, c},
    While[pos < n,
      {u16, pos} = readU16[bytes, pos];
      cnt = BitAnd[BitShiftRight[u16, 4], 16^^FFF];
      vlen = BitAnd[u16, 16^^F];
      Do[
        Switch[vlen,
          0, v = 0,
          1, {v, pos} = readU8[bytes, pos],
          2, Module[{d}, {d, pos} = readS8[bytes, pos]; v = last + d],
          3, Module[{d}, {d, pos} = readS16[bytes, pos]; v = last + d],
          4,
            (* 3-byte signed int = (c<<16)|(b<<8)|a, with c signed *)
            {a, pos} = readU8[bytes, pos];
            {b, pos} = readU8[bytes, pos];
            {c, pos} = readS8[bytes, pos];
            v = last + (BitShiftLeft[c, 16] + BitShiftLeft[b, 8] + a),
          5, Module[{d}, {d, pos} = readS32[bytes, pos]; v = last + d],
          _, Throw[Failure["BadVlen", <|"MessageTemplate" ->
                "Unsupported vlen `1` in spectrum v1 decode",
                "MessageParameters" -> {vlen}|>]]];
        last = v;
        Internal`StuffBag[out, v],
        {k, cnt}]
    ];
    Internal`BagPart[out, All]
  ];

decodeSpectrum[bytes_List, formatVersion_Integer] :=
  Catch @ Module[{ts, a0, a1, a2, p2, rest, counts},
    {ts, p2} = readU32[bytes, 0];
    {a0, p2} = readF32[bytes, p2];
    {a1, p2} = readF32[bytes, p2];
    {a2, p2} = readF32[bytes, p2];
    rest = bytes[[p2 + 1 ;;]];
    counts = If[formatVersion === 1,
       decodeCountsV1[rest],
       decodeCountsV0[rest]];
    If[FailureQ[counts], Return[counts]];
    <|"Duration" -> ts, "a0" -> a0, "a1" -> a1, "a2" -> a2,
      "Counts" -> counts|>
  ];

(* ---- public: read spectrum ---- *)

RadiaCodeNativeReadSpectrum[handle_Integer] :=
  If[!RadiaCodeNativeAvailableQ[],
    unavailable["RadiaCodeNativeReadSpectrum"],
    Module[{snBytes, sn, accBytes, dec, fmt},
      fmt = Lookup[Lookup[$state, handle, <||>], "FormatVersion", 0];
      snBytes = readRequest[handle, vsSerialNumber];
      sn = If[ListQ[snBytes], FromCharacterCode[snBytes, "ASCII"], ""];
      accBytes = readRequest[handle, vsSpecAccum];
      If[FailureQ[accBytes], Return[accBytes]];
      dec = decodeSpectrum[accBytes, fmt];
      If[FailureQ[dec], Return[dec]];
      <|
        "SerialNumber"     -> sn,
        "Calibration"      -> {dec["a0"], dec["a1"], dec["a2"]},
        "Counts"           -> Developer`ToPackedArray[dec["Counts"]],
        "NumberOfChannels" -> Length[dec["Counts"]],
        "Duration"         -> dec["Duration"]
      |>
    ]
  ];

(* ---- realtime decode (mirrors decoders/databuf.py) ---- *)

decodeDataBuf[bytes_List, baseTime_] :=
  Module[{n = Length[bytes], pos = 0, records = Internal`Bag[],
          seq, eid, gid, tsOff, dt, nextSeq = None,
          countRate, doseRate, countRateErr, doseRateErr, flags, rtFlags,
          count, duration, dose, temperature, chargeLevel,
          accX, accY, accZ, samplesNum, smplTimeMs, event, eventParam1,
          ev},
    While[pos + 7 <= n,
      {seq, pos} = readU8[bytes, pos];
      {eid, pos} = readU8[bytes, pos];
      {gid, pos} = readU8[bytes, pos];
      {tsOff, pos} = readS32[bytes, pos];
      dt = baseTime + tsOff * 0.01;
      If[nextSeq =!= None && nextSeq =!= seq, Break[]];
      nextSeq = Mod[seq + 1, 256];
      Which[
        eid == 0 && gid == 0,
          {countRate, pos}    = readF32[bytes, pos];
          {doseRate, pos}     = readF32[bytes, pos];
          {countRateErr, pos} = readU16[bytes, pos];
          {doseRateErr, pos}  = readU16[bytes, pos];
          {flags, pos}        = readU16[bytes, pos];
          {rtFlags, pos}      = readU8[bytes, pos];
          Internal`StuffBag[records,
            <|"Type" -> "RealTimeData", "Time" -> dt,
              "CountRate" -> countRate, "CountRateErr" -> countRateErr / 10.,
              "DoseRate" -> doseRate, "DoseRateErr" -> doseRateErr / 10.,
              "Flags" -> flags, "RealTimeFlags" -> rtFlags|>],
        eid == 0 && gid == 1,
          {countRate, pos} = readF32[bytes, pos];
          {doseRate, pos}  = readF32[bytes, pos];
          Internal`StuffBag[records,
            <|"Type" -> "RawData", "Time" -> dt,
              "CountRate" -> countRate, "DoseRate" -> doseRate|>],
        eid == 0 && gid == 2,
          {count, pos}        = readU32[bytes, pos];
          {countRate, pos}    = readF32[bytes, pos];
          {doseRate, pos}     = readF32[bytes, pos];
          {doseRateErr, pos}  = readU16[bytes, pos];
          {flags, pos}        = readU16[bytes, pos];
          Internal`StuffBag[records,
            <|"Type" -> "DoseRateDB", "Time" -> dt, "Count" -> count,
              "CountRate" -> countRate, "DoseRate" -> doseRate,
              "DoseRateErr" -> doseRateErr / 10., "Flags" -> flags|>],
        eid == 0 && gid == 3,
          {duration, pos}     = readU32[bytes, pos];
          {dose, pos}         = readF32[bytes, pos];
          {temperature, pos}  = readU16[bytes, pos];
          {chargeLevel, pos}  = readU16[bytes, pos];
          {flags, pos}        = readU16[bytes, pos];
          Internal`StuffBag[records,
            <|"Type" -> "RareData", "Time" -> dt, "Duration" -> duration,
              "Dose" -> dose, "Temperature" -> (temperature - 2000) / 100.,
              "ChargeLevel" -> chargeLevel / 100., "Flags" -> flags|>],
        eid == 0 && gid == 4,
          (* GRP_UserData: skip 4+4+4+2+2 = 16 bytes *)
          pos += 16,
        eid == 0 && gid == 5,
          pos += 16,
        eid == 0 && gid == 6,
          pos += 6,
        eid == 0 && gid == 7,
          {event, pos}        = readU8[bytes, pos];
          {eventParam1, pos}  = readU8[bytes, pos];
          {flags, pos}        = readU16[bytes, pos];
          Internal`StuffBag[records,
            <|"Type" -> "Event", "Time" -> dt, "Event" -> event,
              "EventParam1" -> eventParam1, "Flags" -> flags|>],
        eid == 0 && gid == 8,
          pos += 6,
        eid == 0 && gid == 9,
          pos += 6,
        eid == 1 && gid == 1,
          {samplesNum, pos} = readU16[bytes, pos];
          {smplTimeMs, pos} = readU32[bytes, pos];
          pos += 8 samplesNum,
        eid == 1 && gid == 2,
          {samplesNum, pos} = readU16[bytes, pos];
          {smplTimeMs, pos} = readU32[bytes, pos];
          pos += 16 samplesNum,
        eid == 1 && gid == 3,
          {samplesNum, pos} = readU16[bytes, pos];
          {smplTimeMs, pos} = readU32[bytes, pos];
          pos += 14 samplesNum,
        True,
          (* Unknown {eid, gid} pair: log so the user knows we're
             truncating, then bail out -- without the message it
             looked like the device just stopped emitting records. *)
          Message[RadiaCodeNativeReadRealtime::unknownTag, eid, gid];
          Break[]
      ]
    ];
    Internal`BagPart[records, All]
  ];

RadiaCodeNativeReadRealtime::unknownTag =
  "Unknown databuf record tag {`1`, `2`}; truncating remaining bytes. \
This usually means a newer device firmware introduced a record type \
the decoder doesn't know about; please report it.";

RadiaCodeNativeReadRealtime[handle_Integer] :=
  If[!RadiaCodeNativeAvailableQ[],
    unavailable["RadiaCodeNativeReadRealtime"],
    Module[{bytes, baseT, recs, rt},
      bytes = readRequest[handle, vsDataBuf];
      If[FailureQ[bytes], Return[bytes]];
      baseT = Lookup[Lookup[$state, handle, <||>], "BaseTime",
                      AbsoluteTime[]];
      recs = decodeDataBuf[bytes, baseT];
      rt = SelectFirst[recs, #["Type"] === "RealTimeData" &, Missing["NoData"]];
      rt
    ]
  ];

(* ---- reset ---- *)

RadiaCodeNativeReset[handle_Integer, what_String] :=
  If[!RadiaCodeNativeAvailableQ[],
    unavailable["RadiaCodeNativeReset"],
    Switch[what,
      "Spectrum",
        (* WR_VIRT_STRING with vsSpectrum and zero data *)
        Module[{p, retcode, p2},
          p = exec[handle, cmdWrVirtString,
            {Mod[vsSpectrum, 256], Mod[Quotient[vsSpectrum, 256], 256],
             Mod[Quotient[vsSpectrum, 65536], 256],
             Mod[Quotient[vsSpectrum, 16777216], 256],
             0, 0, 0, 0}];
          If[FailureQ[p], Return[p]];
          {retcode, p2} = readU32[p, 0];
          retcode == 1
        ],
      "Dose",
        writeVSFR[handle, vsfrDoseReset, {}],
      _,
        Failure["BadArgument",
          <|"MessageTemplate" ->
              "RadiaCodeNativeReset target must be \"Spectrum\" or \"Dose\""|>]
    ]
  ];

End[];
EndPackage[];