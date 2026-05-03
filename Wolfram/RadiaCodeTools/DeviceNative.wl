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
