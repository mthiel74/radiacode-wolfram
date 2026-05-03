(* ::Package:: *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- File-tail mode against an existing ndjson sample ----- *)

xrayLog = FileNameJoin[{dataDir, "xray.ndjson"}];
id = RadiaCodeTools`LiveViewer`PlaybackNDJson[xrayLog,
       "PollInterval" -> 60.0];   (* slow poll: we'll drive it manually *)

vt[StringQ[id], True, TestID -> "open-returns-id"];

(* Force an immediate poll *)
RadiaCodeTools`LiveViewer`PollRadiaCodeStream[id];

state = RadiaCodeTools`LiveViewer`RadiaCodeStreamState[id];
vt[AssociationQ[state], True, TestID -> "state-is-association"];

vt[state["RecordCount"] > 0, True, TestID -> "records-ingested"];

(* xray.ndjson contains spectrum records — Spectrum should be populated *)
vt[Length[state["Spectrum"]["Counts"]] === 1024, True,
   TestID -> "spectrum-1024-channels"];

vt[state["Spectrum"]["SerialNumber"], "RC-103-000070",
   TestID -> "spectrum-serial"];

(* ----- Dashboard returns a Dynamic[] ----- *)

dash = RadiaCodeTools`LiveViewer`RadiaCodeDashboard[id];
vt[Head[dash], Dynamic, TestID -> "dashboard-is-dynamic"];

(* ----- ListStreams reflects open streams ----- *)
vt[MemberQ[RadiaCodeTools`LiveViewer`RadiaCodeListStreams[], id],
   True,
   TestID -> "listed-while-open"];

(* ----- Cleanup ----- *)

RadiaCodeTools`LiveViewer`CloseRadiaCodeStream[id];
state2 = RadiaCodeTools`LiveViewer`RadiaCodeStreamState[id];
vt[state2["Status"], "closed", TestID -> "status-closed"];

(* The user-supplied file should NOT be deleted on close *)
vt[FileExistsQ[xrayLog], True, TestID -> "user-file-preserved"];

(* ----- Subprocess mode (smoke test only — uses /bin/sh echo so no
        device required).  We pipe two ndjson lines into the buffer. ----- *)

fakeJson1 = "{\"timestamp\": 1.0, \"serial_number\": \"RC-FAKE\", \"count_rate\": 1.5, \"dose_rate\": 0.1, \"count\": 1, \"dose\": 0.0, \"charge_level\": 100, \"temperature\": 21.0, \"duration\": 1.0}";
fakeJson2 = "{\"timestamp\": 2.0, \"serial_number\": \"RC-FAKE\", \"count_rate\": 1.6, \"dose_rate\": 0.11, \"count\": 2, \"dose\": 0.0, \"charge_level\": 100, \"temperature\": 21.0, \"duration\": 1.0}";

id2 = RadiaCodeTools`LiveViewer`OpenRadiaCodeStream[
        {"printf", fakeJson1 <> "\n" <> fakeJson2 <> "\n"},
        "PollInterval" -> 0.3];
vt[StringQ[id2], True, TestID -> "subprocess-id"];

(* Wait briefly for the subprocess to write *)
Pause[1.0];
RadiaCodeTools`LiveViewer`PollRadiaCodeStream[id2];
state3 = RadiaCodeTools`LiveViewer`RadiaCodeStreamState[id2];

vt[state3["RecordCount"] >= 2, True, TestID -> "subprocess-records"];
vt[Length[state3["Realtime"]] >= 2, True, TestID -> "subprocess-realtime-history"];

RadiaCodeTools`LiveViewer`CloseRadiaCodeStream[id2];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["LiveViewer.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
