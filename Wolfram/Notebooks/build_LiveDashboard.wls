#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Generates LiveDashboard.nb in this folder.  Run from the repo root:

       wolframscript -file Wolfram/Notebooks/build_LiveDashboard.wls

   This script is the source of truth for the notebook; the .nb is a
   build artefact regenerated whenever the script changes. *)

outFile = FileNameJoin[{DirectoryName[$InputFileName], "LiveDashboard.nb"}];

(* ----- helper to make a code-input cell from a string ----- *)
codeCell[s_String] := Cell[s, "Input"];
textCell[s_String] := Cell[s, "Text"];
sectionCell[s_String] := Cell[s, "Section"];
subsectionCell[s_String] := Cell[s, "Subsection"];

nb = Notebook[{

  Cell["RadiaCode \[LongDash] live dashboard", "Title"],

  textCell[
    "Plug a RadiaCode detector in via USB, then evaluate the cells \
below in order (Shift\[Hyphen]Enter).  The dashboard cell is a \
Dynamic[]; once it appears, it will keep updating in place as fresh \
spectra and rate samples arrive."],

  textCell[
    "Prerequisites: Python 3 with the `radiacode` package installed \
(`pip install radiacode`).  No baud rate, no serial \
configuration\[LongDash]RadiaCode is a USB\[Hyphen]HID device, and the \
Python side handles the USB transport."],

  sectionCell["1. Load the package"],

  textCell[
    "This locates init.wl relative to the notebook so you can move \
the repo without editing the path."],

  codeCell[
    "Get[FileNameJoin[{NotebookDirectory[], \"..\", \
\"RadiaCodeTools\", \"init.wl\"}]]"],

  sectionCell["2. Diagnostics (run if anything fails)"],

  textCell[
    "Shows the python3 path the bridge will use, the repo root, and \
verifies that the radiacode Python package imports.  If python3 is \
\"python3\" with no leading slash, your front-end's PATH does not \
contain it; install the radiacode package into one of the searched \
locations or set RadiaCodeTools`Device`$RadiaCodePython to an \
absolute path."],

  codeCell[
    "{\n  \"Python\"   -> RadiaCodeTools`Device`$RadiaCodePython,\n  \
\"RepoRoot\" -> RadiaCodeTools`Device`$RadiaCodeRepoRoot,\n  \
\"radiacode importable\" -> (RunProcess[\n     \
{RadiaCodeTools`Device`$RadiaCodePython, \"-c\",\n      \"import radiacode; \
print(radiacode.__file__)\"}, \"ExitCode\"] === 0)\n} // Column"],

  sectionCell["3. Detect attached devices"],

  textCell[
    "Returns a list of RadiaCode serial numbers.  Empty list = either \
no device plugged in or the radiacode Python package is missing.  \
Failure[\[Ellipsis]] = the Python bridge errored\[LongDash]check the \
message."],

  codeCell["RadiaCodeTools`Device`RadiaCodeDevices[]"],

  sectionCell["4. (Option A) Single-shot spectrum acquisition"],

  textCell[
    "Reads the device's current cumulative spectrum and returns a \
RadiaCodeTools spectrum Association you can plot, fit, or export.  \
Pass \"AccumulationTime\" -> Quantity[n, \"Seconds\"] to integrate \
for n seconds before reading."],

  codeCell[
    "spec = RadiaCodeTools`Device`RadiaCodeAcquire[\n  \
\"AccumulationTime\" -> Quantity[30, \"Seconds\"]];"],

  codeCell[
    "RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[spec]"],

  codeCell[
    "RadiaCodeTools`SpectrumPlot`PeakChannels[spec, 5]"],

  sectionCell["5. (Option B) Live streaming dashboard"],

  textCell[
    "Starts an rcmultispg subprocess (the upstream Python tool) that \
polls the device repeatedly and emits one ndjson record per sample.  \
The Wolfram side tails that stream and feeds a Dynamic[] grid \
showing the live spectrum, count\[Hyphen]rate trace, and \
dose\[Hyphen]rate trace.  Adjust \"PollingInterval\" if you want \
spectra more or less often."],

  codeCell[
    "streamId = RadiaCodeTools`Device`RadiaCodeStream[\n  \
\"PollingInterval\" -> 5.0]"],

  textCell[
    "Evaluate the next cell to render the dashboard.  It will keep \
updating itself; you do not need to re\[Hyphen]evaluate."],

  codeCell["RadiaCodeTools`LiveViewer`RadiaCodeDashboard[streamId]"],

  sectionCell["6. Inspect the latest state programmatically"],

  textCell[
    "While the stream is running you can pull the current state at \
any time as a plain Association \[LongDash] handy for ad-hoc analysis \
or saving snapshots."],

  codeCell[
    "state = RadiaCodeTools`LiveViewer`RadiaCodeStreamState[streamId];\n\
Keys[state]"],

  codeCell[
    "(* example: count-rate quantiles over the live trace; rcmultispg \
emits null for transient samples so we filter to NumericQ first *)\n\
Quantile[\n  \
Cases[Lookup[#, \"CountRate\"] & /@ state[\"Realtime\"], _?NumericQ],\n  \
{0.05, 0.5, 0.95}]"],

  sectionCell["7. Stop the stream"],

  textCell[
    "Always close the stream when you are done.  This terminates the \
Python subprocess, removes the polling task, and deletes the \
per-stream temp file."],

  codeCell["RadiaCodeTools`LiveViewer`CloseRadiaCodeStream[streamId]"],

  sectionCell["8. (Optional) Reset device state"],

  textCell[
    "Destructive operations \[LongDash] only run if you really want \
to clear the device's history.  Each function returns True on \
success, Failure[\[Ellipsis]] otherwise."],

  codeCell["(* RadiaCodeTools`Device`RadiaCodeReset[\"Spectrum\"] *)"],

  codeCell["(* RadiaCodeTools`Device`RadiaCodeReset[\"Dose\"] *)"]
},
  WindowSize -> {1400, 900},
  WindowMargins -> {{Automatic, 0}, {Automatic, 0}},
  FrontEndVersion -> "14.",
  StyleDefinitions -> "Default.nb"];

Export[outFile, nb, "Notebook"];
Print["wrote ", outFile, " (", FileByteCount[outFile], " bytes)"];
