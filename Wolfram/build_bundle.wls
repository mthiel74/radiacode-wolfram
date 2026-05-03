#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Concatenate every package file under RadiaCodeTools/ into a single
   self-contained file so users can install the toolkit by uploading
   one attachment instead of cloning the repo.

   Output: Wolfram/RadiaCodeTools/RadiaCodeToolsBundle.wl

   Usage from a fresh kernel (after downloading just the bundle):
       Get["/path/to/RadiaCodeToolsBundle.wl"]

   The bundle defines every public symbol from Formats.wl, Calibrate.wl,
   SpectrumPlot.wl, N42Convert.wl, TrackPlot.wl, Deadtime.wl,
   RecursiveDeadtime.wl, TrackSanitize.wl, TrackEdit.wl,
   SpectrogramEnergy.wl, SpectroPlot.wl, RCSpgFromJson.wl,
   RCTrkFromJson.wl, N42Validate.wl, Spectroscopy.wl, LiveViewer.wl,
   and Device.wl / DeviceNative.wl.

   Device.wl and DeviceNative.wl still need their respective runtime
   dependencies to actually talk to a detector (Python+radiacode, or
   libusb plus the compiled .dylib).  Without those, calling their
   functions returns a Failure[]; the rest of the toolkit (parsers,
   plots, calibration, isotope ID) works offline. *)

repoDir = DirectoryName[$InputFileName];
pkgDir  = FileNameJoin[{repoDir, "RadiaCodeTools"}];

(* Same order as init.wl. *)
files = {
  "Formats.wl",
  "Calibrate.wl",
  "SpectrumPlot.wl",
  "N42Convert.wl",
  "TrackPlot.wl",
  "Deadtime.wl",
  "RecursiveDeadtime.wl",
  "TrackSanitize.wl",
  "TrackEdit.wl",
  "SpectrogramEnergy.wl",
  "SpectroPlot.wl",
  "RCSpgFromJson.wl",
  "RCTrkFromJson.wl",
  "N42Validate.wl",
  "Spectroscopy.wl",
  "LiveViewer.wl",
  "Device.wl",
  "DeviceNative.wl"
};

readSection[name_String] :=
  Module[{path = FileNameJoin[{pkgDir, name}], text},
    If[!FileExistsQ[path],
       Print["WARNING: bundle source missing, skipping: ", path];
       Return[""]];
    text = Import[path, "Text"];
    StringJoin[
      "\n(* ", StringRepeat["=", 70], "\n",
      "   ", name, "\n",
      "   ", StringRepeat["=", 70], " *)\n\n",
      text]
  ];

header = "(* ::Package:: *)

(* RadiaCodeToolsBundle.wl — single-file distribution

   This is `Wolfram/build_bundle.wls`'s output: every package file
   under `Wolfram/RadiaCodeTools/` concatenated in load order.  It
   provides the same public API as Get[\"init.wl\"] from a clone of
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
     - Fit channel→energy calibration polynomials
     - Plot spectra (ListLogPlot styled), tracks (GeoListPlot),
       spectrograms (ArrayPlot)
     - Identify isotopes by matching photopeaks against a built-in
       gamma-line library
     - Convert between formats (RC XML ↔ N42, ndjson → .rcspg/.rctrk)

   What needs additional setup:
     - Live device acquisition via Device.wl: requires Python 3 +
       `pip install radiacode` plus a clone of the repo (for the
       upstream rcmultispg.py / radiacode_poll.py scripts)
     - Live device acquisition via DeviceNative.wl: requires libusb
       (`brew install libusb` on macOS) and the compiled
       radiacode_link.dylib from `Wolfram/RadiaCodeTools/clib/`
*)

";

body = StringJoin[readSection /@ files];
out  = header <> body;

outFile = FileNameJoin[{pkgDir, "RadiaCodeToolsBundle.wl"}];
Export[outFile, out, "Text"];
Print["Wrote ", outFile,
      " (", FileByteCount[outFile], " bytes, ",
      Length[StringSplit[out, "\n"]], " lines, ",
      Length[Select[files, FileExistsQ[FileNameJoin[{pkgDir, #}]] &]],
      " sections)"];

(* Also drop a copy next to the community notebook so users of that
   notebook can `Get[NotebookDirectory[]<>"RadiaCodeToolsBundle.wl"]`
   without rummaging for the master copy. *)
communityCopy = FileNameJoin[{repoDir, "community", "RadiaCodeToolsBundle.wl"}];
If[DirectoryQ[FileNameDrop[communityCopy, -1]],
  Export[communityCopy, out, "Text"];
  Print["  also copied to ", communityCopy]];

(* And a small subset of sample data files so the post is
   self-contained when uploaded as one bundle to Community. *)
sampleFiles = {"data_am241.xml", "data_th232_plus_background.xml",
                "trinitite.xml", "K40.rcspg"};
testsData   = FileNameJoin[{repoDir, "..", "tests", "data"}];
communityData = FileNameJoin[{repoDir, "community", "data"}];
If[DirectoryQ[testsData],
  If[!DirectoryQ[communityData], CreateDirectory[communityData]];
  Scan[
    Function[f,
      With[{src = FileNameJoin[{testsData, f}],
            dst = FileNameJoin[{communityData, f}]},
        If[FileExistsQ[src], CopyFile[src, dst, OverwriteTarget -> True]]]],
    sampleFiles];
  Print["  copied ", Length[sampleFiles],
        " sample files to ", communityData]];
