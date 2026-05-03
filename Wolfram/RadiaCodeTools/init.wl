(* ::Package:: *)

(* RadiaCodeTools/init.wl
   Top-level loader for the Wolfram port of radiacode-tools.
   Loading this file makes every sub-package available.

   Usage:
     Get["/path/to/Wolfram/RadiaCodeTools/init.wl"]
*)

BeginPackage["RadiaCodeTools`"];

RadiaCodeTools::version = "RadiaCodeTools Wolfram port, loaded from `1`.";

EndPackage[];

(* The directory containing this file. *)
RadiaCodeTools`$PackageDirectory =
    DirectoryName[$InputFileName /. "" :> NotebookFileName[]];

Module[{dir = RadiaCodeTools`$PackageDirectory, files},
  files = {"Formats.wl", "Calibrate.wl", "SpectrumPlot.wl", "N42Convert.wl",
           "TrackPlot.wl", "Deadtime.wl", "RecursiveDeadtime.wl",
           "TrackSanitize.wl", "TrackEdit.wl",
           "SpectrogramEnergy.wl", "SpectroPlot.wl",
           "RCSpgFromJson.wl", "RCTrkFromJson.wl", "N42Validate.wl",
           "Spectroscopy.wl",
           "LiveViewer.wl", "Device.wl",
           (* DeviceNative is optional: only loaded if the C library is
              built (clib/radiacode_link.dylib).  See clib/README.md. *)
           "DeviceNative.wl"};
  Scan[
    Function[f,
      With[{path = FileNameJoin[{dir, f}]},
        If[FileExistsQ[path], Get[path]]
      ]
    ],
    files
  ];
];

Message[RadiaCodeTools::version, RadiaCodeTools`$PackageDirectory];
