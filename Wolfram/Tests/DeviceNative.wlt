(* ::Package:: *)

(* Tests for RadiaCodeTools`DeviceNative`.

   Most tests don't require a physical device: the library load,
   availability check, and "no-device" code paths can all run in CI.
   Tests that DO require a device are guarded behind RadiaCodeNativeAvailableQ[]
   and a non-empty RadiaCodeNativeDevices[] list. *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* ----- package loaded ----- *)

vt[
  Length @ DownValues @ RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ
    >= 1,
  True,
  TestID -> "package-loaded"
];

(* ----- library either loaded or absent (both are valid states) ----- *)

vt[
  BooleanQ @ Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ[],
  True,
  TestID -> "available-returns-boolean"
];

(* ----- $RadiaCodeNativeLibrary is None or a path ----- *)

vt[
  With[{v = RadiaCodeTools`DeviceNative`$RadiaCodeNativeLibrary},
    v === None || (StringQ[v] && FileExistsQ[v])],
  True,
  TestID -> "library-path-valid"
];

(* ----- RadiaCodeNativeDevices returns a list (or Failure if not built) ----- *)

devs = Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeDevices[];
vt[
  ListQ[devs] || FailureQ[devs],
  True,
  TestID -> "devices-list-or-failure"
];

(* If the library *is* built, the list type must specifically be List
   (Failures only happen when the lib isn't loaded). *)
If[RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ[],
  vt[ListQ[devs], True, TestID -> "devices-list-when-available"];
  vt[AllTrue[devs, StringQ], True,
     TestID -> "devices-strings-when-available"];
];

(* ----- Reset rejects bad target ----- *)

If[RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ[],
  vt[
    FailureQ @ RadiaCodeTools`DeviceNative`RadiaCodeNativeReset[0, "Banana"],
    True,
    TestID -> "reset-bad-target-fails"
  ]
];

(* ----- Live device round-trip (only if a device is attached) ----- *)

If[RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ[] &&
   ListQ[devs] && Length[devs] >= 1,
  Module[{h, spec},
    h = Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeOpen[First[devs]];
    vt[IntegerQ[h] && h >= 0, True, TestID -> "open-handle"];
    If[IntegerQ[h] && h >= 0,
      spec = Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeReadSpectrum[h];
      vt[AssociationQ[spec], True, TestID -> "spectrum-association"];
      vt[
        AllTrue[{"SerialNumber", "Calibration", "Counts",
                 "NumberOfChannels", "Duration"}, KeyExistsQ[spec, #] &],
        True,
        TestID -> "spectrum-keys"
      ];
      vt[Length[spec["Counts"]] === spec["NumberOfChannels"], True,
         TestID -> "spectrum-channels-consistent"];
      vt[Length[spec["Calibration"]] === 3, True,
         TestID -> "spectrum-calibration-three-coeffs"];
      Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeClose[h]
    ]
  ]
];

(* ----- summary ----- *)
passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["DeviceNative.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
