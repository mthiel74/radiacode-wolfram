(* ::Package:: *)

(* Tests for RadiaCodeTools`Calibrate`. *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

(* Multi-source calibration data lifted from the Python template
   (calibrate.py -W). README.md states this should yield coefficients
   approximately {-7.27, 2.44, 3.77e-4} with R^2 ~ 0.99988. *)

multiSource = {
  {9, 26}, {21, 60},
  {28, 80}, {59, 166}, {109, 303}, {128, 356},
  {14, 40}, {44, 122}, {88, 245}, {124, 344}, {395, 1098}, {507, 1408},
  {526, 1461},
  {106, 295}, {126, 352}, {219, 609}, {403, 1120}, {635, 1765}, {793, 2204},
  {184, 511}, {459, 1275},
  {121, 338}, {210, 583}, {328, 911}, {572, 1588}, {941, 2614}
};

fit = RadiaCodeTools`Calibrate`FitCalibration[multiSource];

vt[Length[fit["Coefficients"]], 3, TestID -> "calib-3-coeffs"];

vt[fit["Order"], 2, TestID -> "calib-order-default"];

(* The Python README quotes coefficients ~{-7.27, 2.44, 3.77e-4}, but
   that fit predicts e.g. (channel 9 -> 14.7 keV) while the actual value
   is 26 keV.  Our LeastSquares fit is strictly better.  Test the fit
   QUALITY rather than match the stale README numbers. *)

vt[
  fit["RSquared"] > 0.9998,
  True,
  TestID -> "calib-rsquared-high"
];

(* Predicted vs. actual: max residual under 5 keV across the full data range *)
vt[
  Module[{c, e, predicted, residuals},
    c = multiSource[[All, 1]];
    e = multiSource[[All, 2]];
    predicted = RadiaCodeTools`Formats`applyCalibration[fit["Coefficients"], c];
    residuals = e - predicted;
    Max[Abs[residuals]]
  ],
  5.,
  SameTest -> (#1 <= #2 &),
  TestID -> "calib-max-residual-bounded"
];

vt[
  fit["Range"],
  {{9, 26}, {941, 2614}},
  TestID -> "calib-range"
];

(* Higher-order option *)
fit3 = RadiaCodeTools`Calibrate`FitCalibration[multiSource, "Order" -> 3];
vt[Length[fit3["Coefficients"]], 4, TestID -> "calib-cubic"];

(* ZeroStart adds a (0,0) point if absent *)
fitZ = RadiaCodeTools`Calibrate`FitCalibration[multiSource, "ZeroStart" -> True];
vt[fitZ["NumberOfPoints"], Length[multiSource] + 1, TestID -> "calib-zero-start-adds"];
vt[First[fitZ["Points"]], {0, 0}, TestID -> "calib-zero-start-first-point"];

(* Round-trip via JSON template ----------------------------------------- *)
tmp = CreateFile[];
RadiaCodeTools`Calibrate`WriteCalibrationTemplate[tmp];
vt[FileExistsQ[tmp], True, TestID -> "template-written"];
fitFromFile = RadiaCodeTools`Calibrate`ImportAndFitCalibration[tmp];
DeleteFile[tmp];
vt[
  fitFromFile["Coefficients"],
  fit["Coefficients"],
  SameTest -> (Norm[#1 - #2] < 10^-6 &),
  TestID -> "template-roundtrip-coefficients"
];

(* applyCalibration uses the fit coefficients sensibly *)
vt[
  RadiaCodeTools`Formats`applyCalibration[fit["Coefficients"], 21],
  60.,
  SameTest -> (Abs[#1 - #2] < 1.5 &),  (* tolerance: typical fit residual *)
  TestID -> "calib-applies-to-am241"
];

(* CalibrationSummary returns a string with the coefficients in it *)
summary = RadiaCodeTools`Calibrate`CalibrationSummary[fit];
vt[StringQ[summary], True, TestID -> "summary-is-string"];
vt[StringContainsQ[summary, "x^0 .. x^2"], True, TestID -> "summary-has-coefs"];
vt[StringContainsQ[summary, "data range:"], True, TestID -> "summary-has-range"];

(* ----- summary ----- *)

passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["Calibrate.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[Function[r, If[r["Outcome"] =!= "Success",
        Print["  ", r["TestID"], ": got ", Short[r["ActualOutput"], 5],
              " expected ", Short[r["ExpectedOutput"], 5]]]], results];
  Exit[1]];
