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
