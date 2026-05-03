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
