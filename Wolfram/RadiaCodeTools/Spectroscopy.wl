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
