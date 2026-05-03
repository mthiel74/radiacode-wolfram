(* ::Package:: *)

(* Tests for RadiaCodeTools`Spectroscopy`.  Run with:

     wolframscript -file Tests/Spectroscopy.wlt
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                  "RadiaCodeTools", "init.wl"}]];

dataDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "..",
                         "tests", "data"}];

results = {};
vt[expr_, expected_, opts___] :=
  AppendTo[results, VerificationTest[expr, expected, opts]];

amSpec   = RadiaCodeTools`Formats`ImportRCSpectrum[
  FileNameJoin[{dataDir, "data_am241.xml"}]];
thSpec   = RadiaCodeTools`Formats`ImportRCSpectrum[
  FileNameJoin[{dataDir, "data_th232_plus_background.xml"}]];
trinSpec = RadiaCodeTools`Formats`ImportRCSpectrum[
  FileNameJoin[{dataDir, "trinitite.xml"}]];

(* ----- IsotopeLibrary ----- *)

vt[
  AssociationQ @ RadiaCodeTools`Spectroscopy`IsotopeLibrary[],
  True,
  TestID -> "lib-is-association"
];

vt[
  KeyMemberQ[RadiaCodeTools`Spectroscopy`IsotopeLibrary[], "Cs-137"],
  True,
  TestID -> "lib-has-cs137"
];

vt[
  KeyMemberQ[RadiaCodeTools`Spectroscopy`IsotopeLibrary[], "Am-241"],
  True,
  TestID -> "lib-has-am241"
];

vt[
  KeyMemberQ[RadiaCodeTools`Spectroscopy`IsotopeLibrary[], "K-40"],
  True,
  TestID -> "lib-has-k40"
];

(* The Cs-137 line should be at 661.66 keV. *)
vt[
  Module[{lines = RadiaCodeTools`Spectroscopy`IsotopeLibrary[]["Cs-137"]},
    Abs[First[lines]["Energy"] - 661.66] < 0.5],
  True,
  TestID -> "lib-cs137-line-energy"
];

(* ----- FindPhotopeaks ----- *)

amPeaks = RadiaCodeTools`Spectroscopy`FindPhotopeaks[amSpec];
vt[Length[amPeaks] >= 1, True, TestID -> "fp-am-has-peaks"];

(* The strongest peak in Am-241 must be at \[Tilde]60 keV.  Am-241's
   single-line library entry should match easily within 8 keV. *)
vt[
  50. <= First[amPeaks]["Energy"] <= 80.,
  True,
  TestID -> "fp-am-strongest-near-60"
];

vt[
  AssociationQ[First[amPeaks]] &&
   And @@ (KeyExistsQ[First[amPeaks], #] & /@
            {"Channel", "Energy", "Counts", "Prominence"}),
  True,
  TestID -> "fp-record-shape"
];

(* ----- IdentifyIsotopes ----- *)

amIds = RadiaCodeTools`Spectroscopy`IdentifyIsotopes[amSpec];
amIdRows = Normal[amIds];

(* Am-241 must be the top candidate from data_am241.xml.  This is the
   canonical "phone-app feature" smoke test. *)
vt[
  First[amIdRows]["Isotope"],
  "Am-241",
  TestID -> "id-am241-top"
];

(* Score for the matched library line must be 1.0 (one of one). *)
vt[
  First[amIdRows]["Score"],
  1.,
  SameTest -> (Abs[#1 - #2] < 0.01 &),
  TestID -> "id-am241-score-1"
];

(* Identification result is a Dataset whose rows are Associations. *)
vt[Head[amIds], Dataset, TestID -> "id-returns-dataset"];

vt[
  AssociationQ @ First[amIdRows] &&
   And @@ (KeyExistsQ[First[amIdRows], #] & /@
            {"Isotope", "Matched", "TotalLines", "Score",
             "PeakWeight", "MatchedLines"}),
  True,
  TestID -> "id-record-shape"
];

(* The Th-232 file has a foreground rich in Pb-212, Ac-228, Bi-214,
   etc.  Identification should return at least one of them in the
   first three rows. *)
thIds = RadiaCodeTools`Spectroscopy`IdentifyIsotopes[thSpec];
thIdRows = Normal[thIds];
thTopThree = Take[Lookup[#, "Isotope"] & /@ thIdRows, UpTo[3]];
vt[
  IntersectingQ[
    thTopThree,
    {"Pb-212", "Ac-228", "Bi-214", "Tl-208", "Pb-214"}],
  True,
  TestID -> "id-th232-natural-series"
];

(* Trinitite contains Am-241 and Cs-137 (from the Trinity device's
   plutonium and from atmospheric fallout); both should appear in the
   top results. *)
trinIds = RadiaCodeTools`Spectroscopy`IdentifyIsotopes[trinSpec];
trinNames = Lookup[#, "Isotope"] & /@ Normal[trinIds];
vt[MemberQ[trinNames, "Am-241"], True, TestID -> "id-trin-has-am"];
vt[MemberQ[trinNames, "Cs-137"], True, TestID -> "id-trin-has-cs"];

(* ----- FitGaussianPeak ----- *)

amFit = RadiaCodeTools`Spectroscopy`FitGaussianPeak[amSpec, 60.];
vt[AssociationQ[amFit], True, TestID -> "fit-returns-association"];

vt[
  And @@ (KeyExistsQ[amFit, #] & /@
            {"Mean", "Sigma", "FWHM", "Amplitude", "Background",
             "Slope", "FitWindow", "FitObject"}),
  True,
  TestID -> "fit-record-shape"
];

(* Centroid must be inside the requested window. *)
vt[50. <= amFit["Mean"] <= 80., True, TestID -> "fit-am-centroid"];

(* Sigma > 0 *)
vt[amFit["Sigma"] > 0., True, TestID -> "fit-am-sigma-positive"];

(* ----- EnergyResolution ----- *)

amRes = RadiaCodeTools`Spectroscopy`EnergyResolution[amSpec, 60.];
vt[AssociationQ[amRes], True, TestID -> "res-am-association"];

vt[
  And @@ (KeyExistsQ[amRes, #] & /@
            {"Centroid", "FWHM", "Resolution", "ResolutionPercent"}),
  True,
  TestID -> "res-record-shape"
];

(* The 60-keV Am-241 photopeak in this device's data is broad: the
   user-spec acceptance is "a reasonable double-digit percent".  The
   physical resolution of CsI(Tl) at 60 keV is dominated by Poisson
   light statistics, the L X-ray escape shoulder and the polynomial
   calibration's accuracy near the low end of the range, and 10\[Dash]40 %
   FWHM/E is a sane band. *)
vt[
  10. <= amRes["ResolutionPercent"] <= 40.,
  True,
  TestID -> "res-am-double-digit-percent"
];

(* Sanity: at higher energies the resolution should improve.  The
   Th-232 sample has many strong gamma lines; pick one in the
   500-700 keV band and require <15 % FWHM/E (CsI typical). *)
thRes = RadiaCodeTools`Spectroscopy`EnergyResolution[thSpec, 583.];
vt[AssociationQ[thRes], True, TestID -> "res-th-association"];
vt[
  thRes["ResolutionPercent"] < 15.,
  True,
  TestID -> "res-th-583-under-15-percent"
];

(* ----- summary ----- *)

passed = Count[results, _?(#["Outcome"] === "Success" &)];
failed = Count[results, _?(#["Outcome"] =!= "Success" &)];
Print["Spectroscopy.wlt: ", passed, "/", passed + failed, " passed"];
If[failed > 0,
  Print["FAILURES:"];
  Scan[
    Function[r,
      If[r["Outcome"] =!= "Success",
         Print["  ", r["TestID"], ": got ",
               Short[r["ActualOutput"], 5],
               " expected ", Short[r["ExpectedOutput"], 5]]]],
    results];
  Exit[1]];
