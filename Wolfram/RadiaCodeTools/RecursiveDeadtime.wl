(* ::Package:: *)

(* RadiaCodeTools`RecursiveDeadtime`
   Walk a directory looking for *_a.xml / *_b.xml / *_a+*_b.xml triplets
   and run ComputeDeadtimeFromFiles on each. *)

BeginPackage["RadiaCodeTools`RecursiveDeadtime`",
  {"RadiaCodeTools`Formats`", "RadiaCodeTools`Deadtime`"}];

ScanDeadtime::usage =
  "ScanDeadtime[dir] walks a directory tree and applies the Knoll \
two-source method to each subdirectory whose XML filenames match the \
*_a.xml, *_b.xml, *_a+*_b.xml convention.  Returns a Dataset, one row \
per matched triplet.  Optional second argument is a background XML file.";

Begin["`Private`"];

processTriplet[dir_String, files_List, rateBg_:0.] :=
  Module[{aFile, bFile, abFile, source, dt},
    aFile  = SelectFirst[files,
      StringMatchQ[#, ___ ~~ "_a.xml"] && !StringContainsQ[#, "+"] &, None];
    bFile  = SelectFirst[files,
      StringMatchQ[#, ___ ~~ "_b.xml"] && !StringContainsQ[#, "+"] &, None];
    abFile = SelectFirst[files,
      StringContainsQ[#, "_a+"] && StringEndsQ[#, "_b.xml"] &, None];
    If[aFile === None || bFile === None || abFile === None,
       Return[Missing["IncompleteTriplet", dir]]];
    dt = RadiaCodeTools`Deadtime`ComputeDeadtimeFromFiles[
           FileNameJoin[{dir, aFile}],
           FileNameJoin[{dir, bFile}],
           FileNameJoin[{dir, abFile}]];
    If[FailureQ[dt], Return[dt]];
    (* Override the BG rate post-hoc; recompute deadtime with bg. *)
    If[rateBg > 0,
      dt = RadiaCodeTools`Deadtime`ComputeDeadtime[
             dt["A"], dt["B"], dt["AB"], rateBg]];
    <|
      "Directory" -> dir,
      "A"         -> aFile,
      "B"         -> bFile,
      "AB"        -> abFile,
      "RateA"     -> dt["A"],
      "RateB"     -> dt["B"],
      "RateAB"    -> dt["AB"],
      "RateBG"    -> dt["BG"],
      "TauUs"     -> dt["TauMicroseconds"],
      "LossFraction" -> dt["LossFraction"],
      "Saturated" -> dt["Saturated"]
    |>
  ];

ScanDeadtime[dir_String] := ScanDeadtime[dir, None];

ScanDeadtime[dir_String, bgFile_] :=
  Module[{rateBg = 0., subdirs, results},
    If[StringQ[bgFile] && FileExistsQ[bgFile],
      rateBg = RadiaCodeTools`Deadtime`CountRateOfSpectrum[
                 RadiaCodeTools`Formats`ImportRCSpectrum[bgFile]]];
    subdirs = Select[
      Prepend[FileNames[All, dir, Infinity], dir],
      DirectoryQ];
    results = Function[d,
      With[{xmls = FileNameTake /@ FileNames["*.xml", d]},
        If[Length[xmls] === 3, processTriplet[d, xmls, rateBg], Nothing]]
      ] /@ subdirs;
    results = DeleteCases[results, Nothing | _Missing | _?FailureQ];
    Dataset[results]
  ];

End[];
EndPackage[];
