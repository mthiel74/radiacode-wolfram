(* ::Package:: *)

(* RadiaCodeTools`N42Convert`
   Convert RadiaCode XML spectra to ANSI N42 format. *)

BeginPackage["RadiaCodeTools`N42Convert`", {"RadiaCodeTools`Formats`"}];

ConvertRCToN42::usage =
  "ConvertRCToN42[infile, outfile] reads a RadiaCode XML spectrum and \
writes an ANSI N42 file.  Options:\n\
  \"Background\" -> file | None — pull background from a separate file\n\
  \"UUID\"       -> Automatic | uuid string\n\
  \"Overwrite\"  -> False | True\n\
ConvertRCToN42[dir] with \"Recursive\" -> True walks a directory tree, \
converting every *.xml in place to *.xml.n42.";

Begin["`Private`"];

mergeBackground[fgSpec_Association, bgSpec_Association] :=
  Module[{result = fgSpec},
    Which[
      (* prefer the explicit BG file's background layer *)
      AssociationQ[Lookup[bgSpec, "Background", None]],
        result["Background"] = bgSpec["Background"],
      (* otherwise its foreground layer *)
      KeyExistsQ[bgSpec, "Counts"] && Length[bgSpec["Counts"]] > 0,
        result["Background"] = <|
          "Calibration" -> bgSpec["Calibration"],
          "Counts"      -> bgSpec["Counts"],
          "Duration"    -> bgSpec["Duration"]|>
    ];
    result
  ];

Options[ConvertRCToN42] = {
  "Background" -> None,
  "UUID"       -> Automatic,
  "Overwrite"  -> False,
  "Recursive"  -> False
};

ConvertRCToN42[infile_String, outfile_String, OptionsPattern[]] :=
  Module[{bgArg, uuid, overwrite, fgSpec, bgSpec, finalSpec},
    bgArg     = OptionValue["Background"];
    uuid      = OptionValue["UUID"];
    overwrite = OptionValue["Overwrite"];

    If[FileExistsQ[outfile] && !overwrite,
      Return[Failure["Exists",
        <|"MessageTemplate" -> "Output file `1` exists; pass \"Overwrite\" -> True.",
          "MessageParameters" -> {outfile}|>]]];
    fgSpec = RadiaCodeTools`Formats`ImportRCSpectrum[infile];
    If[FailureQ[fgSpec] || fgSpec === $Failed, Return[fgSpec]];
    finalSpec = fgSpec;
    If[StringQ[bgArg] && FileExistsQ[bgArg],
      bgSpec = RadiaCodeTools`Formats`ImportRCSpectrum[bgArg];
      If[!FailureQ[bgSpec] && bgSpec =!= $Failed,
        finalSpec = mergeBackground[fgSpec, bgSpec]]];
    RadiaCodeTools`Formats`ExportN42[outfile, finalSpec, "UUID" -> uuid]
  ];

(* Auto-generated output name *)
ConvertRCToN42[infile_String, opts:OptionsPattern[]] :=
  ConvertRCToN42[infile, infile <> ".n42", opts];

(* Recursive directory mode *)
ConvertRCToN42[dir_String, "Recursive" -> True, opts:OptionsPattern[]] :=
  ConvertRCToN42[dir, opts, "Recursive" -> True];
ConvertRCToN42[dir_String, opts:OptionsPattern[]] /;
  TrueQ[OptionValue[ConvertRCToN42, {opts}, "Recursive"]] &&
  DirectoryQ[dir] :=
  Module[{xmls},
    xmls = FileNames["*.xml", dir, Infinity];
    Map[ConvertRCToN42[#, # <> ".n42",
                        FilterRules[{opts}, Except["Recursive"]]] &, xmls]
  ];

End[];
EndPackage[];
