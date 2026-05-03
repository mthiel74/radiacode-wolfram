(* ::Package:: *)

(* RadiaCodeTools`N42Validate`
   Best-effort structural validation of ANSI N42 files.

   Wolfram doesn't ship a full XSD validator, so we run two layers:
     1. A built-in structural check (root tag, required children,
        non-empty channel data, calibration sanity).
     2. If `xmllint` is installed, optionally call it with an XSD path
        for full schema validation.
*)

BeginPackage["RadiaCodeTools`N42Validate`",
  {"RadiaCodeTools`Formats`"}];

ValidateN42::usage =
  "ValidateN42[file] runs a structural check on an N42 file.  Returns \
an Association with keys Valid (Boolean), Issues (list of strings), and \
File. Pass \"Schema\" -> path to an n42.xsd to additionally invoke \
xmllint for full XSD validation.";

ValidateN42Recursive::usage =
  "ValidateN42Recursive[dir] walks a directory tree and validates every \
*.n42 file. Returns a Dataset of validation results.";

Begin["`Private`"];

structuralCheck[file_String] :=
  Module[{xml, root, issues = {}, ns = "http://physics.nist.gov/N42/2011/N42",
          rootTag, rii, rdi, ec, meas, spec, chData, calStr, calNums,
          countStr, counts},
    xml = Quiet @ Import[file, "XML"];
    If[xml === $Failed,
      Return[<|"Valid" -> False, "File" -> file,
                "Issues" -> {"could not parse XML"}|>]];
    root = FirstCase[xml,
      XMLElement[{ns, "RadInstrumentData"} | "RadInstrumentData", _, _],
      Missing[], Infinity];
    If[Head[root] =!= XMLElement,
      AppendTo[issues, "no <RadInstrumentData> root element"];
      Return[<|"Valid" -> False, "File" -> file, "Issues" -> issues|>]];

    rii  = FirstCase[root, XMLElement[{ns, "RadInstrumentInformation"} | "RadInstrumentInformation", _, _], None, Infinity];
    If[rii === None, AppendTo[issues, "missing <RadInstrumentInformation>"]];
    rdi  = FirstCase[root, XMLElement[{ns, "RadDetectorInformation"} | "RadDetectorInformation", _, _], None, Infinity];
    If[rdi === None, AppendTo[issues, "missing <RadDetectorInformation>"]];
    ec   = FirstCase[root, XMLElement[{ns, "EnergyCalibration"} | "EnergyCalibration", _, _], None, Infinity];
    If[ec === None, AppendTo[issues, "missing <EnergyCalibration>"]];
    meas = FirstCase[root, XMLElement[{ns, "RadMeasurement"} | "RadMeasurement", _, _], None, Infinity];
    If[meas === None, AppendTo[issues, "missing <RadMeasurement>"]];

    If[ec =!= None,
      calStr = StringTrim @ FirstCase[ec,
        XMLElement[{ns, "CoefficientValues"} | "CoefficientValues", _, ch_] :>
          StringJoin[Cases[ch, _String]],
        "", Infinity];
      calNums = ToExpression /@ StringSplit[calStr];
      If[!ListQ[calNums] || Length[calNums] < 2 ||
         !AllTrue[calNums, NumericQ],
        AppendTo[issues, "calibration coefficients missing or non-numeric"]]
    ];

    If[meas =!= None,
      spec = FirstCase[meas,
        XMLElement[{ns, "Spectrum"} | "Spectrum", _, _], None, Infinity];
      If[spec === None,
        AppendTo[issues, "<RadMeasurement> has no <Spectrum>"],
        chData = FirstCase[spec,
          XMLElement[{ns, "ChannelData"} | "ChannelData", _, ch_] :>
            StringJoin[Cases[ch, _String]],
          "", Infinity];
        countStr = StringTrim[chData];
        If[countStr === "",
          AppendTo[issues, "<ChannelData> empty"],
          counts = ToExpression /@ StringSplit[countStr];
          If[!ListQ[counts] || !AllTrue[counts, IntegerQ],
            AppendTo[issues, "channel counts non-integer"]]]
      ]
    ];

    <|"Valid" -> issues === {}, "File" -> file, "Issues" -> issues|>
  ];

xmllintCheck[file_String, xsd_String] :=
  Module[{out, success, lint},
    lint = "xmllint";
    out = RunProcess[{lint, "--noout", "--schema", xsd, file},
                     ProcessEnvironment -> <||>];
    success = (out["ExitCode"] === 0);
    <|"Valid" -> success,
      "Output" -> StringTrim[out["StandardError"]]|>
  ];

Options[ValidateN42] = {"Schema" -> None};

ValidateN42[file_String, OptionsPattern[]] :=
  Module[{result, schema, lintResult},
    result = structuralCheck[file];
    schema = OptionValue["Schema"];
    If[StringQ[schema] && FileExistsQ[schema] &&
       FileType["xmllint" /. {"xmllint" -> Quiet @ FindFile["xmllint"]}] =!= File,
      (* fall through if xmllint not on PATH *)
      Null];
    If[StringQ[schema] && FileExistsQ[schema],
      lintResult = Quiet @ Check[xmllintCheck[file, schema],
                                  <|"Valid" -> "unavailable"|>];
      result = <|result,
                  "XmllintValid"  -> lintResult["Valid"],
                  "XmllintOutput" -> Lookup[lintResult, "Output", ""]|>];
    result
  ];

ValidateN42Recursive[dir_String, opts:OptionsPattern[ValidateN42]] :=
  Module[{files},
    files = FileNames["*.n42", dir, Infinity];
    Dataset[ValidateN42[#, opts] & /@ files]
  ];

End[];
EndPackage[];
