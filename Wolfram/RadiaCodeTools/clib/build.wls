#!/usr/bin/env wolframscript
(* clib/build.wls — compile radiacode_link.c into a Wolfram LibraryLink
   shared library.

   Requirements:
     * libusb-1.0  (brew install libusb)
     * A compiler that ships with macOS (Apple clang) — CCompilerDriver
       picks it up automatically.

   Output:
     <this dir>/radiacode_link.<dylib|so|dll>

   Run:
     wolframscript -file build.wls
*)

Needs["CCompilerDriver`"];

here = DirectoryName[$InputFileName /. "" :> NotebookFileName[]];
src  = FileNameJoin[{here, "radiacode_link.c"}];

(* Locate libusb headers/libs.  Try Homebrew first, fall back to system. *)
libusbPrefix = SelectFirst[
  {"/opt/homebrew/opt/libusb",
   "/usr/local/opt/libusb",
   "/opt/homebrew",
   "/usr/local",
   "/usr"},
  DirectoryQ[FileNameJoin[{#, "include", "libusb-1.0"}]] &,
  None];

If[libusbPrefix === None,
  Print["ERROR: cannot find libusb-1.0 headers."];
  Print["       Install with `brew install libusb`."];
  Exit[1]];

includeDir = FileNameJoin[{libusbPrefix, "include"}];
libDir     = FileNameJoin[{libusbPrefix, "lib"}];

Print["[build] libusb prefix: ", libusbPrefix];
Print["[build] source:        ", src];

result = CreateLibrary[
  {src},
  "radiacode_link",
  "TargetDirectory" -> here,
  "IncludeDirectories" -> {includeDir},
  "LibraryDirectories" -> {libDir},
  "Libraries" -> {"usb-1.0"},
  "CompileOptions" -> "-O2 -std=c99 -Wall -Wextra",
  "ShellOutputFunction" -> Print,
  "ShellCommandFunction" -> Print
];

If[StringQ[result] && FileExistsQ[result],
  Print["[build] OK -> ", result];
  Exit[0],
  Print["[build] FAILED. CreateLibrary returned: ", result];
  Exit[1]
];
