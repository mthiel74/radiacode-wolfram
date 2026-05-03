#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Generates RadiaCodeSpectroscopy_Update.nb in this folder.
   Run from the repo root:

       wolframscript -file Wolfram/community/build_RadiaCodeSpectroscopy_Update.wls

   The post is the follow-up amendment to the original
   RadiaCodeSpectroscopy.nb post: it adds the pure-Wolfram libusb
   live-acquisition path and notes one missing data file.
*)

outFile = FileNameJoin[{DirectoryName[$InputFileName],
                         "RadiaCodeSpectroscopy_Update.nb"}];

(* ----- helpers ----- *)
titleCell[s_String]      := Cell[s, "Title"];
subtitleCell[s_String]   := Cell[s, "Subtitle"];
sectionCell[s_String]    := Cell[s, "Section"];
subsectionCell[s_String] := Cell[s, "Subsection"];
textCell[s_String]       := Cell[s, "Text"];
codeCell[s_String]       := Cell[s, "Input"];

linkCell[label_String, url_String] :=
  Cell[TextData[{
    ButtonBox[label,
      BaseStyle  -> "Hyperlink",
      ButtonData :> {URL[url], None}]
  }], "Text", TextAlignment -> Center];

animatedCell[gifPath_String, opts___] :=
  If[FileExistsQ[gifPath],
    Cell[BoxData[
      PaneBox[
        ToBoxes[AnimatedImage[Import[gifPath, "ImageList"],
                              "FrameRate" -> 8,
                              ImageSize -> 720, opts]],
        Alignment -> Center, ImageSize -> Full]],
      "Output",
      CellMargins   -> {{Inherited, Inherited}, {15, 15}},
      TextAlignment -> Center],
    textCell["[gif not found: " <> gifPath <> "]"]];

$imagesDir = FileNameJoin[{DirectoryName[$InputFileName], "images"}];

nb = Notebook[{

  (* ============================================================ *)
  (* TITLE + ABSTRACT                                              *)
  (* ============================================================ *)

  titleCell["Update \[LongDash] Pure\[Hyphen]Wolfram Live Acquisition \
(libusb) for the RadiaCode Toolkit"],

  subtitleCell["A follow\[Hyphen]up to \"Gamma Spectroscopy with the \
RadiaCode Detector in Mathematica\""],

  textCell[
    "This is a short amendment to the original post.  Two things were \
added or fixed since publication:"],

  textCell[
    "  \[Bullet]  A pure\[Hyphen]Wolfram path for live device \
acquisition.  The original post's \[Section]10 dashboard worked, but \
under the hood it depended on Python (the upstream `radiacode` package \
+ `rcmultispg.py`).  This update walks through compiling a tiny \
LibraryLink + libusb shim that talks to the device directly, with no \
Python at runtime.  The new bundle then auto\[Hyphen]selects the \
backend and prints which one it picked, so existing notebook cells \
keep working unchanged."],

  textCell[
    "  \[Bullet]  One sample data file (`K40.rcspg`) could not be \
attached to the original post because Wolfram Community does not \
permit `.rcspg` as an upload extension.  A direct GitHub link is \
provided below; download once and drop into the post's `data/` \
subfolder so \[Section]9 (the K\[Hyphen]40 spectrogram heatmap) \
evaluates cleanly."],

  animatedCell[FileNameJoin[{$imagesDir, "dashboard_live.gif"}]],

  textCell[
    "Hero animation again, for context: a screen recording of the \
live dashboard captured from a connected RadiaCode\[Hyphen]103 \
(serial RC\[Hyphen]103\[Hyphen]015254).  After the steps below, this \
runs Python\[Hyphen]free."],

  (* ============================================================ *)
  (* PART 1: PYTHON-FREE LIVE ACQUISITION                          *)
  (* ============================================================ *)

  sectionCell["Part 1 \[LongDash] pure\[Hyphen]Wolfram live \
acquisition via libusb"],

  textCell[
    "The RadiaCode is a vendor\[Hyphen]class USB device with bulk \
endpoints (0x01 / 0x81), not a HID device, so `hid_enumerate` \
returns nothing for it.  Both the upstream Python `radiacode` \
package and our LibraryLink shim use libusb\[Hyphen]1.0 to speak \
the device's custom command set.  After the one\[Hyphen]time setup \
below, no Python is needed for live acquisition; the original post's \
\[Section]10 cells then auto\[Hyphen]select the libusb backend."],

  subsectionCell["Step 1 \[LongDash] install libusb"],

  textCell[
    "macOS (Homebrew):"],

  codeCell["(* in a shell *)\n(* brew install libusb *)"],

  textCell[
    "Linux:  `sudo apt install libusb-1.0-0-dev`  /  `sudo dnf \
install libusbx-devel`  /  similar for your package manager."],

  subsectionCell["Step 2 \[LongDash] download the C source + build \
script"],

  textCell[
    "Two files, served as plain text from the project's GitHub repo.  \
Drop them into a fresh folder (anywhere; the build will pick its own \
output location).  Right\[Hyphen]click \[Rule] Save Link As, or paste \
the URL into `curl`:"],

  linkCell[
    "Download radiacode_link.c (\[Tilde]13 KB)",
    "https://raw.githubusercontent.com/mthiel74/radiacode-wolfram/main/Wolfram/RadiaCodeTools/clib/radiacode_link.c"],

  linkCell[
    "Download build.wls (\[Tilde]2 KB)",
    "https://raw.githubusercontent.com/mthiel74/radiacode-wolfram/main/Wolfram/RadiaCodeTools/clib/build.wls"],

  textCell[
    "Or browse the folder on GitHub:"],

  linkCell[
    "github.com/mthiel74/radiacode-wolfram (Wolfram/RadiaCodeTools/clib/)",
    "https://github.com/mthiel74/radiacode-wolfram/tree/main/Wolfram/RadiaCodeTools/clib"],

  textCell[
    "Or, from a kernel that has internet access, fetch them in one \
go:"],

  codeCell[
    "$dest = FileNameJoin[{$HomeDirectory, \"radiacode_clib\"}];\n\
If[!DirectoryQ[$dest], CreateDirectory[$dest]];\n\
URLDownload[\n  \
\"https://raw.githubusercontent.com/mthiel74/radiacode-wolfram/main/Wolfram/RadiaCodeTools/clib/radiacode_link.c\",\n  \
FileNameJoin[{$dest, \"radiacode_link.c\"}]];\n\
URLDownload[\n  \
\"https://raw.githubusercontent.com/mthiel74/radiacode-wolfram/main/Wolfram/RadiaCodeTools/clib/build.wls\",\n  \
FileNameJoin[{$dest, \"build.wls\"}]];"],

  subsectionCell["Step 3 \[LongDash] compile the .dylib"],

  textCell[
    "Run the build script under wolframscript.  It uses Wolfram's \
`CCompilerDriver` to invoke clang against the LibraryLink + libusb \
headers and writes `radiacode_link.dylib` (or `.so` on Linux) into \
the same folder as the C source:"],

  codeCell[
    "(* in a shell, from the folder where you put the two files *)\n\
(* wolframscript -file build.wls *)"],

  textCell[
    "Build output is verbose; look for the line that ends with \
`radiacode_link.dylib` (or `.so` / `.dll`).  If clang can't find \
`libusb.h`, double\[Hyphen]check Step 1; on Apple\[Hyphen]Silicon \
Macs Homebrew puts the headers in `/opt/homebrew/include/` which \
the build script picks up automatically."],

  subsectionCell["Step 4 \[LongDash] place the .dylib so the bundle \
finds it"],

  textCell[
    "The updated bundle (attached to this post) probes six \
locations for `radiacode_link.dylib` in priority order.  The \
simplest layout that works:"],

  textCell[
    "  \[Bullet]  Drop both `RadiaCodeToolsBundle.wl` and the \
sample data files into a folder, say `~/RadiaCodeFromCommunity/`.\n\
  \[Bullet]  Inside that folder make a sub\[Hyphen]folder \
`RadiaCodeTools/clib/` and put the freshly built \
`radiacode_link.dylib` there.\n\
The bundle's `DeviceNative.wl` resolver looks for \
`<bundle>/RadiaCodeTools/clib/radiacode_link.dylib` as one of its \
candidates and finds the file."],

  textCell[
    "Alternatively, set the path explicitly before loading the \
bundle:"],

  codeCell[
    "RadiaCodeTools`DeviceNative`$RadiaCodeNativeLibrary =\n  \
\"/full/path/to/radiacode_link.dylib\";\n\
$bundle = NotebookDirectory[] <> \"RadiaCodeToolsBundle.wl\";\n\
Get[ExpandFileName[$bundle]]"],

  subsectionCell["Step 5 \[LongDash] verify"],

  textCell[
    "After a fresh kernel restart and bundle Get, this should be \
True:"],

  codeCell[
    "RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ[]"],

  textCell[
    "And this should list your attached devices via libusb (no \
Python invoked):"],

  codeCell[
    "RadiaCodeTools`DeviceNative`RadiaCodeNativeDevices[]"],

  subsectionCell["Step 6 \[LongDash] use the original post's \
\[Section]10 cells, now Python\[Hyphen]free"],

  textCell[
    "The original post's \[Section]10 \"Open the stream\" cell \
calls `RadiaCodeTools`Device`RadiaCodeAutoStream[\[Ellipsis]]`.  \
That helper auto\[Hyphen]selects: with the .dylib in place it \
uses libusb directly; without it falls back to the Python bridge.  \
On the very first poll it prints a one\[Hyphen]liner telling you \
which backend it picked.  With the steps above completed you will \
see:"],

  Cell[TextData[StyleBox[
        "RadiaCodeAutoStream: using libusb LibraryLink (no Python).",
        FontFamily -> "Courier", FontWeight -> Bold]],
    "Text", TextAlignment -> Center,
    CellMargins -> {{Inherited, Inherited}, {6, 6}}],

  textCell[
    "Re\[Hyphen]evaluating the original post's \[Section]10 cells \
top\[Hyphen]to\[Hyphen]bottom now drives the device through the \
LibraryLink path; the dashboard, state snapshot, and Quantile cell \
all behave identically."],

  (* ============================================================ *)
  (* PART 2: MISSING K-40 RCSPG FILE                               *)
  (* ============================================================ *)

  sectionCell["Part 2 \[LongDash] missing K\[Hyphen]40 spectrogram \
file"],

  textCell[
    "Wolfram Community's attachment policy did not accept the \
`.rcspg` extension when the original post was uploaded, so \
`K40.rcspg` (one of the four sample data files referenced by \
\[Section]9 of the post) had to be left out of the attachments.  \
The file is plain ASCII text \[LongDash] tab\[Hyphen]separated \
header line, hex\[Hyphen]encoded historical spectrum, and one row \
per sample \[LongDash] so the only obstacle is the extension."],

  subsectionCell["Where to download it"],

  textCell[
    "Direct GitHub raw link (\[Tilde]1.3 MB; downloads as \
`K40.rcspg` already \[LongDash] no rename needed):"],

  linkCell[
    "K40.rcspg \[LongDash] direct download",
    "https://raw.githubusercontent.com/mthiel74/radiacode-wolfram/main/Wolfram/community/data/K40.rcspg"],

  textCell[
    "Drop the downloaded file into the original post's `data/` \
subfolder alongside `data_am241.xml`, \
`data_th232_plus_background.xml`, and `trinitite.xml`.  After that, \
the original post's \[Section]9 cells (\"Time\[Hyphen]frequency \
view: the K\[Hyphen]40 spectrogram\" / `RCSpectroPlot[k40]` / the \
integrated\[Hyphen]spectrum + 1461 keV peak fit) all evaluate."],

  subsectionCell["Or import it directly from the cloud"],

  textCell[
    "If you do not want a local copy, you can also import the \
file straight into Wolfram from the GitHub raw URL.  This works for \
`.rcspg` because it is text:"],

  codeCell[
    "k40 = RadiaCodeTools`Formats`ImportRCSpectrogram[\n  \
\"https://raw.githubusercontent.com/mthiel74/radiacode-wolfram/main/Wolfram/community/data/K40.rcspg\"];\n\
k40[\"NumberOfChannels\"]"],

  (* ============================================================ *)
  (* PART 3: WHAT TO ATTACH HERE                                   *)
  (* ============================================================ *)

  sectionCell["What is attached to this update post"],

  textCell[
    "  \[Bullet]  This notebook (`RadiaCodeSpectroscopy_Update.nb`).\n\
  \[Bullet]  The updated single\[Hyphen]file bundle \
(`RadiaCodeToolsBundle.wl`) with the new `RadiaCodeAutoStream` \
helper, the libusb path resolver, and the `RadiaCodeNativeStream` \
streaming wrapper.  Drop alongside this notebook.\n\
\n\
The C source for the libusb shim and the build script live on \
Wolfram Cloud and on GitHub (links above) rather than as Community \
attachments \[LongDash] they are tiny and only needed if you want \
the no\[Hyphen]Python path."],

  (* ============================================================ *)
  (* PS                                                            *)
  (* ============================================================ *)

  sectionCell["PS"],

  textCell[
    "Both the original post and this update assume you have the \
`RadiaCode\[Hyphen]100` series device.  The protocol is the same on \
the `RadiaCode\[Hyphen]101`, `\[Hyphen]102`, and \
`\[Hyphen]103`; the only thing that differs across the family is \
the scintillator volume and a couple of firmware quirks.  The \
toolkit picks them up by USB serial number prefix \
(`RC\[Hyphen]10x\[Hyphen]\[Ellipsis]`) automatically."],

  textCell[
    "If you build the libusb shim and find an undocumented \
firmware record type or run into a `RadiaCodeNativeReadRealtime::\
unknownTag` warning, please open an issue at \
github.com/mthiel74/radiacode\[Hyphen]wolfram with the offending \
`{eid, gid}` pair \[LongDash] adding new tags is a one\[Hyphen]line \
change in `DeviceNative.wl`."]

},
  WindowSize    -> {1400, 900},
  WindowMargins -> {{Automatic, 0}, {Automatic, 0}},
  FrontEndVersion -> "14.0",
  StyleDefinitions -> Notebook[{
    Cell[StyleData[StyleDefinitions -> "Default.nb"]],
    Cell[StyleData["Output"], TextAlignment -> Center,
      CellMargins -> {{Automatic, Automatic}, {Inherited, Inherited}}],
    Cell[StyleData["Print"],  TextAlignment -> Center,
      CellMargins -> {{Automatic, Automatic}, {Inherited, Inherited}}]
  }, StyleDefinitions -> "PrivateStylesheetFormatting.nb"]];

Export[outFile, nb, "Notebook"];
Print["wrote ", outFile, " (", FileByteCount[outFile], " bytes)"];
