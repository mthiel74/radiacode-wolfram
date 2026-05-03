#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Generates RadiaCodeSpectroscopy.nb in this folder.  Run from the
   repo root:

       wolframscript -file Wolfram/community/build_RadiaCodeSpectroscopy.wls

   The .wls is the source of truth; the .nb is a build artefact
   regenerated whenever the script changes.  Style modelled on the
   Standard Wolfram Community structure: title, abstract,
   introduction, methods, worked examples, discussion, references.
*)

outFile = FileNameJoin[{DirectoryName[$InputFileName],
                         "RadiaCodeSpectroscopy.nb"}];

(* ----- helpers ------------------------------------------------- *)

titleCell[s_String]      := Cell[s, "Title"];
subtitleCell[s_String]   := Cell[s, "Subtitle"];
sectionCell[s_String]    := Cell[s, "Section"];
subsectionCell[s_String] := Cell[s, "Subsection"];
textCell[s_String]       := Cell[s, "Text"];
codeCell[s_String]       := Cell[s, "Input"];

(* Embed an image file as an Output cell, centred horizontally and
   sized to fit.  Wrapping in PaneBox[..., Alignment -> Center] keeps
   the figure centred at any window width without forcing a fixed
   width on the cell itself. *)
imageCell[path_String, opts___] :=
  If[FileExistsQ[path],
    Cell[BoxData[
      PaneBox[ToBoxes[Show[Import[path], ImageSize -> 720, opts]],
              Alignment -> Center, ImageSize -> Full]],
      "Output",
      CellMargins   -> {{Inherited, Inherited}, {10, 10}},
      TextAlignment -> Center],
    textCell["[image not found: " <> path <> "]"]];

(* Wolfram Community pre-renders notebooks to static HTML server-side
   (cf. TrafficJAms/Wolfram/Community/README.md).  Anything that needs
   a live kernel (Video, Manipulate, Animate, Dynamic) collapses to a
   still of its first frame in the web viewer.  The one self-playing
   exception is AnimatedImage[Import[gif]] -- the pre-renderer passes
   the GIF payload through as an <img> and the browser loops it.
   Embed live recordings via this helper, NOT via Video[]. *)
animatedCell[gifPath_String, opts___] :=
  If[FileExistsQ[gifPath],
    (* Import as "ImageList" -- a flat List of Image[] frames -- not
       as "Animation" (which returns a Manipulate-style Animator
       wrapper that needs a live kernel to advance, doesn't auto-play
       in the FE, and freezes at the first frame on Community's
       static HTML pre-render).  AnimatedImage[<list of frames>]
       auto-plays in the notebook FE and renders as a looping <img>
       on Community. *)
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
$infoDir   = FileNameJoin[{DirectoryName[$InputFileName], "RadiacodeInfo"}];

(* Embed an image as a centred Output cell that is also a clickable
   hyperlink to a hosted full-resolution copy.  Used for the two
   GPT-generated infographics: inline thumbnail in the post,
   click-through to the full-size PNG on Wolfram Cloud. *)
linkedImageCell[localPath_String, hostedUrl_String, opts___] :=
  If[FileExistsQ[localPath],
    Cell[BoxData[
      PaneBox[
        ToBoxes @ Hyperlink[
          Show[Import[localPath], ImageSize -> 760, opts],
          hostedUrl],
        Alignment -> Center, ImageSize -> Full]],
      "Output",
      CellMargins   -> {{Inherited, Inherited}, {10, 10}},
      TextAlignment -> Center],
    textCell["[image not found: " <> localPath <> "]"]];

nb = Notebook[{

  (* ============================================================ *)
  (* 1. TITLE + ABSTRACT                                          *)
  (* ============================================================ *)

  titleCell["Gamma Spectroscopy with the RadiaCode Detector \
in Mathematica"],

  subtitleCell["A handheld scintillator, an XML file, and a few \
pages of Wolfram Language"],

  textCell[
    "The RadiaCode is a USB / Bluetooth gamma\[Hyphen]ray spectrometer \
the size of a deck of cards: a 1 cm\.b3 CsI(Tl) scintillator coupled \
to a silicon photo\[Hyphen]multiplier, with on\[Hyphen]board pulse\
\[Hyphen]height analysis into 1024 channels.  The phone app shows \
the live spectrum and tries to identify the radioisotopes you are \
pointing at.  This post walks through doing the same thing \[LongDash] \
and considerably more \[LongDash] in the Wolfram Language, with an \
open\[Hyphen]source toolkit that imports the device's native files, \
plots calibrated spectra, finds photopeaks, fits Gaussians, and \
ranks candidate isotopes against a built\[Hyphen]in gamma\[Hyphen]line \
library.  All of the code below runs offline against a handful of \
sample files included with the toolkit, so you can reproduce \
everything without owning a detector."],

  textCell[
    "By the end you will have: (a) a calibrated, log\[Hyphen]scale \
spectrum plot of an Am\[Hyphen]241 disc source, (b) an \
energy\[Hyphen]resolution measurement at 60 keV and at 583 keV, \
(c) a ranked list of candidate isotopes for a thorium sample, and \
(d) a dose\[Hyphen]rate map of a survey route plus a \
time\[Hyphen]frequency spectrogram of a long potassium\[Hyphen]40 \
measurement.  Every claim is backed by a plot."],

  (* ============================================================ *)
  (* HOOK VIDEO                                                   *)
  (* ============================================================ *)

  textCell[
    "Below: a 60\[Hyphen]frame screen recording of the live\[Hyphen]updating \
dashboard built with this toolkit, captured from a connected \
RadiaCode\[Hyphen]103 (serial RC\[Hyphen]103\[Hyphen]015254) over \
97 s of acquisition.  The spectrum panel rebuilds as fresh records \
arrive; the count\[Hyphen]rate and dose\[Hyphen]rate panels scroll \
the corresponding traces; the status block at the top\[Hyphen]left \
shows the latest values.  The dashboard is just a Dynamic[] \
expression depending on a state Association the toolkit keeps \
fresh in the background \[LongDash] no event loop in user code."],

  animatedCell[FileNameJoin[{$imagesDir, "dashboard_live.gif"}]],

  (* ============================================================ *)
  (* 1.5. INSTALLATION                                            *)
  (* ============================================================ *)

  sectionCell["Installation"],

  textCell[
    "Three install paths, in order of effort.  All three give you the \
same offline analysis API; only B and C add the ability to talk to \
a connected detector."],

  subsectionCell["A. Single\[Hyphen]file bundle (offline analysis only)"],

  textCell[
    "If you just want to read RadiaCode files, fit calibrations, plot \
spectra, find peaks, identify isotopes, and play with the survey \
data: this is all you need.  Download one file and Get it.  No clone, \
no Python, no compilation.  Reproduces every cell of this notebook."],

  textCell[
    "Download `RadiaCodeToolsBundle.wl` from the repo \
(https://github.com/mthiel74/radiacode-wolfram/blob/main/Wolfram/RadiaCodeTools/RadiaCodeToolsBundle.wl) \
or directly from this Community post, then:"],

  codeCell[
    "(* Edit the path to match where you saved the bundle.  Default \
finds it next to the notebook (the layout used when you download the \
post's attachments together), and falls back to the repo's master \
copy if you happen to be evaluating in a clone. *)\n\
$bundle = NotebookDirectory[] <> \"RadiaCodeToolsBundle.wl\";\n\
If[!FileExistsQ[ExpandFileName[$bundle]],\n  \
$bundle = FileNameJoin[{NotebookDirectory[], \"..\",\n    \
\"RadiaCodeTools\", \"RadiaCodeToolsBundle.wl\"}]];\n\
Get[ExpandFileName[$bundle]]"],

  textCell[
    "The bundle is the concatenation of every package file under \
`Wolfram/RadiaCodeTools/` in load order \[LongDash] roughly 3,600 lines \
of Wolfram Language defining ~70 public functions.  Sample data files \
referenced below (data_am241.xml, data_th232_plus_background.xml, \
trinitite.xml, K40.rcspg) live in the repo's tests/data/ directory \
(and are bundled into community/data/ for direct download); grab \
them alongside the bundle if you \
want to evaluate this notebook end\[Hyphen]to\[Hyphen]end."],

  subsectionCell["B. Python bridge (live device, easiest live install)"],

  textCell[
    "If you have a RadiaCode plugged in via USB and want live \
acquisition: pair the bundle (or a clone of the repo) with the \
upstream Python tools that drive the device.  This is the path of \
least resistance because the protocol decoder is already written and \
maintained.  Setup:"],

  codeCell[
    "(* in a shell, not in Mathematica *)\n\
(* git clone https://github.com/mthiel74/radiacode-wolfram  *)\n\
(* cd radiacode-tools && pip install radiacode             *)"],

  textCell[
    "Then from Mathematica, after `Get[\"...init.wl\"]` (or the \
bundle), the Device.wl namespace is available.  It auto\[Hyphen]detects \
your python3 even if the Mathematica front\[Hyphen]end's PATH is \
stripped (it probes ~/anaconda3, /opt/homebrew, etc.):"],

  codeCell[
    "(* RadiaCodeTools`Device`RadiaCodeDevices[]                          *)\n\
(* spec = RadiaCodeTools`Device`RadiaCodeAcquire[                       *)\n\
(*   \"AccumulationTime\" -> Quantity[60, \"Seconds\"]];                 *)\n\
(* RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[spec]                      *)"],

  subsectionCell["C. Pure\[Hyphen]Wolfram native via libusb (no Python at runtime)"],

  textCell[
    "If you would rather not have Python in your runtime, the toolkit \
ships a LibraryLink shim that talks to the device directly through \
libusb.  Same Wolfram\[Hyphen]native API as Option B, completely \
self\[Hyphen]contained at runtime.  One\[Hyphen]time setup is a \
shell\[Hyphen]command\[Hyphen]then\[Hyphen]wolframscript dance:"],

  codeCell[
    "(* in a shell *)\n\
(* brew install libusb                                                  *)  \n\
(* git clone https://github.com/mthiel74/radiacode-wolfram                *)  \n\
(* cd radiacode-tools                                                   *)  \n\
(* wolframscript -file Wolfram/RadiaCodeTools/clib/build.wls            *)"],

  textCell[
    "After the build step a `radiacode_link.dylib` (or .so on Linux) \
appears next to the C source.  The package then loads it transparently:"],

  codeCell[
    "(* RadiaCodeTools`DeviceNative`RadiaCodeNativeDevices[]              *)\n\
(* h = RadiaCodeTools`DeviceNative`RadiaCodeNativeOpen[]                *)\n\
(* RadiaCodeTools`DeviceNative`RadiaCodeNativeReadSpectrum[h]           *)\n\
(* RadiaCodeTools`DeviceNative`RadiaCodeNativeClose[h]                  *)"],

  textCell[
    "Implementation note: the RadiaCode is a vendor\[Hyphen]class USB \
device with bulk endpoints (0x01/0x81), not a HID device.  hidapi's \
`hid_enumerate` returns nothing for it; you need libusb.  Verified \
on macOS against an attached RC\[Hyphen]103: byte\[Hyphen]identical \
spectrum payload to the Python bridge, with no Python in the \
process tree."],

  subsectionCell["The rest of this post"],

  textCell[
    "Everything below assumes you have one of the three options \
loaded.  We use sample data shipped with the repo so the notebook \
evaluates with no detector attached, which is what the bundle alone \
gets you.  If you have a detector and prefer live acquisition, \
substitute `RadiaCodeAcquire[...]` or `RadiaCodeNativeReadSpectrum[h]` \
for the `ImportRCSpectrum[file]` calls below \[LongDash] the rest of \
the pipeline is identical."],

  (* ============================================================ *)
  (* 2. INTRODUCTION                                              *)
  (* ============================================================ *)

  sectionCell["1. The detector and the toolkit"],

  textCell[
    "The RadiaCode\[Hyphen]10x family (Scan\[Hyphen]Electronics, RU) \
behaves as a vendor\[Hyphen]class USB device with bulk endpoints \
(0x01 / 0x81) on the host side; on the air it \
also speaks Bluetooth Low Energy.  Internally it is a CsI(Tl) \
crystal read out by a 6 \[Times] 6 mm\.b2 SensL SiPM, with a Hamamatsu\
\[Hyphen]style charge\[Hyphen]integrating front end and a 12\[Hyphen]bit \
ADC binning into 1024 channels.  Photon energy maps to channel by a \
quadratic calibration, E(c) = a\:2080 + a\:2081 c + a\:2082 c\.b2, \
stored in the device's flash and exported with every spectrum."],

  textCell[
    "The companion phone / Windows app saves spectra as an XML file \
that contains the cumulative channel counts, the calibration \
coefficients, the elapsed acquisition time, and (optionally) a \
background spectrum collected separately.  Because every interesting \
piece of physics is in that XML, post\[Hyphen]processing in a \
language like Wolfram is straightforward."],

  textCell[
    "RadiaCodeTools (https://github.com/mthiel74/radiacode\[Hyphen]tools) \
is an idiomatic Wolfram Language port of the upstream Python tools.  \
It exposes a flat namespace of functions \[LongDash] no GUI required \
\[LongDash] that turn the device's files into Associations and \
Datasets ready for Query, Plot, NonlinearModelFit, GeoListPlot, \
ArrayPlot, etc."],

  textCell[
    "Sample data files (a few small .xml / .rcspg samples used by \
the worked examples below) ship alongside the post in a `data/` \
subfolder.  If you cloned the repo instead, they live at \
`tests/data/`.  This cell tries both:"],

  codeCell[
    "$dataDir = SelectFirst[\n  \
{FileNameJoin[{NotebookDirectory[], \"data\"}],\n   \
NotebookDirectory[],\n   \
FileNameJoin[{NotebookDirectory[], \"..\", \"..\", \"tests\", \"data\"}]},\n  \
DirectoryQ];\n\
{\"$dataDir\" -> $dataDir,\n  \
\"contents\" -> (FileNames[\"*.xml\" | \"*.rctrk\" | \"*.rcspg\" | \"*.n42\",\n     \
$dataDir] // Map[FileNameTake] // Sort)}"],

  (* ============================================================ *)
  (* 1.b. ABOUT THE DETECTOR (infographics)                       *)
  (* ============================================================ *)

  subsectionCell["About the detector \[LongDash] two infographics"],

  textCell[
    "Two reference figures before we start.  Both were generated \
with OpenAI's GPT image\[Hyphen]generation model from a verbal \
prompt and a few photos of the device, so treat them as \
illustrative summaries rather than authoritative engineering \
drawings (the official RadiaCode\[Hyphen]100 series page \
\[OpenCurlyDoubleQuote]radiacode.com/100\[Hyphen]series\
\[CloseCurlyDoubleQuote] and the manufacturer's knowledge base \
\[OpenCurlyDoubleQuote]radiacode.com/knowledge\
\[CloseCurlyDoubleQuote] are the canonical source).  Click either \
image to open the full\[Hyphen]resolution copy in a new browser \
tab from Wolfram Cloud."],

  Cell[TextData[{
    StyleBox["1. Components and physical layout. ", Bold],
    "Click the image (or this link) for the full\[Hyphen]resolution copy: ",
    ButtonBox[
      "wolframcloud.com/obj/m.thiel/radiacode_teardown.png",
      BaseStyle  -> "Hyperlink",
      ButtonData :> {URL[
        "https://www.wolframcloud.com/obj/m.thiel/radiacode_teardown.png"],
        None}]
  }], "Text", TextAlignment -> Center],

  linkedImageCell[
    FileNameJoin[{$infoDir,
      "F279E26B-CD3B-4B5C-8646-D7BED224EEB1.png"}],
    "https://www.wolframcloud.com/obj/m.thiel/radiacode_teardown.png"],

  Cell[TextData[{
    StyleBox["2. How the scintillation detector works. ", Bold],
    "Click the image (or this link) for the full\[Hyphen]resolution copy: ",
    ButtonBox[
      "wolframcloud.com/obj/m.thiel/radiacode_howitworks.png",
      BaseStyle  -> "Hyperlink",
      ButtonData :> {URL[
        "https://www.wolframcloud.com/obj/m.thiel/radiacode_howitworks.png"],
        None}]
  }], "Text", TextAlignment -> Center],

  linkedImageCell[
    FileNameJoin[{$infoDir,
      "61CBE512-8F6B-43F7-B17B-209B8F103697.png"}],
    "https://www.wolframcloud.com/obj/m.thiel/radiacode_howitworks.png"],

  textCell[
    "Both infographics were produced by GPT image generation \
(OpenAI) and uploaded to Wolfram Cloud via `CloudPublish[\
Import[path], Permissions -> \"Public\"]`.  The links above point \
at those public CloudObjects.  Acknowledgement: the GPT prompts \
referenced the manufacturer's published photos and product \
descriptions for visual reference."],

  (* ============================================================ *)
  (* 3. METHODS                                                   *)
  (* ============================================================ *)

  sectionCell["2. The physics in three paragraphs"],

  textCell[
    "A gamma photon entering the CsI(Tl) crystal deposits some or all \
of its energy via photo\[Hyphen]electric absorption, Compton \
scattering, or (above 1.022 MeV) pair production.  The deposited \
energy creates electron\[Hyphen]hole pairs in the CsI host (\[Tilde]15 \
eV per pair, the empirical pair\[Hyphen]creation energy in CsI(Tl)); \
those carriers transfer their energy to thallium activator sites, \
which de\[Hyphen]excite by emitting visible photons centred around \
550 nm.  The SiPM converts those photons into a \
charge pulse whose integrated area is proportional to the deposited \
energy.  Pulses are histogrammed channel\[Hyphen]by\[Hyphen]channel; \
photopeaks appear at energies where the full photon energy was \
absorbed in a single interaction."],

  textCell[
    "The intrinsic energy resolution of a scintillator at full \
energy E is dominated by Poisson statistics on the number of \
detected scintillation photons N\:2099\:2090 \[Proportional] E:\n\
\n\t\tFWHM(E) / E \[Proportional] 1 / Sqrt[E]   (Poisson limit),\
\n\nplus a constant electronic\[Hyphen]noise floor and a non\
\[Hyphen]proportionality term that becomes important below \
\[Tilde]100 keV.  For a CsI(Tl) crystal of this size, FWHM/E at \
the Cs\[Hyphen]137 661.7 keV line is typically 7\[Dash]10 %.  We \
will measure exactly that below."],

  textCell[
    "Identifying isotopes from a spectrum means matching the \
locations of photopeaks to the characteristic gamma lines of \
candidate nuclides.  The trick is that one peak is rarely \
diagnostic: a 1460 keV line could be K\[Hyphen]40 (1461 keV, the \
dominant natural\[Hyphen]background line) or the Compton edge of \
Tl\[Hyphen]208's 2614 keV emission feeding counts into that \
energy region; a faint 122 keV peak could be Co\[Hyphen]57, \
Eu\[Hyphen]152, or a Th\[Hyphen]232 daughter.  The function we build below scores \
candidates by a peak\[Hyphen]weighted match: candidates whose \
expected lines coincide with the strongest observed peaks rise to \
the top."],

  (* ============================================================ *)
  (* 4. IMPLEMENTATION WALKTHROUGH                                *)
  (* ============================================================ *)

  sectionCell["3. Reading and plotting a spectrum"],

  textCell[
    "Start with an easy case: an Am\[Hyphen]241 calibration disc.  \
The radioactive isotope sits in most household smoke detectors and \
emits a single strong gamma at 59.5 keV (plus a few low\[Hyphen]intensity \
lines we will ignore).  The XML file records 613 s of acquisition."],

  codeCell[
    "amSpec = RadiaCodeTools`Formats`ImportRCSpectrum[\n  \
FileNameJoin[{$dataDir, \"data_am241.xml\"}]];\n\
KeyDrop[amSpec, {\"Counts\", \"Background\"}]"],

  textCell[
    "The cumulative counts vector is what we plot.  RCSpectrumPlot \
applies the polynomial calibration so the x\[Hyphen]axis is in keV; \
the y\[Hyphen]axis is logarithmic by default because the dynamic \
range of an even modestly active source spans four decades."],

  codeCell[
    "RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[amSpec,\n  \
\"PlotLabel\" -> \"Am-241 calibration disc, 613 s\"]"],

  textCell[
    "The 60 keV photopeak dominates everything else.  The shoulder \
to its left is partly Compton scattering inside the crystal and \
partly the L\[Hyphen]shell X\[Hyphen]ray escape \
peak (\[Tilde]34 keV \
below the photopeak).  PeakChannels lists the n highest\[Hyphen]count \
bins with their calibrated energies."],

  codeCell[
    "RadiaCodeTools`SpectrumPlot`PeakChannels[amSpec, 5]"],

  sectionCell["4. Finding photopeaks programmatically"],

  textCell[
    "PeakChannels just picks the top\[Hyphen]n bins by raw count, \
which is fine for a clean source but useless for a busy spectrum: \
adjacent bins on the same peak crowd out everything else.  \
FindPhotopeaks (in the new Spectroscopy package) does proper local\
\[Hyphen]maximum detection with a moving\[Hyphen]average detrending \
step and a non\[Hyphen]maximum suppression window."],

  codeCell[
    "RadiaCodeTools`Spectroscopy`FindPhotopeaks[amSpec] // Dataset"],

  textCell[
    "Run the same thing on the Th\[Hyphen]232 sample (with its \
background spectrum subtracted in the parser).  Thorium's natural\
\[Hyphen]decay chain emits dozens of gammas; the strongest visible \
ones are Pb\[Hyphen]212 at 238 keV, Tl\[Hyphen]208 at 583 keV and \
2614 keV, Ac\[Hyphen]228 around 911\[Hyphen]968 keV, and Bi\[Hyphen]214 \
at 609 keV (Ra\[Hyphen]226 series, present from ambient soil)."],

  codeCell[
    "thSpec = RadiaCodeTools`Formats`ImportRCSpectrum[\n  \
FileNameJoin[{$dataDir, \"data_th232_plus_background.xml\"}]];\n\
RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[thSpec,\n  \
\"PlotLabel\" -> \"Th-232 sample with separate background, 48 h\"]"],

  codeCell[
    "thPeaks = RadiaCodeTools`Spectroscopy`FindPhotopeaks[thSpec,\n  \
\"MaxPeaks\" -> 12];\n\
Dataset[thPeaks]"],

  textCell[
    "Useful sanity check: overlay the located peak energies onto the \
spectrum.  Vertical red lines mark each FindPhotopeaks pick; the \
strongest ones land squarely on the K\[Hyphen]X\[Hyphen]ray, \
Pb\[Hyphen]212 238 keV, Tl\[Hyphen]208 583 keV / Bi\[Hyphen]214 609 \
keV doublet, and Ac\[Hyphen]228 911 keV groups."],

  codeCell[
    "Show[\n  RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[thSpec,\n    \
\"Background\" -> False,\n    \
\"PlotLabel\" -> \"Th-232 with peaks marked\"],\n  \
Graphics[{Red, Dashed, Thickness[0.001],\n    \
Line[{{#, 1}, {#, 10^6}}] & /@ (#[\"Energy\"] & /@ thPeaks)}]\n]"],

  sectionCell["5. Energy resolution: fitting a Gaussian to a peak"],

  textCell[
    "FWHM/E at a known photopeak is the canonical figure of merit for \
a scintillator.  EnergyResolution[spec, energy] takes a window \
around the requested energy, fits a Gaussian on top of a linear \
background, and returns centroid, FWHM, and the FWHM/E ratio in \
percent.  The fit form is:\n\
\n\t\tcounts(E) = A exp[-(E - \[Mu])\.b2 / 2 \[Sigma]\.b2] + b + m \
(E - E\:2080),\n\
\n\
which is the standard one\[Hyphen]Gaussian + linear\[Hyphen]baseline \
model used by every gamma\[Hyphen]spectroscopy package since SAMPO.  \
The FWHM is then 2 Sqrt[2 ln 2] \[Sigma] \[TildeTilde] 2.355 \[Sigma]."],

  codeCell[
    "(* CsI(Tl) at 60 keV: dominated by L X-ray escape and Poisson \
statistics on a small light pulse \[LongDash] resolution will be \
poor. *)\n\
RadiaCodeTools`Spectroscopy`EnergyResolution[amSpec, 60.]"],

  codeCell[
    "(* CsI(Tl) at 583 keV (Tl-208 in the Th sample): typical CsI \
range, 7\[Dash]10 %. *)\n\
RadiaCodeTools`Spectroscopy`EnergyResolution[thSpec, 583.]"],

  textCell[
    "Visualise the fit.  PlotPeakFit overlays the data points and \
fitted curve on the same axes and labels the panel with the FWHM/E \
result \[LongDash] one line of code instead of a Module of \
plumbing."],

  codeCell[
    "RadiaCodeTools`Spectroscopy`PlotPeakFit[thSpec, 583.,\n  \
\"PlotLabel\" -> \"Tl-208 583 keV peak in the Th-232 sample\"]"],

  textCell[
    "8\[Dash]9 % at 583 keV is right in the published CsI(Tl) band, \
and \[Tilde]30 % at 60 keV is similarly typical for a small \
inorganic scintillator at that energy: the L X\[Hyphen]ray escape \
shoulder dominates the line shape and the calibration polynomial \
itself starts to lose precision at the low end.  A user wanting \
cleaner low\[Hyphen]energy resolution would refit the calibration \
with more low\[Hyphen]energy lines (Calibrate.wl in the toolkit \
does that)."],

  sectionCell["6. The phone\[Hyphen]app feature: identifying isotopes"],

  textCell[
    "The new IdentifyIsotopes function compares the FindPhotopeaks \
output against a built\[Hyphen]in library of common gamma\[Hyphen]emitting \
isotopes (Am\[Hyphen]241, Cs\[Hyphen]137, Co\[Hyphen]60, K\[Hyphen]40, \
Na\[Hyphen]22, Ba\[Hyphen]133, Eu\[Hyphen]152, Mn\[Hyphen]54, \
Co\[Hyphen]57, plus the dominant Bi\[Hyphen]214 / Pb\[Hyphen]214 \
(Ra\[Hyphen]226 series) and Tl\[Hyphen]208 / Ac\[Hyphen]228 / Pb\
\[Hyphen]212 (Th\[Hyphen]232 series) lines).  Each library entry is \
{energy, branching ratio}.  Scoring works in two stages:"],

  textCell[
    "(1) For each library line within the user\[Hyphen]chosen \
energy tolerance of an observed peak, mark the line as matched and \
note its branching intensity.  (2) Sum the matched intensities \
weighted by the height of the peak they matched (relative to the \
single strongest peak).  This is the \"PeakWeight\" column.  The \
plain \"Score\" column is the unweighted fraction of the isotope's \
characteristic lines that were seen at all."],

  codeCell["RadiaCodeTools`Spectroscopy`IsotopeLibrary[] // Dataset"],

  textCell[
    "Run the identifier on Am\[Hyphen]241 first.  The expected \
answer is Am\[Hyphen]241, with a single matched line at 59.5 keV \
and a high PeakWeight (the matched line is the dominant peak in \
the spectrum)."],

  codeCell[
    "RadiaCodeTools`Spectroscopy`IdentifyIsotopes[amSpec]"],

  textCell[
    "And on the Th\[Hyphen]232 sample.  Members of the Th decay \
chain (Pb\[Hyphen]212, Ac\[Hyphen]228, Tl\[Hyphen]208) and the \
co\[Hyphen]present Ra\[Hyphen]226 chain (Bi\[Hyphen]214, Pb\[Hyphen]214) \
should rise to the top.  No single isotope is the answer here \
\[LongDash] thorium\[Hyphen]bearing samples in equilibrium emit a \
dozen lines from a dozen daughters."],

  codeCell[
    "RadiaCodeTools`Spectroscopy`IdentifyIsotopes[thSpec,\n  \
\"MaxResults\" -> 8]"],

  (* ============================================================ *)
  (* 5. WORKED EXAMPLES ON REAL DATA                              *)
  (* ============================================================ *)

  sectionCell["7. A historical sample: trinitite"],

  textCell[
    "Trinitite is the green glass formed when desert sand at the \
Trinity test site (New Mexico, July 1945) was fused by the world's \
first nuclear detonation.  A trinitite specimen retains traces of \
the device's plutonium fuel (and therefore Am\[Hyphen]241 from \
Pu\[Hyphen]241 decay), Cs\[Hyphen]137 fission product, and the \
neutron\[Hyphen]activated soil constituents."],

  textCell[
    "Source: trinitite.xml is a sample file shipped with the \
upstream Python toolkit (github.com/ckuethe/radiacode\[Hyphen]tools), \
contributed by a hobbyist who measured a trinitite chip with their \
own RadiaCode for ~72 h.  We are reusing that measurement here \
because (a) trinitite is a charismatic test case and (b) almost no \
reader will have a piece on hand.  Substitute your own \
ImportRCSpectrum[...] for any sample you have measured yourself."],

  codeCell[
    "trinSpec = RadiaCodeTools`Formats`ImportRCSpectrum[\n  \
FileNameJoin[{$dataDir, \"trinitite.xml\"}]];\n\
RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[trinSpec,\n  \
\"Background\" -> \"Subtract\",\n  \
\"PlotLabel\" -> \"Trinitite, 72 h, background-subtracted\"]"],

  codeCell[
    "RadiaCodeTools`Spectroscopy`IdentifyIsotopes[trinSpec,\n  \
\"MaxResults\" -> 6]"],

  textCell[
    "Am\[Hyphen]241 lights up because Pu\[Hyphen]241 in the \
fission fuel beta\[Hyphen]decays to Am\[Hyphen]241 with a 14.3 yr \
half\[Hyphen]life; eight decades on (\[Tilde]5.6 half\[Hyphen]lives) \
essentially all the original Pu\[Hyphen]241 has converted to \
Am\[Hyphen]241 (>98 %).  Cs\[Hyphen]137 is a primary \
fission product (\[Tilde]6 % yield from U\[Hyphen]235 fission, \
30.1 yr half\[Hyphen]life).  Eu\[Hyphen]152 is the giveaway of \
neutron activation \[LongDash] 13.5 yr half\[Hyphen]life, formed \
by Eu\[Hyphen]151(n,\[Gamma]) on natural europium in the soil.  This \
is exactly the cocktail one expects from a 1945 implosion device's \
calling card."],

  textCell[
    "We can also fit the trinitite 60 keV Am-241 peak and overlay \
the result on the data.  Acquisition time alone does not change a \
detector's intrinsic resolution \[LongDash] FWHM/E is set by the \
crystal physics \[LongDash] but a long run reduces statistical \
scatter and makes the photopeak shape more obvious against the \
Compton continuum, which is what the visualisation lets you see:"],

  codeCell[
    "RadiaCodeTools`Spectroscopy`PlotPeakFit[trinSpec, 60.,\n  \
\"PlotLabel\" -> \"Trinitite Am-241 60 keV peak\"]"],

  sectionCell["8. Survey mode: dose rate along a track"],

  textCell[
    "RadiaCode also logs GPS position, ambient dose rate, and count \
rate during a field survey and writes them to a .rctrk file.  Each \
point is timestamped and tagged with the device's reported accuracy \
in metres."],

  textCell[
    "Note: the route below is a SIMULATION, not a real RadiaCode \
recording.  The author of this post has not yet captured a survey \
walk with their own detector; rather than reuse the upstream \
toolkit's synthetic test fixture (which clusters 223 points within \
\[Tilde]100 m of the (0\[Degree], 0\[Degree]) origin in the Gulf of \
Guinea), we synthesise something legible here:\n\
\n\
  \[Bullet] The path is a real walking route through central \
Berlin obtained from `TravelDirections`, so the polyline follows \
actual streets;\n\
  \[Bullet] Each polyline vertex gets a fake timestamp, accuracy, \
count rate and dose rate, with a deliberately injected hot\[Hyphen]spot \
segment near the eastern end of the route.\n\
\n\
The plotting / filtering / histogram pipeline is identical for a \
real recording \[LongDash] swap the synthetic Association below \
for `RadiaCodeTools`Formats`ImportRCTrack[yourFile.rctrk]` and \
the rest of this section works unchanged."],

  codeCell[
    "(* Real Berlin walking route via TravelDirections; serves as a\n   \
realistic stand-in for a .rctrk file we do not actually have.\n   \
The unwrap uses a Cases pattern that works across all Wolfram\n   \
versions / TravelPath layouts, and falls back to a straight-line\n   \
interpolation if TravelDirections is unavailable for any reason\n   \
(no internet, geo-service rate limit, etc). *)\n\
$waypoints = {\n  \
GeoPosition[{52.5163, 13.3777}],   (* Brandenburg Gate           *)\n  \
GeoPosition[{52.5193, 13.4004}],   (* Museumsinsel               *)\n  \
GeoPosition[{52.5215, 13.4112}],   (* Alexanderplatz             *)\n  \
GeoPosition[{52.5170, 13.3889}]};  (* back near Friedrichstrasse *)\n\
\n\
unwrapLeg[leg_] := Cases[Quiet @ leg[\"TravelPath\"],\n  \
{_?NumericQ, _?NumericQ}, Infinity];\n\
straightLine[a_, b_, n_:30] :=\n  \
Table[a + (b - a) (k/n), {k, 0, n}];\n\
\n\
$legs = Table[\n  \
TimeConstrained[\n    \
Quiet @ Check[\n      \
TravelDirections[{$waypoints[[i]], $waypoints[[i + 1]]}],\n      \
$Failed],\n    \
10, $Failed],\n  \
{i, Length[$waypoints] - 1}];\n\
\n\
$routeCoords = Catenate @ MapIndexed[\n  \
Function[{leg, ix},\n    \
With[{coords = If[FailureQ[leg] || leg === $Failed,\n                       \
{},\n                       \
unwrapLeg[leg]]},\n      \
If[Length[coords] >= 2,\n        \
coords,\n        \
straightLine[\n          \
First[$waypoints[[First[ix]]]],\n          \
First[$waypoints[[First[ix] + 1]]]]]]],\n  \
$legs];\n\
Length[$routeCoords]"],

  codeCell[
    "(* Synthesise a demo .rctrk-shaped Association along that route. *)\n\
SeedRandom[20260501];\n\
$bgDose  = 0.10;                  (* \[Mu]Sv/h cosmic + soil      *)\n\
$hotDose = 0.55;                  (* \[Mu]Sv/h granite hotspot    *)\n\
(* Inject a hot stretch over the easternmost ~15 % of the route. *)\n\
$nPts    = Length[$routeCoords];\n\
$hotIdx  = Range[Round[0.65 $nPts], Round[0.80 $nPts]];\n\
$points  = Table[\n  \
Module[{lat, lon, hot},\n    \
{lat, lon} = $routeCoords[[i]];\n    \
hot = MemberQ[$hotIdx, i];\n    \
<|\"Time\"      -> DatePlus[DateObject[{2026, 5, 1, 12, 0, 0},\n                                          \
TimeZone -> \"UTC\"], {2 (i - 1), \"Second\"}],\n      \
\"FileTime\"  -> 0,\n      \
\"Latitude\"  -> lat,\n      \
\"Longitude\" -> lon,\n      \
\"Accuracy\"  -> RandomReal[{4., 9.}],\n      \
\"DoseRate\"  -> If[hot,\n                        \
$hotDose + 0.10 RandomReal[],\n                        \
$bgDose + 0.03 RandomReal[]],\n      \
\"CountRate\" -> If[hot, 24. + 4. RandomReal[], 5. + 1. RandomReal[]],\n      \
\"Comment\"   -> \" \"|>],\n  \
{i, $nPts}];\n\
walk = <|\n  \
\"Header\" -> <|\"Name\" -> \"Berlin walk simulation\",\n                  \
\"SerialNumber\" -> \"RC-103-SIMULATED\",\n                  \
\"Comment\" -> \"Simulated track over a real walking route; not a \
real measurement\",\n                  \
\"Flags\" -> \"EC\"|>,\n  \
\"Points\" -> Dataset[$points]|>;\n\
walk[\"Header\"]"],

  codeCell[
    "(* Quick stats on the demo track. *)\n\
With[{pts = Normal[walk[\"Points\"]]},\n  \
<|\"Points\"    -> Length[pts],\n    \
\"Time span\" -> {Lookup[First[pts], \"Time\"], Lookup[Last[pts], \"Time\"]},\n    \
\"Dose rate / uSv/h\" -> AssociationThread[\n      \
{\"min\", \"median\", \"max\"} ->\n      \
Through[{Min, Median, Max}[Lookup[#, \"DoseRate\"] & /@ pts]]]|>]"],

  textCell[
    "RCTrackPlot drops the points onto a Wolfram GeoListPlot, \
coloured by dose rate.  Because the underlying coordinates came \
from `TravelDirections`, the polyline follows real Berlin streets \
\[LongDash] Pariser Platz, Unter den Linden, Karl\[Hyphen]Liebknecht\
\[Hyphen]Stra\[SZ]e \[LongDash] rather than cutting across blocks.  \
The injected hot\[Hyphen]spot segment shows up as the warm\
\[Hyphen]coloured stretch near the Alexanderplatz end."],

  codeCell[
    "RadiaCodeTools`TrackPlot`RCTrackPlot[walk,\n  \
\"AccuracyFilter\" -> 15,\n  \
\"PlotLabel\" -> \"SIMULATED dose rate (\[Mu]Sv/h) along a Berlin walking route\",\n  \
\"ImageSize\" -> 720]"],

  textCell[
    "The dose\[Hyphen]rate histogram cleanly separates the two \
populations: the bulk\[Hyphen]of\[Hyphen]route background near \
0.10\[Hyphen]0.13 \[Mu]Sv/h, and the spike near 0.55\[Hyphen]0.65 \
\[Mu]Sv/h from the hot\[Hyphen]spot leg.  On a real survey the \
shape is typically log\[Hyphen]normal around background with a long \
tail of hot points wherever the detector paused near elevated \
activity."],

  codeCell[
    "RadiaCodeTools`TrackPlot`RCTrackHistogram[walk, \"DoseRate\"]"],

  sectionCell["9. Time\[Hyphen]frequency view: the K-40 spectrogram"],

  textCell[
    "A spectrogram is a 2\[Hyphen]D matrix indexed by time and \
channel: each row is one short spectrum, each column is one channel.  \
Plotted as a heatmap it shows you which energies show up steadily \
(natural background, a fixed source) and which come and go (a sample \
brought close, then taken away).  The K40.rcspg file is a long \
unattended measurement focused on the 1461 keV K\[Hyphen]40 line in \
a few kilograms of potassium chloride salt."],

  codeCell[
    "k40 = RadiaCodeTools`Formats`ImportRCSpectrogram[\n  \
FileNameJoin[{$dataDir, \"K40.rcspg\"}]];\n\
KeyDrop[k40, {\"Samples\", \"DeltaMatrix\", \"HistoricalCounts\"}]"],

  codeCell[
    "RadiaCodeTools`SpectroPlot`RCSpectroPlot[k40,\n  \
\"Scale\" -> \"Log\", \"ImageSize\" -> 800]"],

  textCell[
    "The bright vertical band near channel \[Tilde]560 (which the \
calibration places at \[Tilde]1461 keV) is the K\[Hyphen]40 \
photopeak; horizontal striations mark times when the count rate \
dropped (the operator stepped away).  Collapsing the matrix along \
the time axis gives a single integrated spectrum we can run through \
the same FindPhotopeaks / IdentifyIsotopes pipeline."],

  textCell[
    "Each spectrogram row is the *delta* counts since the previous \
sample, and the file's `HistoricalSpectrum` field is the \
device's accumulated counts from before the recording started.  Here \
that historical baseline is a long calibration run from a different \
session without the K\[Hyphen]40 sample, so summing it in would \
swamp the real signal.  We collapse only the delta rows to recover \
the spectrum acquired during this recording:"],

  codeCell[
    "(* Sum just the deltas \[LongDash] each row covers a few seconds \
of new counts during this recording. *)\n\
totalCounts = Total[k40[\"DeltaMatrix\"]];\n\
k40Spec = <|\n  \
\"Counts\"          -> totalCounts,\n  \
\"Calibration\"     -> k40[\"Calibration\"],\n  \
\"Background\"      -> None,\n  \
\"SpectrumName\"    -> \"K-40 integrated\",\n  \
\"NumberOfChannels\" -> Length[totalCounts]|>;\n\
RadiaCodeTools`SpectrumPlot`RCSpectrumPlot[k40Spec,\n  \
\"PlotLabel\" -> \"K-40 sample, integrated over the recording window\"]"],

  codeCell[
    "RadiaCodeTools`Spectroscopy`IdentifyIsotopes[k40Spec,\n  \
\"MaxResults\" -> 5]"],

  textCell[
    "And the resolution at the K\[Hyphen]40 1461 keV line.  At this \
energy CsI(Tl) is approaching the Poisson limit, so we expect about \
5\[Dash]6 % FWHM/E:"],

  codeCell[
    "RadiaCodeTools`Spectroscopy`PlotPeakFit[k40Spec, 1461.,\n  \
\"PlotLabel\" -> \"K-40 1461 keV peak (integrated spectrogram)\"]"],

  codeCell[
    "RadiaCodeTools`Spectroscopy`EnergyResolution[k40Spec, 1461.]"],

  (* ============================================================ *)
  (* 9b. LIVE INTERACTIVE DASHBOARD                               *)
  (* ============================================================ *)

  sectionCell["10. Live acquisition: an interactive dashboard"],

  textCell[
    "This is the headline feature of the toolkit: the same Wolfram \
Language code that imports an XML file can talk to the device \
itself.  With a RadiaCode plugged in over USB, the cells in this \
section read live spectra, count rates, and dose rates straight \
from the detector and feed them into a self\[Hyphen]updating \
Dynamic[] dashboard \[LongDash] exactly the screen recording \
shown at the top of the post.  The rest of this section walks \
through how the dashboard is built, what its API looks like, and \
how to read it."],

  subsectionCell["How it is wired up"],

  textCell[
    "Three pieces compose: a producer that talks to the device, a \
consumer that turns each ndjson record into Wolfram state, and a \
Dynamic[] expression that re\[Hyphen]renders whenever the state \
changes.\n\
\n\
  \[Bullet] Producer.  Either the upstream Python `rcmultispg.py \
--stdout` (Option B in the Installation section) or the native \
LibraryLink + libusb shim (Option C) emits one JSON record per \
poll: spectrum samples (cumulative counts + calibration), realtime \
samples (count rate, dose rate, charge, temperature), and GPS \
samples if available.\n\
\n\
  \[Bullet] Consumer.  `LiveViewer.wl` tails that stream into a \
per\[Hyphen]session state Association: latest spectrum, a sliding \
window of realtime samples, GPS history, status fields.  A \
ScheduledTask polls the buffer every \[Tilde]0.5 s, parses any new \
lines, and updates the state.  No explicit event loop in user code.\n\
\n\
  \[Bullet] Display.  `RadiaCodeDashboard[streamId]` returns \
`Dynamic[Refresh[buildDashboard[streamId], UpdateInterval -> 1, \
TrackedSymbols :> {}]]`.  The Refresh ticks once a second, \
buildDashboard reads the current state Association and lays out \
four panels (status block, spectrum, count\[Hyphen]rate trace, \
dose\[Hyphen]rate trace), and the front\[Hyphen]end re\[Hyphen]renders.  \
That is the entirety of the reactivity story \[LongDash] no \
manipulators, no observer pattern, just one Dynamic[]."],

  subsectionCell["Reading the dashboard"],

  textCell[
    "Looking at the hook video at the top of the post:\n\
\n\
  \[Bullet] Top\[Hyphen]left status block.  Status (open/closed), \
serial, record count, time since last update, uptime since you \
called RadiaCodeStream[], the latest count rate in cool blue, the \
latest dose rate in warm red.  These are pulled fresh from the \
state Association on every Refresh tick.\n\
\n\
  \[Bullet] Spectrum panel.  The cumulative photopeak histogram, \
log\[Hyphen]scaled counts vs calibrated keV.  It looks essentially \
static through a short recording because each new spectrum sample \
adds only a tiny correction to the cumulative counts \[LongDash] \
that is by design; the shape stabilises with integration time.\n\
\n\
  \[Bullet] Count\[Hyphen]rate trace (blue, lower\[Hyphen]left).  \
Wall\[Hyphen]time series, filled\[Hyphen]area undertone, ~1 sample/s \
on this device.  A trend down means the operator is moving away \
from a source; a spike usually means a stationary measurement next \
to something hot.\n\
\n\
  \[Bullet] Dose\[Hyphen]rate trace (red, lower\[Hyphen]right).  \
Same axis convention but in \[Mu]Sv/h (the producer reports Sv/h; \
the dashboard multiplies by 10\.fc to keep the y\[Hyphen]axis \
legible)."],

  subsectionCell["The complete API \[LongDash] live"],

  textCell[
    "These are the only four calls you need.  THE CELLS ARE LIVE \
\[LongDash] when you evaluate them with a RadiaCode plugged in over \
USB they actually open a stream, render the live dashboard, and \
poll the device.  If no detector is attached they return a \
Failure[\[Ellipsis]] you can read for the reason; nothing else in \
the notebook breaks."],

  textCell[
    "First a sanity check: the toolkit reports the serial numbers \
of every attached RadiaCode.  An empty list means no device \
detected; a Failure[\[Ellipsis]] usually means the radiacode Python \
package is not installed on the right interpreter (Option B in the \
Installation section) or the libusb shim has not been compiled \
(Option C).  See `Notebooks/LiveDashboard.nb` for a diagnostics \
notebook that resolves both."],

  codeCell[
    "(* Lists the attached devices.  Tries the libusb LibraryLink \n   \
driver first (no Python); falls back to the Python bridge if the \n   \
native shim isn't compiled.  Either way returns a list of serial \n   \
number strings. *)\n\
If[TrueQ @ Quiet @ RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ[],\n  \
RadiaCodeTools`DeviceNative`RadiaCodeNativeDevices[],\n  \
RadiaCodeTools`Device`RadiaCodeDevices[]]"],

  textCell[
    "Open the stream.  RadiaCodeAutoStream picks the right backend \
automatically: it uses the pure\[Hyphen]Wolfram libusb LibraryLink \
driver when the .dylib is built (Option C above, no Python at \
runtime), and falls back to the Python bridge (Option B) when the \
.dylib isn't there.  The cell prints which backend it chose so you \
know which one you're running.  PollInterval is how often the \
toolkit polls the device in seconds; realtime samples come every \
poll, the (heavier) cumulative spectrum is refreshed every Nth poll \
via SpectrumEvery."],

  codeCell[
    "streamId = RadiaCodeTools`Device`RadiaCodeAutoStream[\n  \
\"PollInterval\" -> 1., \"SpectrumEvery\" -> 5]"],

  textCell[
    "Render the dashboard.  Returns a Dynamic[] that self\
\[Hyphen]updates in place every second; spectrum, count\[Hyphen]rate \
trace, and dose\[Hyphen]rate trace all rebind as fresh records \
arrive.  Evaluate this cell once and watch the live recording \
unfold inside the notebook."],

  codeCell["RadiaCodeTools`LiveViewer`RadiaCodeDashboard[streamId]"],

  textCell[
    "Pull the current state at any time as a plain Association \
\[LongDash] handy for ad\[Hyphen]hoc analysis, snapshotting, or \
saving.  Re\[Hyphen]evaluate to get a fresh snapshot of whatever \
the stream looked like the moment you ran the cell."],

  codeCell[
    "state = RadiaCodeTools`LiveViewer`RadiaCodeStreamState[streamId];\n\
(* Show only the user-facing summary fields.  The full state \n   \
Association also stores Source / Process / BufferFile / Task -- \n   \
the subprocess command line, OS process handle, scratch file path,\n   \
and scheduled-task handle -- which embed local file system paths\n   \
and PIDs you probably do not want to publish a snapshot of. *)\n\
KeyTake[state, {\"Id\", \"Status\", \"RecordCount\",\n  \
\"StartTime\", \"LastUpdate\"}]"],

  codeCell[
    "(* Quantiles of the live count-rate trace.  Filter to NumericQ\n   \
because rcmultispg occasionally emits null for a sample that\n   \
hasn't completed; guard against an empty list because the\n   \
dashboard may have started before any samples arrived. *)\n\
With[{nums = Cases[\n    \
Lookup[#, \"CountRate\"] & /@ state[\"Realtime\"], _?NumericQ]},\n  \
If[Length[nums] >= 2,\n    \
Quantile[nums, {0.05, 0.5, 0.95}],\n    \
\"Not enough samples yet \[LongDash] let the stream run a few seconds.\"]]"],

  textCell[
    "A ready\[Hyphen]to\[Hyphen]run version of this section, with a \
pre\[Hyphen]flight diagnostics cell that prints the resolved Python \
path, repo root, and verifies that the radiacode package imports, \
ships in `Notebooks/LiveDashboard.nb`."],

  subsectionCell["Stopping the stream"],

  textCell[
    "When you are done, free the device by evaluating the close \
call.  The cell below is wrapped in a Wolfram comment so that an \
\"Evaluate Notebook\" sweep does NOT inadvertently tear the stream \
down before any data has flowed (which would leave the dashboard \
above stuck on CLOSED / 0 records).  When you actually want to \
stop, remove the leading `(*` and trailing `*)` and evaluate the \
cell."],

  codeCell["(* RadiaCodeTools`LiveViewer`CloseRadiaCodeStream[streamId] *)"],

  (* ============================================================ *)
  (* 6. DISCUSSION                                                *)
  (* ============================================================ *)

  sectionCell["11. Discussion and future directions"],

  textCell[
    "What we have built reproduces the RadiaCode phone app's two \
headline features \[LongDash] live spectrum and isotope hint \
\[LongDash] in a few hundred lines of Wolfram Language, plus a \
proper resolution measurement the phone app does not give you, plus \
GeoListPlot of dose rate, plus a time\[Hyphen]frequency spectrogram. \
The pieces compose: every function takes the same Association the \
file parser returns, so swapping in your own spectrum (drop a .xml \
on the path and call ImportRCSpectrum) is a one\[Hyphen]line change."],

  textCell[
    "Several natural extensions are not in the toolkit yet but are \
each one notebook away:"],

  textCell[
    "\:25cf  Background subtraction with \[Chi]\.b2 goodness\[Hyphen]of\
\[Hyphen]fit \[LongDash] given a separately\[Hyphen]measured \
background, scale and subtract it under the foreground and report \
the residual sum of squares.  RCSpectrumPlot already supports \
\"Background\" -> \"Subtract\"; what is missing is the \[Chi]\.b2 \
diagnostic."],

  textCell[
    "\:25cf  Activity estimation in becquerels \[LongDash] given an \
efficiency curve \[Epsilon](E) for the detector geometry, the area \
under a fitted photopeak divided by \[Epsilon](E) \[Times] \
(branching ratio) \[Times] (live time) gives the source activity.  \
FitGaussianPeak already exposes A and \[Sigma]; the missing piece is \
the efficiency curve itself, which the toolkit's Calibrate.wl could \
absorb if the user provided a known\[Hyphen]activity calibration source."],

  textCell[
    "\:25cf  Compton continuum modelling \[LongDash] subtracting the \
analytic Klein\[Hyphen]Nishina continuum from a peak fit removes \
the linear\[Hyphen]baseline assumption that breaks down at high \
energies and improves resolution estimates in busy spectra."],

  textCell[
    "\:25cf  Cross\[Hyphen]correlation of multiple spectra to flag \
anomalies \[LongDash] given a library of \"normal\" backgrounds \
(your house, your car, your office), the residual after best\
\[Hyphen]matched scaled subtraction is a cheap and cheerful anomaly \
detector."],

  textCell[
    "\:25cf  Confidence intervals on the identification ranking \
\[LongDash] bootstrap the FindPhotopeaks pass over Poisson\
\[Hyphen]resampled counts and report the rank distribution of each \
candidate.  This catches the \"Pb\[Hyphen]212 vs Bi\[Hyphen]214 vs \
Pb\[Hyphen]214\" sibling\[Hyphen]rivalry case that any flat scoring \
makes hard."],

  textCell[
    "\:25cf  Isodose contours on the survey map \[LongDash] interpolate \
the GeoPosition + dose\[Hyphen]rate cloud onto a regular grid and \
contour, the same way a meteorologist contours a temperature field.  \
The Wolfram functions ListInterpolation + ContourPlot do most of the \
work."],

  textCell[
    "All of the above are, by the toolkit's design, additive.  The \
parser layer is fixed; the analysis layer can grow."],

  (* ============================================================ *)
  (* 7. REFERENCES                                                *)
  (* ============================================================ *)

  sectionCell["References and acknowledgements"],

  textCell[
    "\:25cf  Wolfram port (this toolkit, the bundle, the Spectroscopy / \
Calibrate / LiveViewer packages, the LibraryLink + libusb shim, the \
test suite, and this post) \[LongDash] \
https://github.com/mthiel74/radiacode\[Hyphen]wolfram (MIT)."],

  textCell[
    "\:25cf  Upstream Python toolkit \[LongDash] Chris Kuethe's \
radiacode\[Hyphen]tools (https://github.com/ckuethe/radiacode\[Hyphen]tools, \
MIT, Copyright (c) 2023 Chris Kuethe).  All sample data files used \
in the worked examples (data_am241.xml, data_th232_plus_background.xml, \
trinitite.xml, K40.rcspg, plus the unused walk.rctrk / xray.ndjson / \
deadtime triplets shipped with the source repo) come from there and \
are redistributed under MIT with attribution.  The Wolfram port also \
draws on Chris's file format definitions, deadtime semantics, and N42 \
conversion conventions \[LongDash] thanks!"],

  textCell[
    "\:25cf  Underlying device protocol \[LongDash] Maxim Andreev's \
`radiacode` Python library (https://github.com/cdump/radiacode), MIT, \
which speaks the RadiaCode USB / Bluetooth protocol; both the Python \
bridge (Option B in Installation) and the libusb LibraryLink shim \
(Option C) follow the protocol it documents."],

  textCell[
    "\:25cf  RadiaCode device documentation \[LongDash] \
https://www.radiacode.com/"],

  textCell[
    "\:25cf  Knoll, G. F. \"Radiation Detection and Measurement\", \
4th ed.  Wiley, 2010.  Chapters 8\[Dash]10 cover scintillator physics, \
the photopeak / Compton anatomy of a gamma spectrum, and the \
FWHM/E figure of merit."],

  textCell[
    "\:25cf  Brookhaven National Laboratory ENSDF (Evaluated Nuclear \
Structure Data File) \[LongDash] https://www.nndc.bnl.gov/ensdf/ \
\[LongDash] authoritative source for the gamma\[Hyphen]line \
energies and branching ratios in the built\[Hyphen]in IsotopeLibrary; \
the Lund/LBNL Table of Isotopes (https://nucleardata.nuclear.lu.se/toi/) \
is a useful searchable mirror."],

  textCell[
    "\:25cf  IAEA Nuclear Data Sheets \[LongDash] \
https://www.iaea.org/resources/databases/nuclear-data-sheets  \
Authoritative cross\[Hyphen]check for individual decay schemes."],

  textCell[
    "\:25cf  This notebook is regenerated from \
Wolfram/community/build_RadiaCodeSpectroscopy.wls; the .nb is a \
build artefact.  Tests for the underlying functions live in \
Wolfram/Tests/Spectroscopy.wlt."]
},
  WindowSize    -> {1400, 900},
  WindowMargins -> {{Automatic, 0}, {Automatic, 0}},
  FrontEndVersion -> "14.0",
  StyleDefinitions -> Notebook[{
    Cell[StyleData[StyleDefinitions -> "Default.nb"]],
    (* Centre all output cells (graphics, datasets, raster previews) *)
    Cell[StyleData["Output"], TextAlignment -> Center, CellMargins -> {{Automatic, Automatic}, {Inherited, Inherited}}],
    Cell[StyleData["Print"],  TextAlignment -> Center, CellMargins -> {{Automatic, Automatic}, {Inherited, Inherited}}]
  }, StyleDefinitions -> "PrivateStylesheetFormatting.nb"]];

Export[outFile, nb, "Notebook"];
Print["wrote ", outFile, " (", FileByteCount[outFile], " bytes)"];
