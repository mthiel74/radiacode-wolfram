#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Capture the LiveViewer dashboard while it streams real data from a
   connected RadiaCode and export an MP4 + GIF for the Community post.

   Run from the repo root with the detector plugged in:
       wolframscript -file Wolfram/community/build_live_video.wls

   Output:
       Wolfram/community/images/dashboard_live.mp4
       Wolfram/community/images/dashboard_live.gif
       Wolfram/community/images/dashboard_live_hero.png   (last frame)
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..",
                   "RadiaCodeTools", "init.wl"}]];

imagesDir = FileNameJoin[{DirectoryName[$InputFileName], "images"}];
If[!DirectoryQ[imagesDir], CreateDirectory[imagesDir]];

(* ---- preflight: device must be present ---- *)
devices = RadiaCodeTools`Device`RadiaCodeDevices[];
If[!ListQ[devices] || devices === {},
  Print["No RadiaCode detected.  Plug a device in over USB and rerun."];
  Exit[1]];
Print["Found devices: ", devices];

(* ---- start the live stream ---- *)
streamId = RadiaCodeTools`Device`RadiaCodeStream["PollingInterval" -> 4.];
Print["Stream id: ", streamId];

(* ---- frame capture loop ---- *)
frameCount    = 60;          (* 60 frames *)
secondsBetween = 1.0;        (* sample dashboard once per second *)

(* Let the subprocess come up and emit a first record. *)
Print["Warming up..."];
Pause[10.];

Print["Capturing ", frameCount, " frames at ", secondsBetween, " s intervals"];
frames = Reap[
  Do[
    Module[{img},
      Pause[secondsBetween];
      img = Rasterize[
        RadiaCodeTools`LiveViewer`Private`buildDashboard[streamId],
        ImageResolution -> 144,
        RasterSize      -> {1280, 700},
        ImageSize       -> {1280, 700},
        Background      -> White];
      Sow[img];
      If[Mod[i, 10] === 0,
        Module[{state = RadiaCodeTools`LiveViewer`RadiaCodeStreamState[streamId]},
          Print["  frame ", i, "/", frameCount,
                "  records=", state["RecordCount"],
                "  realtime=", Length[state["Realtime"]],
                "  spectrum?=", Length[state["Spectrum"]["Counts"]] > 0]]]],
    {i, 1, frameCount}]
][[2, 1]];

Print["Captured ", Length[frames], " frames"];

(* Stop the stream cleanly *)
RadiaCodeTools`LiveViewer`CloseRadiaCodeStream[streamId];
Pause[1.];

(* ---- export ---- *)
heroPath = FileNameJoin[{imagesDir, "dashboard_live_hero.png"}];
Export[heroPath, Last[frames]];
Print["Wrote ", heroPath, " (", FileByteCount[heroPath], " bytes)"];

gifPath = FileNameJoin[{imagesDir, "dashboard_live.gif"}];
Export[gifPath, frames,
  "FrameRate" -> 8, "AnimationRepetitions" -> Infinity];
Print["Wrote ", gifPath, " (", FileByteCount[gifPath], " bytes)"];

(* MP4 via ffmpeg from the GIF; Wolfram's MP4 export is patchy *)
mp4Path = FileNameJoin[{imagesDir, "dashboard_live.mp4"}];
ffmpegPath = SelectFirst[{"/opt/homebrew/bin/ffmpeg", "/usr/local/bin/ffmpeg",
                          "/usr/bin/ffmpeg"}, FileExistsQ];
If[StringQ[ffmpegPath],
  RunProcess[{ffmpegPath, "-y", "-i", gifPath,
              "-movflags", "+faststart", "-pix_fmt", "yuv420p",
              "-vf", "scale=trunc(iw/2)*2:trunc(ih/2)*2",
              mp4Path}, "ExitCode"];
  Print["Wrote ", mp4Path, " (", FileByteCount[mp4Path], " bytes)"],
  Print["ffmpeg not found; GIF only"]];
