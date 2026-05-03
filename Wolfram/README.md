# RadiaCode Tools — Wolfram Language port

An idiomatic Wolfram Language re-implementation of the portable parts of
the Python toolkit one directory up. The goal is **"data into Wolfram,
then visualisation and analysis in Wolfram"**: parsers return native
`Association` / `Dataset` values that you can manipulate with `Query`,
plot with `ListLogPlot`, `GeoListPlot`, etc.

## Loading

From a notebook or script:

```wolfram
Get["/Users/thiel/GitHub/radiacode-tools/Wolfram/RadiaCodeTools/init.wl"]
```

This loads every sub-package in the `RadiaCodeTools` context.

## What's here

### File I/O (`Formats.wl`)

| Function | Reads / Writes |
|---|---|
| `ImportRCSpectrum[file]` | RadiaCode native XML spectrum (`.xml`) |
| `ImportRCTrack[file]` | RadiaCode track (`.rctrk`) → `Dataset` |
| `ExportRCTrack[file, dataset, header]` | `.rctrk` writer |
| `ImportRCSpectrogram[file]` | RadiaCode spectrogram (`.rcspg`) |
| `ImportNDJsonLog[file]` | `rcmultispg` ndjson log; groups records by type |
| `ImportCalibrationJSON[file]` | `calibrate.py` calibration JSON |
| `ImportN42[file]` | ANSI N42 file produced by `ExportN42` |
| `ExportN42[file, assoc]` | ANSI N42 writer |

### Helpers

- `fileTimeToDate[ft]` / `dateToFileTime[d]` — Windows FILETIME ↔ `DateObject`.
- `applyCalibration[{a0,a1,a2}, channels]` — energy in keV from channel index.

### Tools

- `Calibrate.wl` — `FitCalibration`, `ImportAndFitCalibration`, `WriteCalibrationTemplate`, `CalibrationSummary`
- `SpectrumPlot.wl` — `RCSpectrumPlot`, `EnergyCalibrationCurve`, `PeakChannels`
- `N42Convert.wl` — `ConvertRCToN42`
- `TrackPlot.wl` — `RCTrackPlot`, `RCTrackHistogram`, `RCTrackPoints`
- `Deadtime.wl` — `ComputeDeadtime`, `ComputeDeadtimeFromFiles`, `CountRateOfSpectrum`
- `RecursiveDeadtime.wl` — `ScanDeadtime`
- `TrackEdit.wl` — `EditTrack`, `EditTrackFile`
- `TrackSanitize.wl` — `SanitizeTrack`, `SanitizeTrackFile`
- `SpectrogramEnergy.wl` — `DoseFromSpectrum`, `SpectrogramEnergy`, `ScanSpectrogramEnergy`
- `SpectroPlot.wl` — `RCSpectroPlot`
- `RCSpgFromJson.wl` — `ConvertNDJsonToRcspg`
- `RCTrkFromJson.wl` — `ConvertNDJsonToRctrk`
- `N42Validate.wl` — `ValidateN42`, `ValidateN42Recursive` (best-effort: structural check + optional `xmllint --schema` shell-out)
- `LiveViewer.wl` — `OpenRadiaCodeStream`, `RadiaCodeDashboard`, `RadiaCodeStreamState`, `CloseRadiaCodeStream`, `PlaybackNDJson`
- `Device.wl` — `RadiaCodeDevices`, `RadiaCodeAcquire`, `RadiaCodeStream`, `RadiaCodeReset`

## Live and on-device data

RadiaCode is a USB-HID device, **not** a `/dev/tty.usbserial` serial
port — there is no baud rate. Wolfram has no native libusb / hidapi
bindings, so a fully native USB driver would require a LibraryLink
shim. Pending that work, `Device.wl` keeps the upstream Python tools
(`radiacode_poll.py`, `rcmultispg.py`) as the on-device driver and
exposes a clean Wolfram API on top:

```wolfram
RadiaCodeDevices[]                                           (* {"RC-103-000070", ...} *)

spec = RadiaCodeAcquire["AccumulationTime" -> Quantity[60, "Seconds"]];
RCSpectrumPlot[spec]

streamId = RadiaCodeStream[]
RadiaCodeDashboard[streamId]                                 (* live grid in the front-end *)
CloseRadiaCodeStream[streamId]
```

`Device.wl` requires `python3` plus the `radiacode` Python package
(`pip install radiacode`) on `PYTHONPATH`. The function signatures
would not change if a future commit replaces the bridge with a
LibraryLink + hidapi build.

## Tests

`.wlt` files under `Tests/` use `VerificationTest` against the existing
sample data in `../tests/data/`. Run a file with:

```sh
wolframscript -file Tests/Formats.wlt
```

## Not ported (intentional)

These Python tools rely on hardware or services with no idiomatic
Wolfram equivalent. Live capture stays in Python; everything downstream
of the captured files works in Wolfram.

| Tool | Why |
|---|---|
| `radiacode_poll.py`, `rcrtlog.py`, `rcmultispg.py` (live capture), `gpsled.py` | USB/Bluetooth/GPIO/sockets via the Python `radiacode` driver. |
| `n42www.py` | Flask HTTP server. |
| `appmetrics.py` | Prometheus metrics for the live-capture server. |

For `rcmultispg.py`, the *post-processing* (ndjson → `.rcspg` / `.rctrk`)
**is** ported, via `RCSpgFromJson.wl` and `RCTrkFromJson.wl`.

## Notebooks

### `Notebooks/LiveDashboard.nb`

Open in Mathematica, plug your RadiaCode in over USB, evaluate the
cells top-to-bottom. Cells:
1. Load the package (auto-locates `init.wl` from the notebook
   directory)
2. List attached devices
3. Optional: single-shot acquisition with a timed accumulation
4. Start a live streaming dashboard
5. Inspect the live state programmatically
6. Stop the stream
7. Optional reset commands (commented out by default)

`Notebooks/build_LiveDashboard.wls` is the source-of-truth generator;
re-run it (`wolframscript -file Wolfram/Notebooks/build_LiveDashboard.wls`)
to rebuild `LiveDashboard.nb` after editing the cell content.

### `Notebooks/Demo.wls`

A non-notebook script that walks through the four main tools end-to-end
using the sample data in `../tests/data/` (no device required). Run it
from the repo root:

```sh
wolframscript -file Wolfram/Notebooks/Demo.wls
```

It prints peak-channel info, a calibration summary, an N42 round-trip
report, and a track summary, and writes four PNGs alongside the
script (`demo_spectrum.png`, `demo_calibration.png`, `demo_track.png`,
`demo_dose_histogram.png`). You can also open the script in Mathematica
and step through it cell by cell.
