# `radiacode_link` — native LibraryLink driver

A small C shim that lets Wolfram talk to a RadiaCode USB device directly,
without spawning Python.  Used by `../DeviceNative.wl`.

## Why libusb (not hidapi)

The RadiaCode is a **vendor-class USB device** with bulk endpoints
`0x01` (OUT) and `0x81` (IN); it is *not* a HID device.  Enumerating
with `hidapi` returns zero matches:

```c
hid_enumerate(0x0483, 0xF123)  /* -> NULL */
```

Confirmed against `RC-103-015254` on macOS 14+ with hidapi 0.15.0.  The
upstream Python `radiacode` package uses `pyusb` (libusb) for the same
reason, so this shim does the same.

## Build

Prerequisites:

* `libusb-1.0` headers and library
  * macOS:   `brew install libusb`
  * Debian:  `sudo apt install libusb-1.0-0-dev`
  * Fedora:  `sudo dnf install libusb1-devel`
* A C99 compiler (Apple clang, gcc, ...)
* A working `wolframscript` on the `PATH`

To build:

```sh
cd Wolfram/RadiaCodeTools/clib
wolframscript -file build.wls
```

You should see something like:

```
[build] libusb prefix: /opt/homebrew/opt/libusb
[build] OK -> .../clib/radiacode_link.dylib
```

The output `.dylib` (Linux: `.so`, Windows: `.dll`) lands in this
directory; `DeviceNative.wl` looks for it there at load time.

## Verify

```sh
wolframscript -code '
  Get["../init.wl"];
  Print[RadiaCodeTools`DeviceNative`RadiaCodeNativeAvailableQ[]];
  Print[RadiaCodeTools`DeviceNative`RadiaCodeNativeDevices[]];
'
```

Expected output:

```
True
{RC-103-015254}            (* or your device's serial *)
```

A more complete check, against an attached device:

```sh
wolframscript -code '
  Get["../init.wl"];
  h = RadiaCodeTools`DeviceNative`RadiaCodeNativeOpen[];
  s = RadiaCodeTools`DeviceNative`RadiaCodeNativeReadSpectrum[h];
  Print[Length[s["Counts"]], " channels, ", s["Duration"], "s, ",
        Total[s["Counts"]], " total counts"];
  RadiaCodeTools`DeviceNative`RadiaCodeNativeClose[h];
'
```

## What lives where

* `radiacode_link.c` — opens the USB handle, drains stale input, claims
  interface 0, and exposes:
  * `RC_Enumerate()` — newline-joined list of attached serials
  * `RC_Open(serial)` — returns an integer slot id or `-1`
  * `RC_Close(handle)`
  * `RC_Serial(handle)` — UTF8 string
  * `RC_Execute(handle, requestBytes)` — round-trips one full request
    frame and returns the response payload (with the 4-byte length
    prefix stripped).  All higher-level decoding (spectrum framing,
    sequence numbers, command opcodes, RealTimeData, ...) lives on the
    Wolfram side in `DeviceNative.wl`.
* `build.wls` — `CCompilerDriver` invocation.
* `radiacode_link.dylib` — build output, **not** committed.  Run
  `build.wls` once after cloning.

## Failure modes

| Symptom | Likely cause | Fix |
|---|---|---|
| `[build] ERROR: cannot find libusb-1.0 headers` | libusb not installed | `brew install libusb` (macOS) |
| `RadiaCodeNativeAvailableQ[]` returns `False` | `.dylib` not built / wrong path | re-run `build.wls` |
| `RadiaCodeNativeOpen` returns `Failure[DeviceOpenFailed]` | device not attached, or in use by another process | unplug+replug, or kill the other process (`radiacode_poll`, the official RadiaCode app, ...) |
| `RadiaCodeNativeReadSpectrum` returns `Failure[BadEcho]` | another process is also driving the device → request/response sequence numbers mismatch | only one client at a time |
| First spectrum read after replugging hangs ~3 s then succeeds | pyusb left an orphaned read in flight | the C shim drains stale input on open; this is expected once |

## Notes

* The library keeps up to **8 simultaneous** open handles (`RC_MAX_HANDLES`).
* `WolframLibrary_uninitialize` closes everything on kernel exit, so you
  generally don't need to call `RadiaCodeNativeClose` explicitly — but
  doing so frees the slot for reuse and releases the USB interface so
  another process can grab the device.
