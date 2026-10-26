# trackkat

Replacement driver and control software for the NaturalPoint TrackIR 5.

Goals: survive suspend/hibernate, never crash, start automatically when a game
wants head tracking, and keep fully adjustable per-axis curves.

Status: **probe stage.** `tirprobe` talks to the camera directly and prints the
IR blobs it sees. Nothing is fed to games yet.

Supported hardware: TrackIR 5 revision 3 (USB id `131D:0159`).

## Getting tirprobe

Download the latest build of `main` (no GitHub login needed):
<https://github.com/chreekat/trackkat/releases/download/dev/trackkat-windows.zip>

The zip contains `tirprobe.exe` and `libusb-1.0.dll`; keep them in the same folder.

## One-time setup: switch the camera to WinUSB

The camera ships bound to NaturalPoint's driver, which only NaturalPoint's
software can use. trackkat talks to it through Microsoft's generic WinUSB
driver instead.

1. Quit the TrackIR software (including from the system tray).
2. Download [Zadig](https://zadig.akeo.ie/) and run it.
3. **Options → List All Devices.**
4. In the dropdown, pick the TrackIR. **Check that the USB ID boxes read
   `131D` `0159`** — if they show anything else you have the wrong device.
5. Set the target driver (right of the green arrow) to **WinUSB** and click
   **Replace Driver**.

To undo: Device Manager → the TrackIR → Update driver → Browse my computer →
Let me pick → choose NaturalPoint's driver. Reinstalling the TrackIR software
also restores it.

## Running the probe

Open a terminal in the folder with `tirprobe.exe` and run `tirprobe`. Wear the
TrackClip, face the camera and press **Enter** to recenter. About ten times a
second it prints your head pose relative to that position:

```
yaw   +3.2  pitch   -1.0  roll   +0.4   x    +2  y    -1  z    +5 mm   distance  812 mm  fit 0.00 px
```

The first green light on the camera shows when all three reflectors are being
tracked.

Options:

- `--clip LR,LT,RT`: your clip's reflector spacing in mm, centre to centre:
  left–right (the two on the headband), left–top, right–top, as seen from the
  camera. The top reflector's post is assumed to lean back. The angles depend
  on the shape of this triangle, so measure your clip. Default: `70,116,116`.
- `--focal F`: camera focal length in pixels (default 660, from the 51.7°
  field of view). Only affects the distance and the size of the angles.
- `--blobs`: show raw blob positions instead of the pose.
- `--threshold N` (30–253, default 140), `--ir 5|6|7` (illuminator
  brightness), `--no-status-led`.
- `--raw`: hex-dump every USB transfer; use this when reporting problems:
  `tirprobe --raw > probe.txt`.
- `--list`: list every USB device and whether it can be opened.

Ctrl-C stops the camera cleanly.

## Building

Needs GHC ≥ 9.4, cabal and libusb-1.0 headers.

```
cabal build all --enable-tests && cabal test
```

On Windows, point cabal at libusb with `--extra-include-dirs` and
`--extra-lib-dirs` (see `.github/workflows/build.yml`).

## Credits

The USB protocol is ported from [linuxtrack](https://github.com/uglyDwarf/linuxtrack)
(MIT licence, © 2009 Tulthix, uglyDwarf), which reverse-engineered it.
