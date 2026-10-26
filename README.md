# trackkat

Replacement driver and control software for the NaturalPoint TrackIR 5.

Goals: survive suspend/hibernate, never crash, start automatically when a game
wants head tracking, and keep fully adjustable per-axis curves.

Status: **first game output.** `trackkat` tracks the TrackClip and serves the
pose to games through its own `NPClient` DLLs, like NaturalPoint's software.
`tirprobe` is the diagnostic tool.

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

## Using trackkat with games

1. Quit NaturalPoint's TrackIR software for good; trackkat replaces it.
2. Run `trackkat.exe` once from the unzipped folder. It registers that folder
   as where games find `NPClient`, writes `trackkat.conf` there, and starts
   tracking. Keep the folder where it is: games load the DLLs from it, and
   start `trackkat.exe` from it themselves when it isn't running.
3. Face the camera and press F12 (recenter). Type `c` and Enter in its window
   once, then turn your head left and right, to calibrate which way is up.
4. Start the game. Its TrackIR support should find trackkat; the camera's
   second green light shows when a game is reading the pose.

Keys work from anywhere, including inside games:

- **F12** recenters. A game's own "recenter TrackIR" binding does too.
- **F9** pauses: games see your head held still until you press it again.

Change them in `trackkat.conf`, e.g. `pause = Ctrl+F9`, then restart
trackkat.

If the camera is unplugged, or the PC sleeps, trackkat waits and picks it up
again when it comes back.

### When a game doesn't respond

- `npclient.log`, beside the DLLs, records what each game asked NPClient for
  and what it got.
- `npgame` loads NPClient like a game does (DCS by default, or
  `npgame <game id>`), unscrambles and checksums what it gets as a game
  must, and prints the pose. Run it while trackkat is running.
- `tools/test-npclient.sh` runs the same check under Wine, with a stand-in
  for trackkat.

## Running the probe

Open a terminal in the folder with `tirprobe.exe` and run `tirprobe`. Wear the
TrackClip, face the camera and press **Enter** to recenter. About ten times a
second it prints your head pose relative to that position:

```
yaw   +3.2  pitch   -1.0  roll   +0.4   x    +2  y    -1  z    +5 mm   distance  812 mm  fit 0.00 px
```

Once per setup, calibrate which way is up: type `c`, press Enter, and turn
your head left and right for five seconds. It prints the camera tilt it
found. Until then, angles are measured about the camera's own axes, so a
camera tipped downward mixes some yaw into roll.

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
