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
3. Face the camera and press F12 (recenter). trackkat works out the rest as
   you look around (see [Calibration](#calibration)).
4. Start the game. Its TrackIR support should find trackkat; the camera's
   second green light shows when a game is reading the pose.

Keys work from anywhere, including inside games:

- **F12** recenters. A game's own "recenter TrackIR" binding does too.
- **F9** pauses: games see your head held still until you press it again.

Either can be a joystick or HOTAS button instead, or as well; see
[Settings](#settings).

Recentering while the clip is out of sight centres the view at once; the
pose you hold when the clip comes back becomes the new centre.

When you shift in your chair, the centre follows where your head comes to
rest, over a minute or two, too slowly to see (`drift` in
[Settings](#settings)). Only rest close to the centre counts, and only
after a while there: watching something off to the side, or holding a
lean, however long, doesn't move it. Recentering puts the centre where
you are at once, as ever. trackkat prints when the centre has followed
you noticeably:

```
Centre has followed you since recentering: yaw +0.3  pitch -0.1  roll +0.0   x +21  y -2  z +6 mm
```

### trackkat's window and tray icon

trackkat shows an icon in the system tray: green while tracking, amber
while the clip is out of sight, grey while looking for the camera, red
without one. A white dot means a game is connected; white bars mean paused.
Click it to open trackkat's window; right-click it to recenter, pause or
quit.

The window shows your head as games see it, with each axis as your head
moves it and as games get it after the curves; whether the camera, the clip
and games are connected; the latency figures; what has been learnt in
[calibration](#calibration); and how far the centre has followed you since
recentering. It has buttons to recenter, pause and
calibrate, and edits the [settings](#settings). Its log shows what
trackkat reports, such as recentering, calibration, joysticks, settings
reloads and errors, but not the once-a-second status line the console
prints; **Copy log** copies it, for bug reports. Closing the window
keeps trackkat running in the tray.

**Started by hand**, trackkat opens its window and keeps its console, where
Enter recenters, `c` Enter calibrates and Ctrl-C quits. Running it again
shows the window of the one already running.

**Started by a game**, trackkat shows only its tray icon, with no console.
Once no game has been tracking for a minute, and its window isn't open, it
quits by itself, so a game restarted within the minute finds it still
running. It prints when it starts counting down, and when it quits:

```
No game or window is using trackkat; quitting in 60 s unless one starts.
Quitting: no game or window used trackkat for 60 s.
```

Quitting from the tray menu turns the camera's lights off and lets it go.

### Calibration

trackkat learns two things from how you move your head, and keeps them in
`trackkat.calibration` beside `trackkat.exe`, so it carries on from where it
left off:

- **Which way is up.** The camera is usually tipped down at you, so its own
  axes would mix some of a left-right turn into roll. trackkat takes up to
  be the axis your head turns about when you glance left and right. Nods,
  tilts and slow drifts don't count.
- **Where your neck pivots.** The clip sits in front of, above and to one
  side of the point your head turns about, so turning your head also moves
  the clip. trackkat finds the point that stays still while you turn (and
  your body doesn't) and measures movement there, so that turning your head
  doesn't also move the view, and only leaning does.

Both keep being refined while you play, with older movement counting less,
so if you move the camera they follow within a few minutes of looking
around (if you tip it by more than about 30 degrees, calibrate up again). Changes apply slowly enough not to move the view, and trackkat
prints a line when one is worth mentioning:

```
Level learnt: camera tipped 14.2 degrees down and 1.3 to its right, from 900 degrees of head turning.
Neck pivot learnt: 95 mm below the clip, 80 behind it and 60 to its right, from 40 head turns.
```

To calibrate up at once, click **Calibrate level** in trackkat's window (or
type `c` and Enter in its console), then turn your head left and right for
five seconds. Delete `trackkat.calibration` to
start afresh; it explains its own format.

### Trying it out: trackdemo

`trackdemo.exe`, in the same zip, starts in a moment, where a game can take
minutes: a cockpit view that follows your head, with the pose, status and
checksum counts NPClient delivers. It loads NPClient as a game does, so it
starts trackkat if it isn't running; if NPClient can't be found or trackkat
isn't tracking, it says so and keeps trying. **C** sends a recenter through
NPClient, as a game would; **Esc** quits.

![trackdemo looking around its cockpit, driven by a scripted pose](docs/trackdemo.gif)

### Settings

`trackkat.conf`, beside `trackkat.exe`, holds the keys and buttons,
smoothing and response curves; it explains its own format. Edit it in
trackkat's window and click **Save**, or in any text editor. Changes apply
as soon as the file is saved, while trackkat runs, so you can tune smoothing
and curves while you play. Saving from the window changes only the settings
you changed there, on their own lines, and keeps the rest of the file,
comments included. If the file has a mistake, trackkat names the
line and carries on with the settings it had. If you had trackkat before
smoothing and curves existed, delete the file and trackkat writes a fresh
one with the new settings documented.

- `recenter = F12`, `pause = Ctrl+F9`: keys, or joystick buttons such as
  `recenter = Throttle - HOTAS Warthog button 23`. Separate several with
  commas: `pause = F9, Joystick - HOTAS Warthog button 5`. In trackkat's
  window, **Add key or button…** adds the next key or joystick button you
  press. For a minute after it starts, and for a minute after you type `j`
  and Enter in its console, trackkat prints each joystick button you press,
  ready to copy:

  ```
  Joystick button pressed: Throttle - HOTAS Warthog button 23
  ```

  Joysticks are named as Windows names them; if two attached have the same
  name, the second is `Name (2)`. trackkat also prints when a joystick is
  connected or disconnected. Joysticks plugged in while it runs, or that
  come back after sleep, work straight away.
- `steadiness = 90`, from 0 to 100: how still the view stays while your
  head is still. Raise it if the view jitters; lower it if the view feels
  floaty or drifts into place.
- `responsiveness = 45`, from 0 to 100: how closely the view keeps up when
  you move. Raise it if the view lags behind your head; lower it if the view
  jitters as you move slowly.
- `smoothing = off` turns smoothing off altogether; `smoothing = on` turns it
  back on.
- `yaw = 1:6`, and likewise `pitch`, `roll`, `x`, `y`, `z`: response
  curves, the same both sides of centre. Most simply a multiplier: `1:6`
  turns the view 6 degrees for each degree you turn your head, so turning
  30 degrees looks straight behind you. The defaults multiply yaw by 6,
  pitch 7, roll 4, x 3, y 4 and z 2.5.

  A centre precision from 0 to 100, as in `yaw = 1:6, precision 40`, makes
  small movements gentler, for aiming and reading gauges, and larger ones
  steeper, while the same head movement still reaches the end of the view:
  180 degrees for yaw, 90 for pitch and roll, 300 mm for x, y and z. With
  multiplier m, that end at R/m and precision p as a fraction k, a movement
  x up to R/m becomes R·((1 − k)·u + k·u³) with u = x·m/R, the expo of
  radio-controlled models: near centre the view moves m·(1 − k) times as
  far as your head, at the end m·(1 + 2k) times, and past it carries on
  that steeply. So `yaw = 1:6, precision 40` still looks straight behind
  at 30 degrees, but a 5 degree glance turns the view 18 degrees rather than 30.

  For any other shape, give input:output points, in degrees or mm, which
  straight lines join, starting from 0:0. For example
  `yaw = 10:10 45:120` turns your view 120 degrees when you turn your head 45.
  Past the last point the curve carries straight on.

  In trackkat's window each axis has a multiplier, a centre precision and
  a plot of its curve, with a dot where your head is now. **Advanced**
  shows the curves as text instead, for points; an axis with points shows
  as a custom curve, and the window leaves it alone unless you change its
  multiplier or precision.
- `lean = view`: leaning moves you relative to where you look in the game.
  With the view turned to look behind you, leaning left (toward the left of
  the screen) leans out of the right side of the cockpit, as your body would
  if it turned with your gaze. Only turning left and right counts: your body
  stays upright when you look up or down. `lean = screen` moves you the same
  way whichever way you look.
- `drift = on`: the centre follows where your head rests, as described
  [above](#using-trackkat-with-games). Moving sideways, forward and back or
  up and down follows after 20 seconds of rest in a new place, by at most
  0.75 mm a second; turning, as from a turned chair or a slouch, after a
  minute, by at most 2 degrees a minute. Only rest with the head still
  within 4 degrees and 40 mm of the centre counts. `drift = off` keeps the
  centre where you recentered.

Single-frame jumps no head could make are ignored.

If the camera is unplugged, or the PC sleeps, trackkat waits and picks it up
again when it comes back.

### When a game doesn't respond

- `npclient.log`, beside the DLLs, records what each game asked NPClient for
  and what it got.
- `npgame` loads NPClient like a game does (as DCS, game 1003, by default,
  or `npgame <game id>`), unscrambles and checksums what it gets as a game
  must, and prints the pose. Run it while trackkat is running.
- [`trackdemo`](#trying-it-out-trackdemo) is a quicker test than a real
  game.
- `tools/test-npclient.sh` runs the same check under Wine, with a stand-in
  for trackkat.

## Running the probe

Open a terminal in the folder with `tirprobe.exe` and run `tirprobe`. Wear the
TrackClip, face the camera and press **Enter** to recenter. About ten times a
second it prints your head pose relative to that position:

```
yaw   +3.2  pitch   -1.0  roll   +0.4   x    +2  y    -1  z    +5 mm   distance  812 mm  fit 0.00 px
```

tirprobe learns which way is up and where your neck pivots as trackkat does,
but starts afresh each time. To calibrate up at once: type `c`, press Enter,
and turn your head left and right for five seconds. It prints the camera
tilt it found. Until then, angles are measured about the camera's own axes,
so a camera tipped downward mixes some yaw into roll.

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

Needs GHC 9.14, cabal and libusb-1.0 headers. Dependencies are frozen in
`cabal.project.freeze`.

```
cabal build all --enable-tests && cabal test
```

On Windows, point cabal at libusb with `--extra-include-dirs` and
`--extra-lib-dirs` (see `.github/workflows/build.yml`).

## Credits

The USB protocol is ported from [linuxtrack](https://github.com/uglyDwarf/linuxtrack)
(MIT licence, © 2009 Tulthix, uglyDwarf), which reverse-engineered it.
