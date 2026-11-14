# tirprobe

`tirprobe.exe`, in the same zip as trackkat, talks to the camera directly and
prints what it sees. Use it when trackkat can't find the camera, or to send a
USB trace with a bug report. Quit trackkat first: only one program can have
the camera.

Open a terminal in the folder with `tirprobe.exe` and run `tirprobe`. Wear the
TrackClip, face the camera and press **Enter** to recenter. About ten times a
second it prints your head pose relative to that position:

```
yaw   +3.2  pitch   -1.0  roll   +0.4   x    +2  y    -1  z    +5 mm   distance  812 mm  fit 0.00 px
```

tirprobe tracks the clip shape set by `clip` in trackkat.conf, in
`%APPDATA%\trackkat`, and ignores still lights unless trackkat.conf has
`ignore lights = off`, as trackkat does; `--clip` and `--no-mask` override
these. It only reads trackkat.conf, and tracks with the defaults if there is
none. Its first line says which clip shape it is using and where that came
from:

```
# clip 70,110,110 from C:\Users\you\AppData\Roaming\trackkat\trackkat.conf
```

tirprobe learns which way is up and where your neck pivots as trackkat does,
but starts afresh each time: it doesn't read or write trackkat.calibration. To calibrate up at once: type `c`, press Enter,
and turn your head left and right for five seconds. It prints the camera
tilt it found. Until then, angles are measured about the camera's own axes,
so a camera tipped downward mixes some yaw into roll. Like trackkat, it
corrects a mirror-image pose, with yaw and pitch reversed, from how your
head turns, and prints `# corrected a mirror-image pose`.

The first green light on the camera shows when the clip is being tracked.

Reflectors don't always show as three neat blobs: seen at a slant one can fade
out, two can run together, and other bright things show up too. Tracking
carries on through these, and the line ends with a note when it happens:

- `2 of 3 reflectors`: one reflector is out of sight, and its pose is held
  from the last frames while the other two are followed. After a second of
  this the clip counts as lost.
- `merged`: two reflectors made one blob, split by its shape. Less accurate.
  A blob no bigger than one reflector may be just one, the other out of
  sight, so after a few seconds of that the clip counts as lost too.
- `+1 stray`: a blob that isn't a reflector was ignored.
- `+2 masked`: blobs where a bright light stays put, such as a lit fire
  behind you, were ignored; see [still lights](details.md#still-lights).

When the clip is lost the line says how many blobs there were, such as
`lost: 2 blobs, need 3`. When it is first lost, and whenever the reason
changes, a line says why: what tracking was trying, and what ruled the blobs
out, each with its figure and its limit:

```
# lost, with 3 blobs: following the clip, rejected: aside 401.2 mm > 400 mm
# lost, with 3 blobs: finding the clip afresh, rejected: near 181 mm < 250 mm, size 81 px < 926 px
# lost, with 2 blobs: following the clip, rejected: partly seen 1.008 s > 1 s
```

Here the blobs fit the clip, but 401 mm to the side of where you recentered,
further than a seated head goes; then they were nearer the camera than the
clip can be tracked, and far smaller than its reflectors have been; then one
reflector had been out of sight for over a second. With `--explain` every
line ends with what that frame was judged by, passed or not, and a line says
what ruled out each half second of head turning that the neck pivot, the
clip's shape or the mirror image wasn't learnt from. The names and limits
are listed in [heuristics.md](heuristics.md).

A light being ignored says why it was taken for one, and a mirror-image
pose corrected says on what evidence:

```
# Ignoring a bright light at (156, 296): it stayed put for 3 seconds, and at 813 pixels is bigger than the clip's reflectors (349).
# corrected a mirror-image pose: switched the tracked pose: the tracked pose 99.8% likely the mirror image, the centre 51.8%; the head turned 12 degrees from centre as it was, 19 with one switched
```

## Recording a problem

When tracking goes wrong in a way you can make happen again, such as the
view jumping, flipping or drifting, a recording lets the developer replay
it exactly, frame by frame, through trackkat's tracking.

1. Quit trackkat (**Quit trackkat** in its window, or from the tray menu):
   only one program can have the camera.
2. Open a terminal in the folder with `tirprobe.exe` and run
   `tirprobe --record problem.txt`.
3. Wear the clip, face the camera and press **Enter** to recenter.
4. Do whatever makes it go wrong. The moment it does, type `m` and press
   **Enter**: tirprobe prints `# mark 1` (then `# mark 2`, and so on) and
   the recording notes the moment.
5. Press **Ctrl-C** to stop.
6. Zip `problem.txt` (right-click it, **Compress to ZIP file**) and attach
   the zip to your bug report, saying what went wrong at each mark.

A recording takes about 6 MB a minute, a quarter of that zipped, so a few
minutes is plenty. It holds what tirprobe tracked: the bright spots the
camera saw, which are not pictures, and the settings it tracked with, from
trackkat.conf. Recording doesn't slow tracking down.

`tirprobe --replay problem.txt` tracks a recording as tirprobe tracked it
live, on Windows or Linux, printing the same lines, each after the time in
seconds, and where Enter, `c` Enter and `m` Enter were pressed:

```
     12.483  # mark 1
```

With `--blobs` it shows the blobs instead, `--every-frame` prints every
frame rather than ten a second, `--explain` says what each frame was judged
by, and `--clip` and `--no-mask` track with other settings than those
recorded. A replay ends with how many windows of head turning the neck
pivot, the clip's shape and the mirror image were each learnt from, and how
many were left out, by what ruled them out:

```
# windows of turning for the pivot: 90 taken, 35 turn
```

## Options

- `--clip LR,LT,RT`: your clip's reflector spacing in mm, centre to centre,
  with left and right as you wear it: left–right (the two on the headband),
  left–top, right–top. The top reflector's post is assumed to lean back. The
  angles depend on the shape of this triangle, so measure your clip. Default:
  `clip` from trackkat.conf, else `70,110,110`.
- `--focal F`: camera focal length in pixels (default 660, from the 51.7°
  field of view). Only affects the distance and the size of the angles.
- `--blobs`: show the blobs instead of the pose: each one's centre in
  pixels, how many pixels it lit, and how long it is for its width, like
  `( 303.6,   95.4)  543px 1.6:1`. A single reflector is round (about 1:1);
  two run together are longer. Blobs ignored as part of a still light end
  with `masked`.
- `--no-mask`: don't learn and ignore bright lights that stay put, whatever
  trackkat.conf's `ignore lights` says.
- `--threshold N` (30–253, default 140), `--ir 5|6|7` (illuminator
  brightness), `--no-status-led`.
- `--record FILE`: record every frame to FILE, to be replayed; see
  [recording a problem](#recording-a-problem).
- `--replay FILE`: track a recording instead of the camera.
- `--explain`: end each frame's line with what tracking judged it by, each
  figure against its limit, and say what ruled out each window of head
  turning not learnt from.
- `--raw`: hex-dump every USB transfer; use this when reporting problems:
  `tirprobe --raw > probe.txt`.
- `--list`: list every USB device and whether it can be opened.

Ctrl-C stops the camera cleanly.
