# tirprobe

`tirprobe.exe`, in the same zip as trackkat, talks to the camera directly and
prints what it sees. Use it when trackkat can't find the camera, or to send a
USB trace with a bug report. Quit trackkat first: only one program can have
the camera.

Open a terminal in the folder with `tirprobe.exe` and run `tirprobe`. Wear the
TrackClip, face the camera, and press **Enter** to recenter. About ten times a
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
changes, a line says why: the likeliest way of seeing the clip in the
frame, what counted most against it with its figure, and what each
hypothesis about the frame scored:

```
# lost, with 3 blobs: finding the clip afresh, unlikely: near 181 mm < 250 mm; over away -7.785 nats
# lost, with 3 blobs: finding the clip afresh, unlikely: left size 78 px (expected 826) (-6.6 nats); over away -8.19 nats, afresh by layout -17.73 nats
# lost, with 2 blobs: following the clip, unlikely: partly seen 1.008 s > 1 s; over continued as predicted 22.66 nats, afresh as predicted 9.175 nats
# lost, with 3 blobs: finding the clip afresh, unlikely: sure 0.06785 nats < 2.197 nats; over afresh by layout -1.878 nats, away -1.946 nats
```

Here the blobs fit the clip, but nearer the camera than the clip can be
tracked; then they were far smaller than its reflectors have been; then
one reflector had been out of sight for over a second, so that the two in
view no longer said where the head was; then three blobs were hardly
likelier the clip than not. With `--explain` every line ends with which
hypothesis that frame was taken for and what each term of its score found,
and a line says what ruled out each half second of head turning that the
neck pivot, the clip's shape, or the mirror image wasn't learned from. The
terms are listed in [heuristics.md](heuristics.md).

Every frame, tracking scores each way the blobs could be the clip, and the
clip being away, and takes the likeliest. A line says so when that wasn't
the clip carrying on from where it was, its reflectors where predicted:

```
# afresh by layout 34.76 nats; over continued as predicted 33.59 nats, afresh as predicted 32.9 nats
# resumed by layout 32.66 nats; over afresh by layout 16.78 nats, resumed as predicted 14.41 nats
```

*Continued* is the clip carrying on from where it was a moment ago; *by
layout* says its blobs were picked out by how they lie, not by where they
were predicted. *Afresh* is the clip with nothing said of where it was:
what was known of the [mirror image](details.md#mirror-images) starts
over. *Resumed* is the clip back where it was before being taken afresh
somewhere else for a moment. Scores are in nats, logs of how much likelier
the frame is that way than with no clip in it: more is likelier.

A light being ignored says why it was taken for one, and a mirror-image
pose corrected says on what evidence:

```
# Ignoring a bright light at (156, 296): it stayed put for 3 seconds, and at 813 pixels is bigger than the clip's reflectors (349).
# corrected a mirror-image pose: switched the tracked pose: the tracked pose 99.8% likely the mirror image, the center 51.8%; the head turned 12 degrees from center as it was, 19 with one switched
```

## Recording a problem

When tracking goes wrong, such as the view jumping, flipping, or drifting,
a recording lets the developer replay it exactly, frame by frame, through
trackkat's tracking. trackkat records while you play; tirprobe records on
its own, with no game, when you can make the problem happen again.

### While you play, with trackkat

1. Bind a key or button to `mark` in `trackkat.conf`, in
   `%APPDATA%\trackkat`, as you would to `recenter`:

   ```
   mark = F8
   ```

   The window has no list for it; the file is the only place.
2. Start trackkat with `--record`, either way:
   - From a terminal in the folder with `trackkat.exe`, run
     `.\trackkat.exe --record`.
   - Copy trackkat's shortcut, open the copy's **Properties**, and add
     ` --record` to the end of **Target**, after the closing quote. An
     installed trackkat's shortcut names the version it had then; once
     that version has been updated away, make the copy again.

   A trackkat already running makes way for it. The log says where the
   recording is going:

   ```
   Recording to C:\Users\you\AppData\Roaming\trackkat\recording-2026-11-16-203015.txt: about 6 MB a minute, until trackkat quits.
   ```

   The file is named for the date and time, beside `trackkat.conf`.
   `--record FILE` records to FILE instead, replacing it.
3. Play. The moment tracking goes wrong, press your mark: the log says
   `Mark 1 recorded.` (then 2, and so on) and the recording notes the
   moment.
4. Quit trackkat when you are done. The last line of its log, which a
   terminal shows, is `Recording saved to` and the file.
5. Zip the file (right-click it, **Compress to ZIP file**) and attach the
   zip to your bug report, saying what went wrong at each mark.

The recording holds what trackkat tracked from, in the order it happened:
the bright spots the camera saw in every frame, which are not pictures;
the settings, as trackkat.conf had them at the start and each time it was
saved; the calibration trackkat started from; and each recenter, from a
key, the window, or a game, each calibration asked for, and the
calibration being cleared. So replaying it tracks your head just as
trackkat did, to the last digit, through the camera being unplugged and
found again, which the recording notes. What it does not hold is what
came after tracking: pause, trim, aim, smoothing, and the curves, so a
replay shows your head's pose, not the view games were sent.

Recording doesn't slow tracking down: frames are written by a thread of
their own. If the disk is full, or falls about 20 seconds behind, the
recording stops there and the log says so, as `Recording stopped: the disk
can't keep up. Tracking goes on.`, and tracking does go on. If trackkat
is killed instead of quit, the recording lacks its last second at most,
and replays up to there. Uninstalling trackkat without keeping your
settings deletes the recordings kept beside them.

Replay it with tirprobe, as [below](#replaying):

```
tirprobe --replay recording-2026-11-16-203015.txt --explain
```

### On its own, with tirprobe

1. Quit trackkat (**Quit trackkat** in its window, or from the tray menu):
   only one program can have the camera.
2. Open a terminal in the folder with `tirprobe.exe` and run
   `tirprobe --record problem.txt`.
3. Wear the clip, face the camera, and press **Enter** to recenter.
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

### Replaying

`tirprobe --replay problem.txt` tracks a recording as it was tracked live,
by tirprobe or by trackkat, on Windows or Linux, printing the lines
tirprobe prints, each after the time in seconds, and where there was a
recenter (`# Enter pressed`), a calibration asked for (`# c Enter
pressed`), or a mark, and what else trackkat recorded: a setting saved,
the calibration cleared, the camera going and coming back:

```
     12.483  # mark 1
     31.020  # setting clip = 70 105 105
     40.250  # camera stopped: The camera went quiet, perhaps after sleep.
```

Recordings made by builds before trackkat could record replay as they did.

With `--blobs` it shows the blobs instead, `--every-frame` prints every
frame rather than ten a second, `--explain` says what each frame was judged
by, and `--clip` and `--no-mask` track with other settings than those
recorded, throughout. A replay ends with how many windows of head turning the neck
pivot, the clip's shape, and the mirror image were each learned from, and how
many were left out, by what ruled them out:

```
# windows of turning for the pivot: 90 taken, 35 turn
```

## Options

- `--clip LR,LT,RT`: your clip's reflector spacing in mm, center to center,
  with left and right as you wear it: left–right (the two on the headband),
  left–top, right–top. The top reflector's post is assumed to lean back. The
  angles depend on the shape of this triangle, so measure your clip. Default:
  `clip` from trackkat.conf, else `70,110,110`.
- `--focal F`: camera focal length in pixels (default 660, from the 51.7°
  field of view). Only affects the distance and the size of the angles.
- `--blobs`: show the blobs instead of the pose: each one's center in
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
- `--replay FILE`: track a recording, tirprobe's or trackkat's, instead of
  the camera.
- `--explain`: end each frame's line with what tracking judged it by, each
  figure against its limit, and say what ruled out each window of head
  turning not learned from.
- `--raw`: hex-dump every USB transfer; use this when reporting problems:
  `tirprobe --raw > probe.txt`.
- `--list`: list every USB device and whether it can be opened.

Ctrl-C stops the camera cleanly.
