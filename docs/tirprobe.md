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

tirprobe learns which way is up and where your neck pivots as trackkat does,
but starts afresh each time. To calibrate up at once: type `c`, press Enter,
and turn your head left and right for five seconds. It prints the camera
tilt it found. Until then, angles are measured about the camera's own axes,
so a camera tipped downward mixes some yaw into roll.

The first green light on the camera shows when the clip is being tracked.

Reflectors don't always show as three neat blobs: seen at a slant one can fade
out, two can run together, and other bright things show up too. Tracking
carries on through these, and the line ends with a note when it happens:

- `2 of 3 reflectors`: one reflector is out of sight, and its pose is held
  from the last frames while the other two are followed. After a second of
  this the clip counts as lost.
- `merged`: two reflectors made one blob, split by its shape. Less accurate.
- `+1 stray`: a blob that isn't a reflector was ignored.

When the clip is lost the line says how many blobs there were, such as
`lost: 2 blobs, need 3`.

## Options

- `--clip LR,LT,RT`: your clip's reflector spacing in mm, centre to centre,
  with left and right as you wear it: left–right (the two on the headband),
  left–top, right–top. The top reflector's post is assumed to lean back. The
  angles depend on the shape of this triangle, so measure your clip. Default:
  `70,110,110`; trackkat reads the same from `clip` in trackkat.conf.
- `--focal F`: camera focal length in pixels (default 660, from the 51.7°
  field of view). Only affects the distance and the size of the angles.
- `--blobs`: show the blobs instead of the pose: each one's centre in
  pixels, how many pixels it lit, and how long it is for its width, like
  `( 303.6,   95.4)  543px 1.6:1`. A single reflector is round (about 1:1);
  two run together are longer.
- `--threshold N` (30–253, default 140), `--ir 5|6|7` (illuminator
  brightness), `--no-status-led`.
- `--raw`: hex-dump every USB transfer; use this when reporting problems:
  `tirprobe --raw > probe.txt`.
- `--list`: list every USB device and whether it can be opened.

Ctrl-C stops the camera cleanly.
