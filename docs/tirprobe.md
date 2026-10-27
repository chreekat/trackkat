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

The first green light on the camera shows when all three reflectors are being
tracked.

## Options

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
