# Backlog

Roughly in order.

- Measure latency from frame arrival to pose publication, and GC pauses;
  show them in the status line. Tune the RTS only if they show a need.
- Per-axis response curves, deadzones and smoothing; reject implausible jumps
  (anatomical limits, maximum head speed).
- Save the level calibration, and keep refining it from everyday head
  movement.
- Estimate the neck pivot from movement, so translation means leaning.
- Optional slow drift of the centre toward where the head rests.
- Joystick/HOTAS buttons for recenter and pause.
- A GUI like TrackIR's: live view of the head pose, binding settings, and
  statistics on the automatic calibration.
- Estimate the clip's shape from movement, for clips other than the
  developer's.
- Head movements as desktop controls (switching virtual desktops or window
  manager panes), on Linux too.
