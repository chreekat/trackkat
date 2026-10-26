# Backlog

Roughly in order.

- Tune the RTS only if the status line's latency and GC figures show a need.
- Optional slow drift of the centre toward where the head rests.
- Joystick/HOTAS buttons for recenter and pause.
- When a game starts trackkat, run it in the system tray rather than a
  console window.
- A GUI like TrackIR's: live view of the head pose, binding settings, and
  statistics on the automatic calibration.
- Estimate the clip's shape from movement, for clips other than the
  developer's.
- Find the source of the wiggle after a fast head turn (the view overshoots
  and comes back): print, per snap, the overshoot in the raw pose, the fit
  error during it, and the overshoot sent to the game. If the rebound is real
  head motion, consider negative inertia (IBM TrackPoint) to make stops crisp;
  if the fit worsens, the clip's post is swinging.
- Head movements as desktop controls (switching virtual desktops or window
  manager panes), on Linux too.
