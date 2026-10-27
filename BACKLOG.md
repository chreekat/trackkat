# Backlog

Roughly in order.

- Head movements as desktop controls (switching virtual desktops or window
  manager panes), on Linux too.
- Bright IR sources behind the user (a lit fireplace makes a big blob that
  stops NaturalPoint's software entirely). Beyond rejecting blobs that don't
  match the predicted reflectors, learn sources that stay put while the head
  moves and mask them, and report them so the user can see what the camera
  sees. Test when the fire is lit.

## Low priority

- Tune the RTS only if the status line's latency and GC figures show a need.
- Estimate the clip's shape from movement, for clips other than the
  developer's.
- Find the source of the wiggle after a fast head turn (the view overshoots
  and comes back): print, per snap, the overshoot in the raw pose, the fit
  error during it, and the overshoot sent to the game. If the rebound is real
  head motion, consider negative inertia (IBM TrackPoint) to make stops crisp;
  if the fit worsens, the clip's post is swinging.
- Per-game profiles. Not wanted for now: good defaults instead.
- The window is about 930 px tall at 100%: on 1080p screens at 125% or more
  the bottom is cut off. Split the settings into tabs or let them scroll.
- Two identical controllers (e.g. a pair of T.16000Ms for "HOSAS"): they are
  told apart by sorting device paths, so unplugging one renumbers the other.
  Identify each by its USB port (or serial number, if it has one), so a
  binding follows the stick in that port. The T.16000M's left/right-handed
  switch doesn't change its USB identity, so it doesn't help.
