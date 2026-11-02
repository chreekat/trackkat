# Backlog

Roughly in order.

- Once ignoring still lights has proven itself, make it always on and drop
  the `ignore lights` setting and its checkbox.
- Split merged blobs of elongated reflectors: `splitMerged` takes each
  reflector for a round disc, but tape on a curved tab shows wider than tall.
  In simulation, looking up 30 degrees while turning, the top reflector runs
  into a lower one, the split is off, and tracking goes 10-20 degrees wrong
  and is lost, for points and tabs alike; round blobs pass the same place.
- The level learner can lock onto a wrong axis: each turn is accepted only
  within 30 degrees of the current estimate, so an early wrong estimate keeps
  confirming itself (in simulation, heavy quick nodding and tilting settled
  near 40 degrees right, true 4, in 2 of 4 runs), and roll that goes with
  yaw reads about 8 degrees too far down. Estimate up as the main axis of the
  spread of head orientations instead, with no acceptance cone.
- Ground truth from a webcam: record it alongside the camera and estimate the
  head pose from the face, to check poses, the learnt pivot and level, and the
  reflectors' shift.

- For a second kind of clip (e.g. the side-mounted, LED TrackClip PRO),
  once one is in hand to check against:
  - markers named by place on the clip, told apart by the triangle's
    geometry, not by where they appear in the image (`Model`'s left, right
    and top);
  - one appearance model per kind of marker (point, tape on a curved tab, LED
    that dims off-axis), used alike by the solver, matching, the still-light
    mask's size rule and the camera view;
  - the shape estimate as "hold one edge, place the other points";
  - clip presets as data (`clip = trackclip`, `trackclip-pro left`, or
    points and a mounting).

## Low priority

- Tune the RTS only if the status line's latency and GC figures show a need.
- Apply the clip shape estimated from movement (see docs/details.md), once
  it has proven itself on real clips: re-solve its linearisation about the
  estimate (it falls short of a far different shape by about a tenth), keep
  it with the calibration, and offer to write it to `clip`.
- Find the source of the wiggle after a fast head turn (the view overshoots
  and comes back): print, per snap, the overshoot in the raw pose, the fit
  error during it, and the overshoot sent to the game. If the rebound is real
  head motion, consider negative inertia (IBM TrackPoint) to make stops crisp;
  if the fit worsens, the clip's post is swinging.
- Per-game profiles. Not wanted for now: good defaults instead.
- The window (880 px tall at 100%) scrolls when the screen is shorter, but
  tabbing to a control scrolled out of sight doesn't scroll it into view.
- Two identical controllers (e.g. a pair of T.16000Ms for "HOSAS"): they are
  told apart by sorting device paths, so unplugging one renumbers the other.
  Identify each by its USB port, so a binding follows the stick in that
  port. Thrustmaster sticks report no serial number (their instance ids
  end in a port-derived 6&...&0&N), so the port is all there is. The T.16000M's left/right-handed
  switch doesn't change its USB identity, so it doesn't help.
- Head movements as desktop controls (switching virtual desktops or window
  manager panes), on Linux too.
