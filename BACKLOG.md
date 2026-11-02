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
- With the top tab bent, as on the user's clip, nodding through where the
  triangle squarely faces the camera has tracking slip 10-20 degrees onto
  another pose that fits, for a quarter second each time the head passes,
  and lose the clip briefly; points, and tabs with the top one unbent, pass.
  It happens in simulation (the clip worn pitched 30 degrees forward, the
  head pitched 20 and glancing) even with the tape drawn exactly as the
  tracker models it, so it is the tracking near the meeting of the poses,
  not the model of the tape. The mirror-image tests in `clipChecks` leave
  those clips out until it is fixed.
- On tabs, the clip's lean is more than a hint for the mirror image: it says
  which way the tape faces from the triangle. In simulation, a post 30
  degrees more upright than configured leaves the mirror image uncorrected
  on the user's clip and the scalene one, and with the top tab unbent, the
  pose 3-5 degrees and 10-35 mm off. Estimate it, or ask for it.
- Even without locking on, level learnt from a minute of glancing that nods
  and rolls too is off by up to about 2 degrees (0.6-1.8 to the right in simulation, for every clip
  shape), from how the turns' axes mix in nods; three minutes bring it to
  0.3. Turning left and right alone teaches it within a tenth of a degree.
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

- Predict which reflectors are out of sight in tracking: `TrackKat.Visibility`
  says which tape faces away and which the head hides, but in simulation
  leaving those out of matching, and following two reflectors longer while
  the third is predicted hidden, didn't help (see Note [Seeing the
  reflectors]). Try again once merged blobs split well looking up, where it
  made poses wrong less often but lost the clip more.
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
