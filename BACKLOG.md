# Backlog

Roughly in order.

- Straighten tracking's heuristics, what is left of it (see
  [docs/heuristics.md](docs/heuristics.md), "What comes next"): follow two
  poses with the odds between them, in place of the one sum of doubt;
  carry how sure the pose is from frame to frame, so that how long two
  reflectors are followed is a term and not a rule; then one way of
  remembering, evidence that fades at a stated rate, in place of six.
- Show why the clip is out of sight in the window, not only the log: the
  Clip light's "Out of sight (3 of 3)" has no room for "too far to the side
  of where you recentered" (`describeWhy`), so it wants a second line or
  the head view's note.
- Once ignoring still lights has proven itself, make it always on and drop
  the `ignore lights` setting and its checkbox.
- Split merged blobs of elongated reflectors: `splitMerged` takes each
  reflector for a round disk, but tape on a curved tab shows wider than tall.
  In simulation, looking up 30 degrees while turning, the top reflector runs
  into a lower one, the split is off, and tracking goes 10-20 degrees wrong
  and is lost, for points and tabs alike; round blobs pass the same place.
- With the top tab bent, as on the user's clip, nodding through where the
  triangle squarely faces the camera has tracking slip 10-20 degrees onto
  another pose that fits, for a quarter second each time the head passes,
  and lose the clip briefly; points, and tabs with the top one unbent, pass.
  It happens in simulation (the clip worn pitched 30 degrees forward, the
  head pitched 20 and glancing) even with the tape drawn exactly as the
  tracker models it, so it is the tracking near the meeting of the poses,
  not the model of the tape. Since poses are chosen by score it slips for
  far fewer frames (the scorecard in `Test.Scorecard` has 10 wrong of 1441
  where it had 175), but it slips. The mirror-image tests in `Test.Clips`
  leave those clips out until it is fixed.
- On tabs, the clip's lean is more than a hint for the mirror image: it says
  which way the tape faces from the triangle. In simulation, a post 30
  degrees more upright than configured leaves the mirror image uncorrected
  on the user's clip and the scalene one, and with the top tab unbent, the
  pose 3-5 degrees and 10-35 mm off. Estimate it, or ask for it.
- Ground truth from a webcam: record it alongside the camera and estimate the
  head pose from the face, to check poses, the learned pivot and level, and the
  reflectors' shift.
- Aim in the window: its curves and smoothing are only in `trackkat.conf`,
  and a binding's `hold` or `double` is typed after it in its list rather
  than chosen.

- For a second kind of clip (e.g. the side-mounted, LED TrackClip PRO),
  once one is in hand to check against:
  - markers named by place on the clip, told apart by the triangle's
    geometry, not by where they appear in the image (`Model`'s left, right,
    and top);
  - one appearance model per kind of marker (point, tape on a curved tab, LED
    that dims off-axis), used alike by the solver, matching, the still-light
    mask's size rule, and the camera view;
  - the shape estimate as "hold one edge, place the other points";
  - clip presets as data (`clip = trackclip`, `trackclip-pro left`, or
    points and a mounting).

## Low priority

- Everyday learning takes up as the main axis of turning nearest the
  camera's own up (see Note [Learning up]), so a camera tipped or rolled
  more than 45 degrees, which Calibrate now can still level, has its level
  pulled onto the axis of nods or tilts as turning goes on. Keep the
  calibrated up as the axis to stay nearest, if such cameras turn up.

- Predict which reflectors are out of sight in tracking: `TrackKat.Visibility`
  says which tape faces away and which the head hides, but in simulation
  leaving those out of matching, and following two reflectors longer while
  the third is predicted hidden, didn't help (see Note [Seeing the
  reflectors]). Try again once merged blobs split well looking up, where it
  made poses wrong less often but lost the clip more.
- Tune the RTS only if the status line's latency and GC figures show a need.
- Apply the clip shape estimated from movement (see docs/details.md), once
  it has proven itself on real clips: re-solve its linearization about the
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

## Promises that don't hold yet

Things the code looks as if it promises, or that would be good to promise,
that tests found false. None is fixed.

- A frame is dropped if its stripe bytes happen to spell the next frame's
  header.
- The mirror-image belief's least chance (`Belief.leastChance`) is applied
  before scaling, so strong evidence goes below it; `normalise` is not
  idempotent.
- A saved level more than about 45° from the camera's reloads as a
  perpendicular one.
- `verdict`'s accepted score and `score` sum in different orders and can
  differ in the last bit; a NaN score is accepted.
- `rotationVector` loses turns below about 1e-7 rad; `rotateToward` does
  not turn toward the exactly opposite direction; `levelFromUp (V3 0 0 1)`
  is not a rotation.
- Not every `Config` value survives render-then-parse: out-of-range
  steadiness, empty or sub-0.0001 Points, joystick names ending in "(n)".
- `parseNumber` is not correctly rounded and accepts "--5"; `renderNumber`
  can write "-0".
- A recording cut inside its first entry line is refused whole.
- A Points curve can turn back by one ulp at a knot.
- `Belief.weighed` given a score that is impossible both ways, alike and
  unalike, gives no belief but not-a-numbers; nothing gives it one today,
  as how far a head is turned is never scored impossible.
- `NoTrim` and a trim of zero disagree once the curved view passes half a
  turn.
- `solvePose` promises reprojection, not pose: up to about 0.07° and
  0.16 mm off at under 1e-3 px.
- `parseVersion "0.1" < parseVersion "0.1.0"`.
- `matches` considers only the first 8 blobs, in the order given.
- The most a way of making out the clip is reckoned to score, before it is
  solved, is right only to within a slack (`Tracker.Choose.boundSlack`):
  what the sizes say is reckoned at a few distances, not all.
- One frame of the clip's three blobs, with nothing else known, is at
  some poses less than nine times likelier the clip than not, nearer than
  about 400 mm, and isn't taken until it is.
- A settings reload that changes nothing still rebinds hotkeys and
  re-shows settings.
- Keys and buttons are only looked at when frames arrive, so they do nothing
  while the camera is away, and then every press made meanwhile acts at
  once when it is back, up to 64 presses and releases; and a hold begins,
  and a tap that waited on a double is made, only at a frame.
- A key whose release Windows never tells of, as when the screen is locked
  while it is down, stays held, and aiming its hold began stays on, until
  the key is next pressed.
- With two keys down that each aim while held, letting go of either ends
  aiming.
- `describeChanges` says a list of bindings emptied as `trim = `, with a
  space after it.
- Pause after a recenter with the clip out of sight holds the centered
  view, not the pose games last saw.
- A "Check for updates" click with no network still puts off the next
  automatic look by four hours.
- The trim log line can print "-0".
- An unwritable settings folder kills trackkat at start with nothing in
  the window.
- A failed write of the served-game file is never retried.
- Camera diagnostics ("# …" lines) are developer lines but go to the
  window log.
