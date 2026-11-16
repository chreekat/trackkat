# Tracking's heuristics

Every number tracking judges by, in one place: what it is, what it decides,
where it comes from, and which others it repeats or contradicts. This is a
map for whoever tunes tracking, not a guide for users.

## The scheme

Tracking's numbers are of five kinds, and each kind is to have one home and
one currency:

| Kind | What it is | Its home |
| --- | --- | --- |
| Head fact | how far and fast a head and neck move; where the camera can see | `World` (`src/TrackKat/World.hs`), with units |
| Noise | how far off a measurement is | `World` for the primary ones; multiples of them elsewhere |
| Appearance | what a reflector's blob looks like, beside a lamp's or a fragment's | the module that judges it, for now |
| Memory | how long something learnt is kept | the module that keeps it, for now |
| Threshold | how sure a decision must be | the module that decides |

Four things hold the scheme together:

- **`World`** states each primary fact once, with its unit. A module's
  constant that is the same fact *is* that field; one that is a multiple
  says so where it is defined (`asideSpread` is half `headReachAside`). The
  tables below say which is which in the *From* column: `World.x` for the
  fact itself, `k × World.x` for a multiple, *tuned* for a number that
  stands alone.
- **`Term`** (`src/TrackKat/Term.hs`) is one judgement of a hypothesis with
  a name, scored in nats of log-likelihood: 0 for nothing against it, minus
  infinity for impossible. Terms add. A hard gate is a term that says 0 or
  minus infinity; a soft one says how unlikely, or, as the log of a ratio
  of likelihoods, how much likelier one way than another, which is more
  than 0 where the evidence is for the hypothesis. A decision made of
  terms can say which term decided, with its figure.
- **Explanations** reach whoever is looking: a tracked frame carries what
  each term of its pose's score found (`outJudged`), which hypothesis gave
  the pose and what every other scored (`outChosen`); a lost one why
  (`outWhy`: the term that counted most against the likeliest way of
  seeing the clip) and every hypothesis' score (`outRivals`); a light
  learnt why it was taken for one, a correction its evidence, and windows
  of turning are counted by what took or rejected them (`trackerWindows`).
  `tirprobe` prints them (`--explain` for every frame), and trackkat's log
  says, in the user's words, why the clip is out of sight with lights in
  view.
- **Stages** make each of the tracker's decisions in one function of just
  what it reads, in a module of its own under `src/TrackKat/Tracker/` (the
  head of `Tracker.hs` says which, in the order a frame goes through
  them). What each promises is stated as properties under
  `test/Test/Tracker/`: a hypothesis's score is the sum of its named terms
  and nothing else, each term is greatest where its figure is as expected
  and no greater further off, nothing is impossible but what is stated,
  the choice is the best scored and goes by nothing but the scores. A
  number tuned is then tested by the promises of the stage it belongs to.

Gathering the numbers changed none of them. Step 3 did: what tracking
makes of a frame is now chosen among hypotheses all scored by [one
account](#one-account), in place of gates tried in a fixed order, and
`test/Test/Scorecard.hs` pins what tracking then does over some forty
simulated scenes, so that a change to a number shows as what it does.

## Headline

About 140 numbers in 24 modules, twelve of them the tracker's stages,
counting each named constant and each literal inside a function once.
(Many gate or weigh something, so they are thresholds too; each is counted
by what it measures.) 28 are fields of `World`. Step 3 took out some
fifteen that were cuts through a distribution (the gates on a pose's fit,
turn, move and reach; the six tests of a blob's size; the price of a
reflector unseen; the score below which following was doubted) and put in
about as many: ten facts of the world its account needs, all tuned and
all in `World` (how big reflectors are, how often they hide, how many
strays a frame shows), and a handful of bounds on a frame's work. The
rest of what the account goes by is derived from those and from what was
there.

Duplicates that collapsed to one source:

| Fact | Was stated in | Now |
| --- | --- | --- |
| A frame's largest jump, 15° and 40 mm | `Tracker.Judge.maxTurn`/`maxMove`, `Response.maxStepDegrees`/`maxStepMm` | `World.headStepTurn`, `headStepMove`, for `Response` alone since step 3 |
| How far a head turns from centre | `Branch.headReachDegrees` 40 (a spread), `Tracker.Judge.maxReachTurn` 120 (a limit) | `World.headTurnSpread`; the limit is gone since step 3 |
| How far a neck is from one pivot | `Calibration.windowNoise` 2, `Calibration.windowFit` 4, `Shape.windowFit` 8 | `World.neckWander`, and 2 and 4 times it |
| Where the pivot is, head to head | `Calibration.priorSpread` 40, `Branch.pivotSpreadMM` 80 | `World.pivotSpread`, and twice it |
| A reflector's centre is good to a pixel | `Tracker.Judge.maxFit` 2, `Pose.solvePose`'s 1, `Match.missingCost` 36 | `World.centroidNoise`: 1 of it, and since step 3 the spread of term `place` |
| Blobs of one thing differ by 1.5 times | `Lights.reflectorLikeFactor`, `Lights.sameSizeFactor` | `World.sameSizeFactor` |
| 120 frames a second | `Calibration.olderWindowPoses` 60 | `windowSeconds × World.frameRate` |

## Contradictions

Each is a candidate to reconcile. Those step 3 settled say how; the rest
stand.

1. **A head's speed was stated two ways that disagreed, and neither was
   a head's speed.** *Settled.* The step a followed pose was allowed (15°
   and 40 mm a frame, and more by a rate) was an allowance for a wrong
   match, not a speed, and it gated. How far a pose turned and moved from
   the one it continues is now scored as what it is, a gaussian of the
   head's speed (`headTurnSpeed`, `headMoveSpeed`) over the time between
   (terms `turning`, `moving`), and nothing gates it: a pose that would
   need a faster head than heads are loses to the same pose come afresh
   once that is likelier, which a frame on is at about 19° or 31 mm
   (28 nats: the room between them, and the clip's having left where it
   was). The rates are gone; the step remains only as what `Response`
   allows of the pose games see (2).
2. **`Response` ignores the time between frames.** Its jump limit is 15° and
   40 mm *per frame* whatever the frame rate, while `Tracker`'s grows with
   the gap. At 60 fps `Response` allows half the speed it does at 120.
3. **Body sway is 5 mm in `Branch` and at most 4 mm in `Calibration`.**
   `Branch.swayMM` takes a 5 mm misfit to turning about the pivot as one
   ordinary spread. `Calibration.windowFit` rejects a window above 4 mm as
   "the body moved", and `windowNoise` puts the floor at 2 mm. So learning
   the pivot throws away windows the mirror-image test calls typical.
   `Shape.windowFit` allows 8 mm, for a stated reason (the shape may be
   off).
4. **A window of turning must turn 4°, 8° or 12°,** in `Branch`,
   `Calibration` and `Shape`; and a turn that teaches which way is up,
   10° within 0.6 s. `Branch` says why it wants less; `Shape` doesn't say
   why it wants more. Windows are 0.5 s in two and 1 s in `Shape`.
5. **The pivot's uncertainty is 40 mm, 80 mm, or up to 300 mm.**
   `Calibration.priorSpread` is 40 mm a direction; `Branch.pivotSpreadMM`
   lets the window's point lie 80 mm off as cheaply as one spread of sway;
   `plausiblePivot` accepts anything from 30 mm behind the reflectors to
   300 mm away. And the usual pivot is (0, 100, 100) mm from the clip in
   `Pose.standardMounting`, where the simulation's head has it at (0, 130,
   90).
6. **How far to the side a head goes is 400 mm for tracking and 300 mm for
   the view.** `World.headReachAside` against `Response.leanRange`. They
   are different things (where a head goes; where a game's view ends), so
   both stay. Since step 3 the first is no limit but two spreads of how
   far aside a head is (term `aside`), so a head 450 mm aside is tracked,
   as less likely, and sent to a game as beyond the end of its range.
7. **"Is this blob a reflector?" was asked six ways.** *Settled, for the
   tracker.* At least 15 pixels, at least 3/25 of the biggest in view, at
   least 1/8 of its reflector's usual size, at least half of it to be the
   reflector and not a stray: each cut one distribution at a different
   place. There is now one likelihood of a blob's pixels given what is
   learnt of its reflector (`Tracker.Sizes.reading`), beside the
   likelihood of a stray's, and matching, the choice among hypotheses and
   the search by layout all go by it (term `size`). What is left: the
   smallest and largest blobs kept at all (`minFaintPixels`,
   `maxBlobPixels`), which bound what a stray can be, and `Lights`' two
   uses of `sameSizeFactor`, which are about lights and not reflectors.
8. **A reflector's size is remembered for 10 s, for 600 s, and for good.**
   What the tracker learns of a size fades by e in `World.sizeMemory`, 10
   s, seen or not; `Lights.sizeMemorySeconds` fades the biggest seen over
   600 s; `Lights` also keeps the size at the centre (`lUsual`), which
   never fades.
9. **Pixel distances each have their own number:** 1 (a guess is good
   enough), 3 (a blob is drawn as its reflector's, plus its diameter), 4
   (`stillPixels`), 5 (`marginPixels`), 15 (clear of the clip, plus a
   diameter), 20 (`nearPixels`), 25 (`reachPixels`). *Fewer than there
   were:* the 2 of `maxFit`, the 6 of `missingCost` and the 10 a factor of
   e of `sizeScale` are gone, and what a reflector's centre being off
   costs is `centroidNoise` and nothing else (term `place`).
10. **Half a second is three separate constants** that happen to agree:
    `Tracker.Trail.predictionSeconds`, `Tracker.Still.confirmSeconds` and
    `Calibration.windowSeconds`. A quarter second is `Lights.gapSeconds`.
11. **Things counted in frames assume 120 a second:** `Shape.blockFrames` 8,
    `Response.jumpPersistence` 3, and the per-frame jump limits above. At
    60 fps each means twice the time.
12. **Four currencies decided.** `Branch` weighs log-odds and acts at
    99%. `Calibration` and `Shape` keep information matrices but admit
    windows by gates. `Lights` gates. *Settled, for the tracker:* `Match`
    added squared pixels and `Tracker` gated, with nats in one place; now
    both score in nats by the same function (`whatShows`), which `Match`
    calls to rank the ways of matching and the account calls to score
    them.
13. **Six ways of forgetting** (see [Memory](#memory)).
14. **A reflector's size was scored against two things,** the size it
    had in the last frame and the most it had been lately. *Settled:*
    there is one learnt size (7, 19).
15. **A reflector unseen cost the same 18 nats each frame,** as if frames
    were independent, while how long the clip might show less was a
    deadline. *Settled, as to the cost:* a reflector comes and goes as a
    thing with two states does (`Tracker.Presence`), so being unseen costs
    once for going, some 7 nats at 120 frames a second, and then by the
    time it stays away, 2 nats a second, however many frames that is. The
    deadline (`showingSeconds`) is no longer part of any score. It stands
    as a rule on what is told, and why it can't yet be a term is under
    [What comes next](#what-comes-next).
16. **A pose found by layout needn't fit the blobs.** *Settled.* Every
    hypothesis pays for how far each blob is from where its pose puts the
    reflector (term `place`). The three blobs of 106 pixels at (272, 394),
    (394, 279) and (328, 410), which were found as the clip at a pose
    fitting them to 42 pixels, are now likelier three strays, and the
    frame says so by the term.
17. **A pose refound was taken without weighing the clip still where it
    was.** *Settled.* Nothing is taken whatever it scores: every
    hypothesis about a frame is scored by the same account and the best is
    taken.
18. **The clip still where it was was impossible by a deadline, however
    much of it showed.** *Settled.* No hypothesis is impossible but by the
    camera's range. The clip with all three blobs in place is made out
    however long since all three last showed.
19. **A reflector's usual size could be less than its latest.** *Settled.*
    What is learnt of a size is one distribution, evidence that fades by e
    in `sizeMemory`, and what a blob teaches moves it toward the blob: the
    blob is never less likely after than before.

## Head facts

How heads, necks and the camera are. `World` fields first.

| Name | Value | Gates or weighs | From | Repeats or contradicts |
| --- | --- | --- | --- | --- |
| `World.frameRate` | 120 /s | `Calibration.olderWindowPoses` (60) | the TrackIR 5 | frame-counted constants assume it (11) |
| `World.cameraNearest`, `cameraFarthest` | 250, 2500 mm | `Tracker.Account.nearest`, `farthest`: a pose outside isn't the clip (terms `near`, `far`), the only gates on a pose; and the room it has anywhere along the line of sight | tuned; leaning in to 270 mm is tested | `maxBlobPixels` is sized for `nearest` |
| `World.headStepTurn`, `headStepMove` | 15°, 40 mm | `Response.maxStepDegrees`, `maxStepMm`: a bigger step of the pose games see is a jump | tuned: an allowance well beyond a head's speed | (2) |
| `World.headTurnSpeed`, `headMoveSpeed` | 300 °/s, 500 mm/s | the spreads of a head's speed: how likely a pose continues another (terms `turning`, `moving`), and how much room it had to (term `room`) | a head's fastest, about 600°/s and 1 m/s, as two spreads | |
| `World.headTurnSpread` | 40° | how likely a head is turned so far from where it was recentered (term `turned`), and the room it has anywhere (term `room`); `Branch.headReachDegrees` (term `reach`) | tuned | |
| `World.headReachAside` | 400 mm | the farthest a seated head goes to the side | tuned | (6) |
| `Tracker.Account.asideSpread` | 200 mm | how likely a head is so far to the side of where it was recentered (term `aside`), and the room it has anywhere | `World.headReachAside` / 2: the farthest at two spreads | |
| `World.clipInViewFor`, `clipAwayFor` | 60 s, 10 s | how likely the clip has gone from view, or from where it was followed, since it was made out (terms `out of view`, `left`) | tuned | |
| `World.reflectorShowsFor`, `reflectorHidesFor` | 10 s, 0.5 s | how likely a reflector is out of sight, given whether it showed and how long ago (term `shows`) | tuned | (15) |
| `World.bodySway` | 5 mm | `Branch.swayMM`: the spread a window's misfit is scored against (term `turning`) | tuned | (3) |
| `World.neckWander` | 2 mm | `Calibration.windowNoise`: the least a window's poses are taken to stray, in weighing it | tuned | (3) |
| `Calibration.windowFit` | 4 mm | a window whose still point moved more isn't learnt from (term `fit`) | 2 × `World.neckWander` | (3) |
| `Shape.windowFit` | 8 mm | the same, for the clip's shape (term `fit`) | 4 × `World.neckWander` | (3); looser as the shape may be off |
| `World.pivotSpread` | 40 mm | `Calibration.priorSpread`: the prior holding the pivot where turning hasn't shown it | tuned | (5) |
| `Branch.pivotSpreadMM` | 80 mm | how far a window's still point may be from the pivot as cheaply as one spread of sway | 2 × `World.pivotSpread` | (5) |
| `World.neckBehindClip`, `neckWithin` | 30, 300 mm | `Calibration.plausiblePivot`: a pivot elsewhere came from mirror-image poses (terms `pivot behind`, `pivot away`) | tuned | (5) |
| `World.headStillFor` | 300 s | `Lights.stillHeadSeconds`: a blob like a reflector is learnt as a light only after this (term `not the clip`) | tuned | |
| `Pose.standardMounting` pivot | (0, 100, 100) mm | where the pivot is first taken to be | anatomy, in its comment | (5) |
| `Visibility.usualHead` | centre (0, −60, −20), half-widths (75, 105, 95) mm | which reflectors the head hides (predicted; tracking doesn't use it) | tuned | |
| `Visibility.surfaceMM` | 20 mm | how far through the head a line of sight goes before the head hides a reflector | tuned | |
| `Calibration.turnDegrees`, `turnSeconds` | 10°, 0.6 s | a turn brisk enough to show an axis | tuned | (4) |
| `Calibration.turnMost` | 20° | the most one turn counts for | 2 × `turnDegrees` | |
| `Calibration.windowSeconds` | 0.5 s | a window of turning, for the pivot and the mirror image | tuned | (4), (10) |
| `Shape.windowSeconds` | 1 s | a window of turning, for the shape | tuned | (4) |
| `Branch.crossingChance` | 0.3 | how likely tracking leaves a meeting of the two poses on the other | tuned | |
| `Branch.togetherDegrees` | 8° | the two poses have met when this close | tuned | |
| `Pose.alternatives` range | 0.5 to 1.5 times the distance | which other poses are looked for | tuned | |
| `Pose.alternatives` apart | 0.5° | another pose nearer than this is the same pose | tuned | |
| `Pose.levelFromYaws` | 0.5 | too little turning to calibrate level from | tuned | |
| `Tracker.Learners.calibrationSeconds` | 5 s | how long a requested calibration watches | tuned | |
| `Response.yawRange`, `pitchRange`, `rollRange`, `leanRange` | 180°, 90°, 90°, 300 mm | the ends of the view's ranges, as games and the window have them | games; `cbits/gui.c` draws the same | (6) |
| `Config.clipShape` tabs | radius 10 mm, tape 25 mm of arc, bend 37.5°, post leaning 45° | where a reflector's light is on its tab | measured from a TrackClip | |

## Noise

How far off measurements are.

| Name | Value | Gates or weighs | From | Repeats or contradicts |
| --- | --- | --- | --- | --- |
| `World.centroidNoise` | 1 px | how likely a reflector's blob is so far from where a pose puts it (term `place`); `Pose.guessFit` | tuned | (9) |
| `Tracker.Reflectors.blobGain` | 10.8 nats | what a blob just where a reflector should be says for its being the reflector and not a stray | log of the picture's area over `World.strayBlobs` × 2π `centroidNoise`² | |
| `Tracker.Account.turnSpread`, `moveSpread` | 2.5°, 4.2 mm a frame apart | how likely a pose continues another (terms `turning`, `moving`) | `World.headTurnSpeed`, `headMoveSpeed` × the time between, a frame at least | |
| `Tracker.Account.room` | −6 nats a frame after a pose, −19 at 0.1 s, −24 at 0.5 s; −26 with nothing known | how much of where a head could be the reflectors seen leave (term `room`) | derived: see [One account](#one-account) | |
| `World.sizeNoise` | 0.1 | how far a blob's pixels are from its reflector's, as a fraction, from frame to frame (term `size`) | tuned; was `Match.sizeScale`'s 10 px a factor of e | |
| `Tracker.Sizes.pixelNoise` | `sizeNoise`² + 2√(π a) / a² | how much a blob of a pixels teaches of its reflector's size | `World.sizeNoise`, and the pixels on a blob's edge | |
| `Tracker.Follow` shift spread | 4 px a frame apart, at 1 m | how far the clip's whole picture shifts, in ranking the ways of matching (term `shift`) | `turnSpread`, `moveSpread`, the pivot's distance and the camera | ranks only |
| `Pose.holdTurn`, `holdMove` | 3 px/radian, 0.05 px/mm | how strongly a followed pose is held where it was | tuned | |
| `Tracker.Reflectors.sightWeight` shared | 0.5 | how much the place of a blob two reflectors share counts, in the fit and in term `place` | tuned | |
| `Tracker.Reflectors.sightWeight` dim | pixels over those expected, from 0.05 to 1 | how much a dim reflector's place counts | tuned | |
| `Tracker.Blobs.reflectorReach` | 3 px + a blob's diameter | a blob this near a reflector is drawn as one (`roles`) | tuned | (9) |
| `Tracker.Still.clipView` margin | 15 px + the biggest reflector's diameter | lights aren't learnt this near the clip | tuned | (9) |
| `Tracker.Mirror.clarity` alone | a reflector's diameter | a reflector with another blob this near isn't seen clearly | tuned | |
| `Lights.stillPixels` | 4 px | a blob straying further isn't a still light | "a fire's core moves less than 2" | (9) |
| `Lights.marginPixels` | 5 px | the margin of a light's area | tuned | (9) |
| `Lights.reachPixels` | 25 px | how far a light's fragments show | "a fire's flicker, about 20" | (9) |
| `Lights.activeMask` near | 20 px | a light this near a predicted reflector isn't masked | tuned | (9) |
| `Calibration.windowPoses` | 1 | how many independent poses a window is worth | tuned | |
| `Calibration.holdStrength` | 1 | how a window's own still point is held to the estimate | tuned | |
| `Shape.windowEvidence` hold | 1e-4 | the same, for the shape | tuned | `holdStrength` |
| `Shape.blockFrames` | 8 frames | poses averaged into one | tuned | (11) |
| `Shape.groups` | 8 | groups whose scatter says how sure the shape is | tuned | |
| `Response` 1€ filter | cutoff 20 to 0.2 Hz, beta 0.001 to 1, derivative cutoff 1 Hz | smoothing, from the user's two knobs | the 1€ filter's paper; ranges tuned | |

Numerical settings that are not judgements, for completeness: the solver's
50 steps and its tolerances (`Pose.levenbergMarquardt`), 3 steps to settle
an alternative, 150 steps and 30 halvings in `Pose.fitThree`, 8 Jacobi
sweeps in `Calibration.eigenSymmetric`, finite-difference steps of 1e-6
radians and 1e-3 mm.

## Appearance

What a reflector's blob looks like.

| Name | Value | Gates or weighs | From | Repeats or contradicts |
| --- | --- | --- | --- | --- |
| `Tracker.Blobs.minFaintPixels`, `maxBlobPixels` | 6, 5000 px | the smallest and largest blobs kept at all; a stray is as likely any size between, on the log scale (`Tracker.Sizes.strayDensity`) | 250 px at 950 mm is 3600 at `nearest` | (7) |
| `World.reflectorPixels`, `reflectorPixelsFactor` | 200 px at 1 m, to within 6 times | how big a reflector is before any is seen (term `size`) | tuned; 245 and 180 on the user's camera | |
| `World.sameSizeFactor` | 1.5 | how alike one clip's reflectors are in size, before any is learnt (term `alike`); when a size counts as known (`sizeKnown`); `Lights.reflectorLikeFactor`, `Lights.sameSizeFactor` | tuned; "a fire's core is about twice a reflector" | (7) |
| `World.partShowing` | 0.01 | the chance a reflector shows only part of itself: a blob smaller than expected is that unlikely, times its fraction (term `size`) | tuned | |
| `World.inPieces` | 0.05 | the chance a reflector shows as two blobs (term `pieces`) | tuned | |
| `World.strayBlobs` | 1 | how many blobs that aren't the clip's a frame shows: what a blob in a reflector's place says (`blobGain`) | tuned | |
| `Lights.fragmentFactor` | 2 | a blob near a light and half its size or less is a fragment of it | tuned | |
| `Match.bigEnoughForTwo` | area² > bigger × both | a shared blob no bigger is perhaps one reflector, for how long a pose is told (`showingSeconds`) | the geometric mean, on the log scale sizes are compared on | (7) |
| `Tracker.Sizes.sharedReading` | from the bigger to both | a blob two reflectors share is as likely any size between (term `size`) | two discs overlap by any amount | |
| `Match.reach` | 1.5 × the two radii; 10 px a radius unknown | two reflectors predicted this close may share a blob | touching, and half again for movement | |
| `Match.together` | the radius of a disc of both areas, from the centre of both | two blobs each with its centre this close may be pieces of one reflector | geometry: a piece of a disc has its centre inside it | |
| `Match.maxBlobs` | 8 | blobs considered | tuned | |
| `Match.matches` options | 4 a reflector; 2 blobs for a shared pair; 1 for the third | candidates tried | tuned | |
| `Tracker.Layout.clipSearch`, `maskedSearch` | 5, 4 | blobs searched for the clip by layout, and masked blobs tried in place of blobs missing | tuned: bounds on a frame's work | |
| `Tracker.Follow.followTries`, `rankWindow` | 6, 30 nats | ways of matching solved for a pose, and how far below the best ranked they may rank | tuned: bounds on a frame's work | |
| `Tracker.Choose.boundSlack` | 2 nats | how far below the best so far the most a way could score must be for it not to be solved | tuned: the most is reckoned at a few distances | |
| three blobs | 3 | making out the clip by layout wants three (term `blobs`) | the clip | |
| reflector size at 1 m | learnt (`Tracker.Sizes`: evidence fading by `World.sizeMemory`) | term `size` | about 245 px lower, 180 top, in the simulation (`Synth.lowerRadius`, `topRadius`) | (8) |

## Memory

How long things are kept. Six ways of forgetting are in use:

| Way | Where |
| --- | --- |
| A deadline: kept whole, then gone | `predictionSeconds`, `showingSeconds`, `confirmSeconds`, `Lights.learnSeconds`, `gapSeconds`, `forgetSeconds`, `clipMemorySeconds`, `Calibration.turnSeconds`, `longestStep` |
| Fading with time, by e each so many seconds | `World.sizeMemory`, `Lights.sizeMemorySeconds` |
| A cap on how much is kept, the old scaled down as new comes | `Calibration.upMemory` (degrees), `Shape.memory` (windows), `Calibration.countMemory` (windows) |
| A cap on how sure, direction by direction | `Calibration.pivotMemorySpread` |
| The last few | `Branch.placesRemembered` |
| A floor under probabilities | `Belief.leastChance` |

| Name | Value | Keeps | From | Repeats or contradicts |
| --- | --- | --- | --- | --- |
| `Tracker.Trail.predictionSeconds` | 0.5 s | a pose predicts the reflectors, and one left for a pose found afresh may be resumed; after, the clip is found afresh only | tuned | (10) |
| `Tracker.Still.confirmSeconds` | 0.5 s | tracking this long before lights are learnt clear of the clip | tuned | (10) |
| `Tracker.Follow.showingSeconds` | 1 s one reflector out; 2.5 s one perhaps behind another | how long a pose is told of the clip showing less (rule `partly seen`) | tuned | a rule on what is told, not a score (15) |
| `World.sizeMemory` | 10 s | what is learnt of a reflector's size: its evidence weighs less by e | tuned | (8) |
| `Lights.learnSeconds` | 3 s | still this long, a blob is a light (term `still`) | tuned | |
| `Lights.gapSeconds` | 0.25 s | how long a light may vanish while watched; how old a frame's sights may be | tuned | (10) |
| `Lights.forgetSeconds` | 45 s | a light unseen | tuned | |
| `Lights.clipMemorySeconds` | 10 s | where the reflectors were is kept clear of learning | tuned | equals `World.sizeMemory` by chance |
| `Lights.sizeMemorySeconds` | 600 s | the biggest reflector seen | tuned | (8) |
| `Calibration.upMemory` | 1800° | turning that says which way is up | "a couple of minutes of looking about" | |
| `Calibration.pivotMemorySpread` | 3 mm | evidence for the pivot, to this sureness a direction | tuned | |
| `Calibration.countMemory` | 1000 windows | the count of windows told | tuned | counts what `pivotMemorySpread` forgets otherwise |
| `Calibration.windowInfo` older builds | a tenth of what is remembered | how much a calibration saved by an older build counts | tuned | |
| `Calibration.upRate`, `pivotRate` | 0.25 °/s, 2 mm/s | how fast what is applied follows what is learnt | slow enough not to show | |
| `Calibration.longestStep` | 0.1 s | the longest gap what is applied moves over | tuned | |
| `Shape.memory` | 600 windows | evidence for the shape | "about ten minutes of turning" | |
| `Branch.placesRemembered`, `placeDegrees` | 4, 15° | where windows lately ended, so that fidgeting in one place counts once | tuned | |
| `Belief.leastChance` | 0.001 | no belief falls below, so any can come back | tuned | |
| `Response.jumpPersistence` | 3 frames | a jump lasting this long is real | tuned | (11) |
| `Tracker.Absence.absenceSeconds`, `absenceQuiet` | 1 s, 10 s | how long out of sight before the user is told why, and how seldom | tuned | |

## Thresholds

How sure a decision must be.

| Name | Value | Decides | From | Repeats or contradicts |
| --- | --- | --- | --- | --- |
| `Branch.sure` | 0.99 | switching a pose for its mirror image; learning from a pose | tuned | (12) |
| `Branch.windowWeight` | 3 nats | the most one window, or one place, moves the odds | tuned | |
| `Branch.windowDegrees` | 4° | a window that turned less says nothing (term `turn`) | tuned | (4) |
| `Branch.foundAfresh` | 0.5 | found afresh, either pose as likely | symmetry | |
| `Calibration.windowSpan` | 8° | a window that turned less isn't learnt from (term `turn`) | tuned | (4) |
| `Shape.windowSpan` | 12° | the same, for the shape (term `turn`) | tuned | (4) |
| `Calibration.upConfident` | 180° | turning before the learnt up is applied or told | tuned | |
| `Calibration.pivotConfident` | 10 windows | windows before the pivot is told | tuned | |
| `Calibration.upNews`, `pivotNews` | 0.5°, 5 mm | changes worth telling | tuned | |
| `Shape.newsMM` | 1.5 mm | a change in the shape worth telling | tuned | |
| `Shape.confidence` | 1 minute and 0.5 mm | an estimate sure enough to tell | tuned | |
| `Shape.difference` | 2 mm and 4 spreads | an estimate different from what is configured | tuned | |
| `Shape.solveEvidence` | 1e-9 | evidence too thin to solve | numerical | |

| `Tracker.Choose.sureness` | log 9, 2.2 nats | how much likelier than the clip away a pose come afresh must be, to be taken (rule `sure`) | tuned | |

The gates among the head facts above are thresholds too: each row that
names a term is a decision.

## One account

Every hypothesis about a frame is scored by one account
(`Tracker.Account`, with its Note [One account]), and the best scored is
what the frame is taken to show (`Tracker.Choose`). The score is how much
likelier the hypothesis makes the frame than every blob being a stray
with no clip to explain any, in nats, and it is a sum of three parts,
each a sum of named terms:

    score = where the head is
          + how it got there
          + what the frame shows, given that

| Part | Terms | Each is |
| --- | --- | --- |
| Where (`whereItIs`) | `near`, `far` | gates: the camera's range, the only things that can't be |
| | `turned`, `aside` | gaussians about where the head was recentered, of `headTurnSpread` and `asideSpread` |
| How, continuing a pose (`howItCame`) | `turning`, `moving` | gaussians of `headTurnSpeed` and `headMoveSpeed` over the time since |
| | `room` | the log of the share of the poses there was room for that fit what is seen: derived, below |
| | `in view` | the chance the clip is still in view since it was made out |
| How, come afresh | `room` | the same, of the room a head has anywhere |
| | `left`, or `in view` | the chance the clip has left where it was followed since it was made out; never made out, that it is in view |
| | `doubt` | what the frames have said, lately, for the clip being elsewhere: below |
| Shows, each reflector seen (`whatShows`) | `place` | `blobGain`, less half the square of how many `centroidNoise` its blob is from where the pose puts it |
| | `size` | the log of how much likelier its blob's pixels are of the reflector, as learnt, than of a stray |
| | `shows` | the chance it is in sight, given whether it was and how long ago |
| | `pieces` | of a reflector seen as two blobs: the chance of that, and what the second blob gains |
| Shows, a reflector unseen | `shows` | the chance it is out of sight |
| Shows, together | `alike` | what the three sizes being alike adds, before any is learnt |

The hypotheses of a frame are: the clip made out near where the last pose
predicts it, for each of the best ranked ways of matching blobs to
reflectors (up to `followTries`, six); the clip made out by layout, for
each three of the heaviest `clipSearch` blobs in view (five, so ten
threes); each of those as continuing the last pose, as resuming a pose
left a moment ago for one come afresh, and as come afresh; and the clip
away, every blob a stray. That is at most 49. A way is solved for its
pose only if the most it could score is within `boundSlack` of the best so
far, which in an ordinary frame leaves one solve.

Where the constants come from. Derived: `blobGain` (the picture's area
over a stray's rate and a centre's noise), `room` (the spreads above and
the camera's geometry), `turnSpread` and `moveSpread`, what being unseen
costs (the two times a reflector shows and hides for, and the time
between frames). Tuned, all in `World`: `strayBlobs`, `reflectorPixels`
and its factor, `sizeNoise`, `partShowing`, `inPieces`, `sizeMemory`, the
four times things come and go by, and what was there before
(`centroidNoise`, the head's speeds and spreads, the camera's range).

Room. A hypothesis that says "near where it was" and one that says
"anywhere a head can be" both end up naming one pose, the one that fits
the blobs; the second had far more poses to choose from and would have
fitted almost any three blobs. What a hypothesis is worth is its
likelihood summed over every pose it allows, which is the likelihood at
the best pose times the share of the allowed poses that fit as well.
`room` is the log of that share: −½ log det (I + S Jᵀ W J S / σ²), with S
the spreads of the pose's six freedoms (`followingRoom`, `anywhereRoom`),
J how far each freedom moves each reflector seen in the image
(`Pose.poseInformation`), W their weights and σ `centroidNoise`. It is −6
nats a frame after a pose and −26 with nothing known, with three
reflectors seen a metre off, and the 19 between, with the 9 of `left`, is
what makes a pose that continues the last likelier than the same pose
come afresh: it replaces
the `afresh` cost of step 2, which was the same ratio reckoned from
boxes. A reflector unseen fixes less, so costs less room.

Coming and going. A reflector that goes out of sight stays out for a
while, so being unseen is not a price per frame. A thing with two states
(`Tracker.Presence`) goes at one over the time it usually stays and comes
back at one over the time it stays away, and the chance of either after
any time follows. For a reflector (10 s showing, 0.5 s hidden): going
between two frames at 120 a second costs 7.1 nats, and each second away
after that 2. For the clip (60 s in view, 10 s away): going from view, or
from where it was followed, costs 8.9 nats a frame after it was made out,
and less the longer since. None of it depends on the frame rate but
through the time between frames.

Sizes. What is learnt of a reflector's size is one gaussian over the log
of its pixels at 1 m (`Tracker.Sizes`), kept as evidence that fades by e
in `sizeMemory`; before any, it is `reflectorPixels` to within its
factor, and one clip's three are alike to within `sameSizeFactor`. A
blob's pixels are that gaussian to within `sizeNoise`, or, with
`partShowing`'s chance, any fraction of it, since a reflector dims at a
slant; a blob teaches by how likely it is all of its reflector, so a
dimming reflector doesn't teach that it is small. A stray is as likely
any size blobs are kept at.

Doubt. Each frame is scored by itself, so a pose followed onto the wrong
blobs would be kept for as long as each frame's evidence against it
stayed under what it costs the right ones to be somewhere the last pose
doesn't say. `Tracker.Choose.doubted` keeps one sum from frame to frame:
by how many nats the frames have been likelier with the reflectors seen
as some three blobs than as the pose followed sees them, counting what
shows, where, and how far the pose followed had to turn and move. Each
frame adds the best rival's lead, the sum never goes below nothing (it
is Page's cumulative sum for detecting a change, and has no memory to
tune), and a pose come afresh as three blobs is credited with it. A lamp
as big as a reflector, taken for it while it was dark, is left a fifth
of a second after the reflector shows again, as the head turns and the
lamp doesn't.

Two rules stand outside the scores, on what is told:

- `sure`: a pose come afresh is taken only if nine times likelier than
  the clip being away (`sureness`). One frame of three blobs laid out
  like the clip says only so much for it, some 5 to 15 nats when it is
  the clip, and a pose taken wrongly is recentered on and teaches the
  sizes.
- `partly seen`: no pose is told of the clip showing less than all three
  reflectors for longer than `showingSeconds` since all three showed, nor
  come afresh showing less. Two reflectors are as likely the clip after
  ten seconds as after one; what wears out is how well they say where the
  head is.

What continues. A pose that continues the last carries on the belief
about the mirror image and its window of turning, the time tracked that
lets lights be learnt clear of the clip, and what learns from poses. One
come afresh starts them over (`Branch.foundAfresh`); the pose it left is
kept for `predictionSeconds`, and a pose that continues *that* one is
**resumed**, taking up its belief and time tracked as if the frames
between had been lost.

What it doesn't do:

- It doesn't follow two poses. Doubt takes this frame's rival for the
  last frame's, and a frame showing the clip whole somewhere else is
  taken there for that frame (and resumed the next), where carrying both
  poses with their odds would wait to see which lasts.
- It doesn't know how sure the pose was a frame ago, which is why
  `partly seen` is a rule.
- A lamp as big as a reflector, taken for it, is told from it only by
  the sizes the pose then implies and by how the pose has to move: held
  still, nothing says which is which.
- Splitting a blob two reflectors share still takes each for a round
  disc (see the backlog).

## What is a term, and what isn't yet

| Decision | Terms | State |
| --- | --- | --- |
| Where a head is (`Tracker.Account.whereItIs`) | `near`, `far`; `turned`, `aside` | terms: two gates, two likelihoods |
| How a pose came to be (`Tracker.Account.howItCame`) | `turning`, `moving`, `room`, `in view`, `left`, `doubt` | terms, all likelihoods |
| What a frame shows (`Tracker.Reflectors.whatShows`) | `place`, `size`, `shows`, `pieces` for each reflector; `alike` | terms, all likelihoods |
| What to make of a frame (`Tracker.Choose.makeOut`) | the three above, summed | the best scored hypothesis is taken; the rest are told with their scores |
| A pose come afresh is sure enough (`Tracker.Choose.surely`) | `sure` | a threshold on log-odds, outside the scores |
| How long a pose is told of the clip showing less (`Tracker.Follow.partlySeen`) | `partly seen` | a rule, outside the scores |
| Making out the clip by layout wants three blobs (`Tracker.Layout.threeBlobs`) | `blobs` | a gate: fewer fix no pose |
| How the ways of matching rank (`Match.matches`) | what a frame shows, and `shift` | terms; the rank says which are solved, and decides nothing more |
| Which matches exist (`Match.matches`) | none | rules, where they are what can be: two reflectors seen, no blob twice, two share a blob only within `reach` |
| A watched blob is a light (`Lights.learnLights`) | `still`, `not the clip` | terms; `not the clip` wraps three reasons, told in `Learning` |
| A blob is watched at all, a light forgotten, a light moved (`Lights`) | none | rules; a light moved says how (`Moving`) |
| A window says which pose is the head's (`Branch.watch`) | `turn`, `turning` | terms; the log-odds is a difference of scores |
| Where the head would be (`Branch.judged`) | `reach` | a term |
| Acting on the belief (`Branch.correction`, `settled`) | none | thresholds on probabilities; a correction says its evidence |
| A window teaches the pivot (`Calibration.addWindow`) | `turn`, `fit`, `window's pivot behind`, `window's pivot away`, `pivot behind`, `pivot away` | terms; counted in `learnerWindows` |
| A window teaches the shape (`Shape.windowEvidence`) | `turn`, `no still point`, `fit`, `window's pivot behind`, `window's pivot away` | terms; counted in `shapeWindows` |
| A turn teaches which way is up (`Calibration.addTurn`) | none | a rule |
| A jump in the pose games see (`Response.shape`) | none | a rule |

## What comes next

The scheme has four steps. Three are done: the inventory and `World`;
terms with explanations wrapped round the decisions as they were; and one
account by which every hypothesis about a frame is scored, the best
taken. What is left:

**Following two poses.** Doubt is the first evidence carried from frame to
frame, and it carries one number. The same test done properly keeps the
rival as a pose of its own, followed beside the first with its own
continuity, and the log-odds between them; `LeftBehind` is its seed. That
would settle a frame of the clip whole somewhere else (taken for a frame
now, and resumed), a jump (doubted for a frame or two now), and a light
as big as a reflector held still.

**How sure the pose is.** `room` already reckons how finely a frame fixes
a pose. Carried from frame to frame, as the pose's own spread, it says
when two reflectors no longer say where the head is, and `partly seen`
becomes a term; and it is what the learners should take in place of
their `Clear` and `settled` gates.

**Still open from the list:** the sway of contradiction 3, so that
`Branch` and `Calibration` score a window by one spread; `Response`'s
step by time (2); and the size tests of `Lights` (7, 8).

**Step 4: one way of remembering.** The six ways of forgetting become one:
evidence with a weight that fades at a stated rate, in seconds. The
tracker's sizes are now kept so (`Tracker.Sizes`), which is the type the
rest want: `Lights`' biggest reflector, the level's scatter, the pivot's
information and the shape's evidence are all sums that can fade alike;
deadlines become the time for a weight to fall below use. Contradictions
8 (10 s, 600 s, never) and 11 (frames for time) settle as it is adopted.
