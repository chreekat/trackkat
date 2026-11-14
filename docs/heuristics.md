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

Three things hold the scheme together:

- **`World`** states each primary fact once, with its unit. A module's
  constant that is the same fact *is* that field; one that is a multiple
  says so where it is defined (`maxFit` is twice `centroidNoise`). The
  tables below say which is which in the *From* column: `World.x` for the
  fact itself, `k × World.x` for a multiple, *tuned* for a number that
  stands alone.
- **`Term`** (`src/TrackKat/Term.hs`) is one judgement of a hypothesis with
  a name, scored in nats of log-likelihood: 0 for nothing against it, minus
  infinity for impossible. Terms add. A hard gate is a term that says 0 or
  minus infinity; a soft one says how unlikely. A decision made of terms can
  say which term rejected, with its figure and limit.
- **Explanations** reach whoever is looking: a lost frame carries why
  (`outWhy`), a tracked one what it was judged by (`outJudged`) and which
  hypothesis gave its pose (`outChosen`), a light learnt why it was taken
  for one, a correction its evidence, and windows of turning are counted by
  what took or rejected them (`trackerWindows`).
  `tirprobe` prints them (`--explain` for every frame), and trackkat's log
  says, in the user's words, why the clip is out of sight with lights in
  view.

Gathering the numbers changed none of them: every value was what tracking
used before, and the tests' figures and replays were the same to the last
digit. The contradictions found are listed, and mostly not fixed. Since
then one decision is made by score, and changed what tracking does:
[following or finding afresh](#following-or-finding-afresh).

## Headline

About 140 numbers in ten modules, counting each named constant and each
literal inside a function once: 47 head facts, 30 of noise, 21 of
appearance, 25 of memory and 20 thresholds. (Many of the first three kinds
gate something, so they are thresholds too; each is counted by what it
measures.) Choosing between following and finding afresh added seven of
them, one tuned pair (a head's speed) and the rest derived. 19 are now
fields of `World`. 26 literals that stood in the
modules are now those fields (19) or named multiples of them (7). Eight
literals that were inline in `Tracker`, `Match`, `Lights` and `Pose` have
names. The rest are tuned and stand alone.

Duplicates that collapsed to one source:

| Fact | Was stated in | Now |
| --- | --- | --- |
| A frame's largest jump, 15° and 40 mm | `Tracker.maxTurn`/`maxMove`, `Response.maxStepDegrees`/`maxStepMm` | `World.headStepTurn`, `headStepMove` |
| How far a head turns from centre | `Branch.headReachDegrees` 40 (a spread), `Tracker.maxReachTurn` 120 (a limit) | `World.headTurnSpread`; the limit is 3 spreads |
| How far a neck is from one pivot | `Calibration.windowNoise` 2, `Calibration.windowFit` 4, `Shape.windowFit` 8 | `World.neckWander`, and 2 and 4 times it |
| Where the pivot is, head to head | `Calibration.priorSpread` 40, `Branch.pivotSpreadMM` 80 | `World.pivotSpread`, and twice it |
| A reflector's centre is good to a pixel | `Tracker.maxFit` 2, `Pose.solvePose`'s 1, `Match.missingCost` 36 | `World.centroidNoise`: 2 of it, 1 of it, 6 of it squared |
| Blobs of one thing differ by 1.5 times | `Lights.reflectorLikeFactor`, `Lights.sameSizeFactor` | `World.sameSizeFactor` |
| 120 frames a second | `Calibration.olderWindowPoses` 60 | `windowSeconds × World.frameRate` |

## Contradictions

Each is a candidate to reconcile. None was changed.

1. **A head's speed was stated two ways that disagreed, and neither was
   a head's speed.** `headStepTurn` lets a followed pose turn 15° between
   two frames (1800°/s at 120 fps), and `headTurnRate` adds 100°/s for the
   time between them. At one frame apart the rate adds 0.8° to the 15°:
   the limit is nearly all allowance, and the allowance is not a head's
   speed but what a wrong match looks like. Likewise 40 mm a frame
   (4.8 m/s) beside 200 mm/s. *Split, not yet reconciled:* how fast a
   head goes is now its own fact, `headTurnSpeed` and `headMoveSpeed`
   (spreads of 300°/s and 500 mm/s, the fastest at two), and the step and
   rate are named as the allowance they are. The gates `turn` and `move`
   still stand on the allowance, unchanged; what scores how likely a pose
   continues the last goes by the speed (see
   [Following or finding afresh](#following-or-finding-afresh)). The speed
   says where the allowance should be, which is close to where it is:
   a pose 16.7° or 28 mm from the last, a frame on, is as likely found
   anywhere as followed. Putting the gates there is the softening of
   step 3.
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
   the view.** `Tracker.maxReachMove` against `Response.leanRange`. They
   are different things (where blobs can be the clip; where a game's view
   ends), so both stay, but a head 350 mm aside is tracked and sent to a
   game as beyond the end of its range.
7. **"Is this blob a reflector?" is asked six ways.** At least 15 pixels
   and at most 5000 (`minBlobPixels`, `maxBlobPixels`), or 6 when one is
   predicted nearby (`minFaintPixels`); at least 3/25 of the biggest blob in
   view (`besideBiggest`); at least 1/8 of its reflector's usual size
   (`smallestShare`); at least 1/2 of it, to be the reflector and not a
   stray (`Match.wholeFactor`); no more than 1.5 times the biggest reflector
   lately (`reflectorLikeFactor`); within 1.5 times a light's size
   (`sameSizeFactor`). 3/25 and 1/8 are the same idea at 0.12 and 0.125.
   None knows the others, and none is a likelihood: a reflector's size has
   a distribution, learnt per reflector in `trSizes`, and each test cuts it
   at a different place.
8. **A reflector's size is remembered for 10 s, for 600 s, and for good.**
   `Tracker.sizeMemory` fades the usual size over 10 s out of sight;
   `Lights.sizeMemorySeconds` fades the biggest seen over 600 s; `Lights`
   also keeps the size at the centre (`lUsual`), which never fades.
9. **Pixel distances each have their own number:** 1 (a guess is good
   enough), 2 (`maxFit`), 3 (a blob is its reflector's, plus its diameter),
   4 (`stillPixels`), 5 (`marginPixels`), 6 (`missingCost`), 10 a factor of
   e in size (`sizeScale`), 15 (clear of the clip, plus a diameter), 20
   (`nearPixels`), 25 (`reachPixels`). Only 1, 2 and 6 derive from
   `centroidNoise`.
10. **Half a second is three separate constants** that happen to agree:
    `Tracker.predictionSeconds`, `Tracker.confirmSeconds` and
    `Calibration.windowSeconds`. A quarter second is `Lights.gapSeconds`.
11. **Things counted in frames assume 120 a second:** `Shape.blockFrames` 8,
    `Response.jumpPersistence` 3, and the per-frame jump limits above. At
    60 fps each means twice the time.
12. **Four currencies decide.** `Branch` weighs log-odds and acts at 99%.
    `Match` adds squared pixels. `Tracker` and `Lights` gate: yes or no.
    `Calibration` and `Shape` keep information matrices but admit windows
    by gates. Terms put the first three in nats; the gates are still
    gates, but `Tracker` now also compares in nats, between following and
    finding afresh.
13. **Six ways of forgetting** (see [Memory](#memory)).
14. **A reflector's size is scored against two things.** Matching scores a
    blob against the size its reflector had in the last frame (`sizes`);
    choosing between following and finding afresh scores it against the
    most it has been lately (`usual size`), because the latest is whatever
    the last frame took for the reflector, which is what is in question.
    Both use `Match.sizeScale`. One likelihood of a blob's size under what
    was learnt of its reflector would serve both (7).
15. **A reflector unseen costs the same 18 nats each frame,** in a match
    and in the clip being still where it was (`stillThere`), as if frames
    were independent; while how long the clip may show less is a deadline
    (`showingSeconds`). A reflector that fades stays faded: the cost
    belongs to its going, and a rate to its staying.

## Head facts

How heads, necks and the camera are. `World` fields first.

| Name | Value | Gates or weighs | From | Repeats or contradicts |
| --- | --- | --- | --- | --- |
| `World.frameRate` | 120 /s | `Calibration.olderWindowPoses` (60) | the TrackIR 5 | frame-counted constants assume it (11) |
| `World.cameraNearest`, `cameraFarthest` | 250, 2500 mm | `Tracker.nearest`, `farthest`: a pose outside isn't the clip (terms `near`, `far`) | tuned; leaning in to 270 mm is tested | `maxBlobPixels` is sized for `nearest` |
| `World.headStepTurn`, `headStepMove` | 15°, 40 mm | `Tracker.maxTurn`, `maxMove` at no gap (terms `turn`, `move`); `Response.maxStepDegrees`, `maxStepMm` | tuned: an allowance, below what a wrong match looks like | (1), (2) |
| `World.headTurnRate`, `headMoveRate` | 100 °/s, 200 mm/s | how `Tracker.maxTurn`, `maxMove` grow with the gap between sightings | tuned: likewise an allowance | (1) |
| `World.headTurnSpeed`, `headMoveSpeed` | 300 °/s, 500 mm/s | the spreads of a head's speed: how likely a pose continues the last (terms `turning`, `moving`), and what finding afresh gives up (term `afresh`) | a head's fastest, about 600°/s and 1 m/s, as two spreads | (1) |
| `World.headTurnSpread` | 40° | `Branch.headReachDegrees`: where a head would be weighs which pose is the mirror image (term `reach`) | tuned | `Tracker.maxReachTurn` |
| `Tracker.maxReachTurn` | 120° | a pose turned further from the centre isn't the clip (term `turned`) | 3 × `World.headTurnSpread` | `Response.yawRange` 180 is the view's, not the head's |
| `World.headReachAside` | 400 mm | `Tracker.maxReachMove`: a pose further to the side of the centre isn't the clip (term `aside`) | tuned; vetoed good blobs before depth was let go | (6) |
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
| `Tracker.calibrationSeconds` | 5 s | how long a requested calibration watches | tuned | |
| `Response.yawRange`, `pitchRange`, `rollRange`, `leanRange` | 180°, 90°, 90°, 300 mm | the ends of the view's ranges, as games and the window have them | games; `cbits/gui.c` draws the same | (6) |
| `Config.clipShape` tabs | radius 10 mm, tape 25 mm of arc, bend 37.5°, post leaning 45° | where a reflector's light is on its tab | measured from a TrackClip | |

## Noise

How far off measurements are.

| Name | Value | Gates or weighs | From | Repeats or contradicts |
| --- | --- | --- | --- | --- |
| `World.centroidNoise` | 1 px | `Pose.guessFit`: a pose refined from the last is taken if it fits this well | tuned | (9) |
| `Tracker.maxFit` | 2 px | a followed pose fitting worse isn't the clip (term `fit`) | 2 × `World.centroidNoise` | (9) |
| `Tracker.misfit` spread | 0.577 px | how likely a pose's reflectors are seen so far from where it puts them, in scoring a hypothesis (term `misfit`) | `World.centroidNoise` / √3: three centres each out by the fit | the gate `fit` is the same figure, cut at 2 px |
| `Tracker.turnSpread`, `moveSpread` | 2.5°, 4.2 mm a frame apart | how likely a pose continues the last (terms `turning`, `moving`) | `World.headTurnSpeed`, `headMoveSpeed` × the time between, a frame at least | the gates `turn`, `move` are the same figures, cut at the allowance (1) |
| `Match.missingCost` | 36 px² | what a reflector unseen costs a match (term `unseen`) | (6 × `World.centroidNoise`)² | (9) |
| `Match.squaredPixelsPerNat` | 2 px² | turns a match's cost into nats | 2 × `World.centroidNoise`² | |
| `Match.sizeScale` | 10 px per factor of e | what a blob's size being off costs a match (term `sizes`) | tuned | (9); a size is not a distance |
| `Match.shiftedCost` shift | 0.1 | how much the shift all reflectors share costs, beside each one's own | tuned | |
| `Pose.holdTurn`, `holdMove` | 3 px/radian, 0.05 px/mm | how strongly a followed pose is held where it was | tuned | |
| `Tracker.observe` shared | 0.5 | how much a reflector sharing a blob counts in the fit | tuned | |
| `Tracker.observe` dim | pixels over usual, at most 1 | how much a dim reflector counts | tuned | another size test (7) |
| `Tracker.reflectorReach` | 3 px + a blob's diameter | a blob this near a reflector is drawn as one (`roles`); a reflector with a blob this near where it is predicted isn't counted out of sight (`stillThere`) | tuned; about what a reflector moves in a frame at a head's fastest | (9) |
| `Tracker.reflectorDiscs` margin | 15 px + the biggest reflector's diameter | lights aren't learnt this near the clip | tuned | (9) |
| `Tracker.step` alone | a reflector's diameter | a reflector with another blob this near isn't seen clearly | tuned | |
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
| `Tracker.minBlobPixels`, `maxBlobPixels` | 15, 5000 px | a blob outside isn't a reflector, finding afresh | 250 px at 950 mm is 3600 at `nearest` | (7) |
| `Tracker.minFaintPixels` | 6 px | the smallest blob kept at all | tuned | (7) |
| `Tracker.besideBiggest` | 3/25 | a blob smaller beside the biggest in view isn't big (count term `big blobs`) | tuned | (7): 0.12 against `smallestShare`'s 0.125 |
| `Tracker.smallestShare` | 1/8 | three blobs any of which is smaller beside its reflector's usual size aren't the clip (term `size`) | tuned | (7) |
| `Match.wholeFactor` | 2 | a reflector isn't unseen, or sharing, while a blob half its usual size is left over | tuned | (7); `Lights.fragmentFactor` |
| `Lights.fragmentFactor` | 2 | a blob near a light and half its size or less is a fragment of it | tuned | `Match.wholeFactor`: the same half, of another thing |
| `Match.bigEnoughForTwo` | area² > bigger × both | a shared blob no bigger is perhaps one reflector (term `both in one`) | the geometric mean, on the log scale sizes are compared on | (7) |
| `World.sameSizeFactor` | 1.5 | `Lights.reflectorLikeFactor`: a still blob up to this much bigger than reflectors looks like one; `Lights.sameSizeFactor`: a reflector within this of a light is taken for it | tuned; "a fire's core is about twice a reflector" | (7) |
| `Match.reach` | 1.5 × the two radii; 10 px a radius unknown | two reflectors predicted this close may share a blob | touching, and half again for movement | |
| `Match.together` | the diameter of a disc of both areas | two blobs this close may be pieces of one reflector | geometry | |
| `Match.maxBlobs` | 8 | blobs considered | tuned | |
| `Match.matches` options | 4 a reflector; 2 blobs for a shared pair; 1 for the third | candidates tried | tuned | |
| `Tracker.clipSearch`, `maskedSearch` | 5, 4 | blobs searched for the clip among more than three, and among those masked | tuned | with a pose to go on, the best scored three of the 5 is taken; without, the only one that passes the gates |
| `Tracker.jumpRetries` | 5 | matches tried after the best jumps | tuned | |
| three big blobs | 3 | finding afresh wants just three (term `big blobs`) | the clip | |
| reflector size at 1 m | learnt (`trSizes`: latest, and usual with `sizeMemory`) | every size test above | about 245 px lower, 180 top, in the simulation (`Synth.lowerRadius`, `topRadius`) | (8) |

## Memory

How long things are kept. Six ways of forgetting are in use:

| Way | Where |
| --- | --- |
| A deadline: kept whole, then gone | `predictionSeconds`, `showingSeconds`, `confirmSeconds`, `Lights.learnSeconds`, `gapSeconds`, `forgetSeconds`, `clipMemorySeconds`, `Calibration.turnSeconds`, `longestStep` |
| Fading with time, by e each so many seconds | `Tracker.sizeMemory`, `Lights.sizeMemorySeconds` |
| A cap on how much is kept, the old scaled down as new comes | `Calibration.upMemory` (degrees), `Shape.memory` (windows), `Calibration.countMemory` (windows) |
| A cap on how sure, direction by direction | `Calibration.pivotMemorySpread` |
| The last few | `Branch.placesRemembered` |
| A floor under probabilities | `Branch.leastChance` |

| Name | Value | Keeps | From | Repeats or contradicts |
| --- | --- | --- | --- | --- |
| `Tracker.predictionSeconds` | 0.5 s | a pose predicts the reflectors, and one left for a pose found afresh may be resumed; after, the clip is found afresh only | tuned | (10); no longer how long a rejected follow waits |
| `Tracker.confirmSeconds` | 0.5 s | tracking this long before lights are learnt clear of the clip | tuned | (10) |
| `Tracker.showingSeconds` | 1 s one reflector out; 2.5 s one perhaps behind another | how long the clip is followed showing less (term `partly seen`) | tuned | |
| `Tracker.sizeMemory` | 10 s | a reflector's usual size, out of sight | tuned | (8) |
| `Lights.learnSeconds` | 3 s | still this long, a blob is a light (term `still`) | tuned | |
| `Lights.gapSeconds` | 0.25 s | how long a light may vanish while watched; how old a frame's sights may be | tuned | (10) |
| `Lights.forgetSeconds` | 45 s | a light unseen | tuned | |
| `Lights.clipMemorySeconds` | 10 s | where the reflectors were is kept clear of learning | tuned | equals `sizeMemory` by chance |
| `Lights.sizeMemorySeconds` | 600 s | the biggest reflector seen | tuned | (8) |
| `Calibration.upMemory` | 1800° | turning that says which way is up | "a couple of minutes of looking about" | |
| `Calibration.pivotMemorySpread` | 3 mm | evidence for the pivot, to this sureness a direction | tuned | |
| `Calibration.countMemory` | 1000 windows | the count of windows told | tuned | counts what `pivotMemorySpread` forgets otherwise |
| `Calibration.windowInfo` older builds | a tenth of what is remembered | how much a calibration saved by an older build counts | tuned | |
| `Calibration.upRate`, `pivotRate` | 0.25 °/s, 2 mm/s | how fast what is applied follows what is learnt | slow enough not to show | |
| `Calibration.longestStep` | 0.1 s | the longest gap what is applied moves over | tuned | |
| `Shape.memory` | 600 windows | evidence for the shape | "about ten minutes of turning" | |
| `Branch.placesRemembered`, `placeDegrees` | 4, 15° | where windows lately ended, so that fidgeting in one place counts once | tuned | |
| `Branch.leastChance` | 0.001 | no belief falls below, so any can come back | tuned | |
| `Response.jumpPersistence` | 3 frames | a jump lasting this long is real | tuned | (11) |
| `Tracker.absenceSeconds`, `absenceQuiet` | 1 s, 10 s | how long out of sight before the user is told why, and how seldom | tuned | |

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

| `Tracker.afreshCost` | 22.4 nats a frame after the last pose; 7.5 at 0.1 s; 1.8 at 0.5 s | what a pose found by layout gives up to a followed one (term `afresh`) | derived: log of the room a head has anywhere over the room a followed head has, see below | |
| `Tracker.weakScore` | −4 nats | a followed pose scoring less is doubted, and the clip looked for by layout too | derived: a head turning and moving at twice the spreads of its speed, "little beyond two" | |
| `Match.unseenScore` × reflectors with no blob near | −18 nats each | what the clip still being where it was scores (`stillThere`), which a pose found elsewhere must beat | `Match.missingCost` in nats | (15) |

The hard gates among the head facts, noise and appearance above are
thresholds too: each row that names a term is a decision.

## Following or finding afresh

With a pose from a moment ago there are up to four hypotheses about a
frame, and `Tracker.followOrFind` takes the best scored (the reasoning is
in Note [Following or finding afresh] in `Tracker.hs`):

| Hypothesis | Scored by | Vetoed by |
| --- | --- | --- |
| Followed: the best match to the prediction, as before | `usual size`, 18 nats a reflector unseen, `misfit`, `turning`, `moving` | `followable`: `fit`, `turn`, `move`, `canBeThere`; `partly seen` |
| Refound: the big blobs by layout, at a pose within the jump following allows (`jumpable`) | `usual size`, `misfit`, and the better of `turning` + `moving` and `afresh` + `reach` | `clipLike`: `canBeThere`, `size` |
| Found afresh: the same, at a pose beyond that jump | `usual size`, `misfit`, `afresh`, `reach` | `clipLike`; and it must beat the clip being still where it was |
| Still where it was: the frame doesn't show the clip rightly | 18 nats for each reflector with no blob within `reflectorReach` of its predicted place | `showingSeconds`: not once the clip has shown less for a second |

When they are tried:

- Following alone, while it scores at least `weakScore`. On ordinary
  glancing it always does, so nothing else is computed.
- By layout too, when following is rejected or weak: exactly three big
  blobs in view, or the best scored three of the heaviest five; never
  masked blobs; not the very three following took; and, against a weak
  follow, not three whose sizes alone score no better than it.
- A tie is following's. A frame nothing wins is lost, with following's
  reason, and any pose passed over for the clip being where it was
  (`outHeld`).

What continues:

- **Followed** and **refound** carry on the belief about the mirror image
  and its window of turning, the time tracked that lets lights be learnt
  clear of the clip, and what learns from poses.
- **Found afresh** starts them over, as it always has (`Branch.foundAfresh`:
  either pose as likely the mirror image). The pose it left is kept for
  `predictionSeconds`; a pose found by layout within the jump following
  allows from *that* pose is **resumed**: scored as continuing it, and
  taking up its belief and time tracked as if the frames between had been
  lost.

Where the advantage of following comes from. `afreshCost` is not tuned: it
is how much higher the density of "about where it was" is at its peak than
that of "anywhere a head can be". In turning, three ways, a spread of
`headTurnSpread` (40°) against `headTurnSpeed` × the time since (2.5° a
frame on): 3 ln 16 = 8.3 nats. In place, the box `canBeThere` allows (800 mm
by 800 mm by 2250 mm) against a gaussian of `headMoveSpeed` × the time
(4.2 mm): ln (1.44e9 / 1139) = 14.1 nats. Together 22.4 nats a frame on,
falling by 6 ln n after n frames, each part never below nothing. Following and
finding afresh are otherwise taken as equally likely beforehand.

What that says about the gates (contradiction 1): a pose a frame on is as
likely found anywhere as followed when it has turned 16.7° or moved 28 mm,
beside gates at 15.8° and 41.7 mm. The turn gate is where the speed puts
it; the move gate is half again looser.

What the wait was, and is now. Following alone for half a second protected
a brief occlusion from being taken for the clip somewhere else. That is
now the fourth hypothesis, with a score: two reflectors still in place and
a stray score 18 nats as the clip where it was, and more than 22 as the
clip elsewhere, so the frame is lost as before; three blobs nowhere near
score 54 against 22, and are found at once. A head that has really jumped,
with a reflector or two landing near the old places, is found after two or
three frames, as `afreshCost` falls below 18.

What it doesn't do. A stray the size of a reflector, taken for it, scores
as well as the reflector: size is the only evidence of what a blob is,
`usual size` follows what is tracked within seconds, and frames are scored
one by one. Such a pose is left only when a gate rejects it. Strays under
half a reflector's size aren't taken (`Match.wholeFactor`), and between
half and full size they cost under 24 nats, which finding afresh elsewhere
rarely beats; so replacing a follow happens mostly as a refind, close by.
The likelihood of a blob's size (7, 14) and evidence that adds up over
frames are what would widen it.

## What is a term, and what isn't yet

| Decision | Terms | State |
| --- | --- | --- |
| A pose is somewhere a head can be (`Tracker.canBeThere`) | `near`, `far`, `turned`, `aside` | terms; the decision is their sum |
| A followed pose is the clip (`Tracker.followable`) | `fit`, `turn`, `move`, and the above | terms |
| Three blobs found afresh are the clip (`Tracker.clipLike`) | the above, and `size` | terms |
| Following or finding by layout (`Tracker.followOrFind`) | `usual size`, `misfit`, `turning`, `moving`, `afresh`, `reach`; `unseen` | soft terms; the better score wins, after the gates above |
| A pose found by layout continues the last (`Tracker.jumpable`) | `turn`, `move` | the gates, used as the test |
| The clip is still where it was (`Tracker.stillThere`) | `unseen`, counted | a score; its deadline is a rule |
| Which blobs to try afresh (`Tracker.findAfresh`) | `big blobs` | the rule decides as before; the term says why it came to nothing |
| How long to follow the clip showing less (`Tracker.follow`) | `partly seen` | a term, filtering matches as before |
| A match's cost (`Match.matches`) | `places`, `sizes`, `unseen`; `shared size`, `third size` | terms, in nats; matches are ordered by them |
| Which matches exist (`Match.matches`) | none | rules: two reflectors seen, no blob twice, `missingPlausible`, `reach` |
| A shared blob is both reflectors (`Match.bigEnoughForTwo`) | `both in one` | a term |
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

The scheme has four steps. The first two are done: the inventory and
`World`, and terms with explanations wrapped round the decisions as they
were. The next two change behaviour, and each has what it needs in place.

**Step 3: choose among hypotheses by score.** Each candidate used to be
accepted or rejected by itself, in a fixed order, and the first accepted
won. With every candidate's terms in nats, the tracker can score all the
candidates of a frame (each match followed, the three big blobs afresh,
the mirror image) and take the best, falling back to the next when the
best is impossible. The first of these is done: following and finding
afresh are tried in the same frame and compared
([above](#following-or-finding-afresh)), in place of following for
`predictionSeconds` and only then looking afresh. The rest needs:

- the gates softened where a hard edge has bitten: `aside` and `turned`
  as likelihoods of where a head is (`headTurnSpread` already is one, in
  `Branch`), `fit` as a likelihood under `centroidNoise`, `turn` and
  `move` as likelihoods of a head's speed, with contradiction 1 settled by
  splitting a head's real speed from the allowance for a wrong match;
- one term for a blob's size, a likelihood under the size learnt for its
  reflector, in place of the six tests of contradiction 7 and the two
  sizes of 14: this is what would let a pose followed onto a stray be
  told from the clip;
- each match followed scored as a hypothesis, not only the best, and
  evidence carried from frame to frame, so that a pose that has fitted
  badly for a while is doubted more than one that just began to (15);
- `Match`'s rules for which matches exist turned into terms, so that a
  match excluded by `missingPlausible` can be told;
- the sway of contradiction 3 settled, so that `Branch` and `Calibration`
  score a window by one spread.

Scores then also say how sure tracking is, frame by frame, which the
learners can take in place of their `Clear` and `settled` gates.

**Step 4: one way of remembering.** The six ways of forgetting become one:
evidence with a weight that fades at a stated rate, in seconds. `Size`,
`Lights`' biggest reflector, the level's scatter, the pivot's information
and the shape's evidence are all sums that can fade alike; deadlines become
the time for a weight to fall below use. That needs a small type for a
fading sum with its half-life in `World`'s `Seconds`, and contradiction 8
(10 s, 600 s, never) and 11 (frames for time) settled as it is adopted.
The memory figures of the tests will move then; nothing else should, if
each half-life is first set to what the old scheme came to.
