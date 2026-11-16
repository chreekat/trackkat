# How trackkat behaves

The details behind the [README](../README.md).

## Installing and updating

trackkat runs from wherever it was unzipped until you click **Install
trackkat** at the top of its window (or run `trackkat.exe --install`). That copies it to
`%LOCALAPPDATA%\Programs\trackkat\<version>`, for you alone and without
admin, adds it to the Start menu and to Settings → Apps, points games at its
NPClient there, and switches to the installed copy. If a copy as new is
installed already, the window offers to switch to it instead.

Settings, calibration, and `npclient.log` live in `%APPDATA%\trackkat`, for
any copy, installed or not, so installing and updating leave them alone.
Settings files beside `trackkat.exe`, where older versions kept them, are
moved there once.

Each build has a version, such as `0.1.142 (3f9c2e1)`: the build's number and
its commit. The window's title, the log window, and the start of the log
show it. An installed trackkat looks for a newer build when it starts and
every four hours, downloads it in the background, checks its SHA-256, and
unpacks it beside the others. The strip across the top of the window then
turns amber and offers **Restart to update**, as does the tray menu;
otherwise the new version starts the next time trackkat does. If it can't
reach GitHub, it tries again later; the log says so once, until a look
works again, and a terminal it was started from every time. The strip says
what the last look found. **Keep up to date** (`updates = off`) stops this. **Check for updates**, in
the strip, looks at once, even with it off, and the log says what it found.

A game keeps the NPClient it loaded until it quits, and that DLL starts
`trackkat.exe` from its own folder, so an older version's folder is kept
while any game has its DLL, and deleted once none does. Its `trackkat.exe`,
started by such a game, hands over to the newest installed.

**Start with Windows** (`startup = on`) starts trackkat in the tray when you
log in. Games start trackkat themselves anyway, except in
[anti-cheat mode](#anti-cheat).

To uninstall, use Settings → Apps → trackkat. It quits trackkat, removes its
shortcut, apps entry, start at login, NPClient pointer, and folders; files a running game holds go when you next log in. It asks
whether to keep your settings. `trackkat.exe --uninstall --quiet` keeps them
without asking.

## Tray icon

trackkat shows a cat in the system tray, its eyes green while tracking,
amber while the clip is out of sight, gray while looking for the camera,
red without one. Its tooltip says whether a game is connected or trackkat
is paused. Click it to open trackkat's window; right-click it to recenter, pause,
trim, or quit. Quitting from the tray menu turns the camera's lights off and
lets it go.

## Started by hand, or by a game

trackkat never opens a console window, however it is started.

**Started by hand**, trackkat opens its window. Running it again shows the
window of the one already running. For the first minute, the log names the
joystick buttons you press, ready to bind in `trackkat.conf`.

**Started from a terminal** (PowerShell or Command Prompt), trackkat is a
window program, so the prompt comes back at once, but trackkat prints what
its log says to that terminal as it goes. Ctrl-C there doesn't stop it:
quit it from its window or the tray. Closing the terminal quits it too.
Piping its output, as in `.\trackkat.exe | Out-Host`, makes the terminal
wait for it, and adds a line a second with the pose and figures.

**Started with Windows** (`--tray`), trackkat shows only its tray icon,
until you quit it.

**Started with `--record`**, trackkat also writes every frame the camera
sends to a file, to replay the tracking exactly later, and takes over from
a trackkat already running. A key or button bound to `mark` in
`trackkat.conf` notes a moment in the recording, and the log says `Mark 1
recorded.`; without `--record` it says there is no recording to mark. See
[recording a problem](tirprobe.md#recording-a-problem).

**Started by a game**, trackkat shows only its tray icon.
Once no game has been tracking for a minute, and neither its window nor its
log window is open, it quits by itself, so a game restarted within the
minute finds it still running. It says when it starts counting down, and when it quits:

```
No game or window is using trackkat; quitting in 60 s unless one starts.
Quitting: no game or window used trackkat for 60 s.
```

If the camera is unplugged, or the PC sleeps, trackkat waits and picks it up
again when it comes back. Joysticks plugged in while it runs, or that come
back after sleep, work right away.

## The window

The tooltips in trackkat's window are kept to a phrase each; this is the
rest of what there is to say.

**Head and games see.** The head column is your head as the camera measures
it, from the center you set by recentering: degrees turned, mm moved. The
games-see column is what games get, after smoothing, the curves, and any
trim; while paused, or with the clip out of sight, it holds still. In each
bar, blue is where games see that axis, out to the end of its range (behind
you, straight up, 300 mm), and the yellow tick is your head.

**Lights.** Camera is green when trackkat has the camera, gray while it
looks for it, and red, with the reason, when it can't use it. Clip says
whether the camera sees all three reflectors. Games names a game reading
your head, by its program's name when trackkat can read it; the camera's
second green light shows the same.

**Figures.** Under the switches, over the last second: how long trackkat
took from a camera frame to games having the pose (average and worst), the
longest gap between frames, the most stripes in a frame, and garbage
collections with the longest pause; corrupt packets too, if there were any.

**Bindings.** Each list has one key or button to a line, however long; a
line too long for its column scrolls sideways as you edit it. Click a
list's text to edit it by hand, and click anywhere else to finish; tag a
line with `hold` or `double` at its end for
[what the button does held or pressed twice](#one-button-several-things).
**Add…**
waits for the next key, joystick button, or middle or side mouse button
pressed over the window, and adds it; click it again to stop waiting.
Delete a line to remove one.

**Saving.** Switches and sliders are written to `trackkat.conf` right
away, text when you leave its box or press Enter. Only what you changed is
rewritten, so your own edits and comments in the file stay. The line beside
**Response** says whether the last save worked, and if not, why.

**Curves.** In a plot, across is your head and up is the game's view; the
dot is your head now. The multiplier is how many times further the view
turns or moves than your head, and its arrows step it by halves. More
**Curve** gives finer control near the center of the range, for aiming and
reading gauges, and makes large movements steeper, so the same head
movement still reaches the end of the view; the dotted line is then the
multiplier alone. The line under each says how far your head moves for the
view to reach the end of its range. **Curves as text** shows them as
`trackkat.conf` writes them: a multiplier such as `1:6`, perhaps with a
curve as in `1:6, curve 40` (older files say `precision` there, which
still reads), or input:output points such as `10:10 45:120`, joined by
straight lines from 0:0. Points are left as they are unless you change the
multiplier or the curve.

**Aids.** Smoothing calms the view while your head is still, at the cost of
a little lag when you move; off, games get every tremor. Raise steadiness
if the view jitters while you are still, and lower it if it feels floaty or
drifts into place. Raise responsiveness if the view lags behind your head,
and lower it if it jitters as you move slowly. With **Lean follows the
view** on (`lean = view`), looking behind you and leaning left leans out of
the cockpit's right side; off, leaning moves you the same way whichever way
you look.

**The cover** over **Calibrate now** and **Clear calibration** keeps them
from being clicked by mistake. Click it, or press Space, to lift it; it
closes when you click close, after you use one of them, or once the mouse
has been away for a few seconds. Clear the calibration after moving the
camera or bending the clip, or if turning your head turns the view the
wrong way.

**Check for updates** rests for a minute after each look, and works only
for an installed trackkat. **Install trackkat** and **Restart to update**
restart trackkat in a moment; games keep their head tracking.

## Recentering

Recenter (F12, or a game's own "recenter TrackIR" binding) makes the way you
face straight ahead. Recentering while the clip is out of sight centers the
view at once; the pose you hold when the clip comes back becomes the new
center.

## Trim

Trim makes the view in the game now the new forward: where it looks, how
it's tilted, and where it has moved to. In DCS, to work the left MFD and
the keyboard unit while entering waypoints, look at them and press trim:
the view now rests there, and you can still glance around from it. Crane
toward the UH-1's center console and trim, and the view stays craned
while you sit back. Presses add up, so you can trim again from a trimmed
view, and recentering goes back to straight ahead. Leaning follows the
trimmed view, and the trimmed movement stays where it is whichever way you
then look. trackkat logs the new forward, in degrees and mm as games see
them (pitch positive down), and when recentering clears it:

```
Trimmed: forward is now yaw -42 pitch +18 roll +0, x +0 y -35 z -60 mm.
Trim cleared.
```

The view jumps when you trim: where you look becomes forward, and your
head, still turned toward it, turns the view on from there; look back
toward the screen, and sit back, to bring the trimmed view ahead. While
paused, trimming takes the held view as forward, and games stay held until
you resume.

In the window, the TRIM switch lights blue while a trim is in place, with
its yaw and pitch, and "moved" if it also moves or tilts the view, and
PAUSE lights amber while paused, with the yaw and pitch of the view it
holds. A switch that is off shows its name alone. Each switch's keys
and buttons are listed under it, one to a line, to edit or add to.

## Aim

The curves turn the view much further than your head, so that you can look
behind you without looking away from the screen: too far to lay a gunsight
on something by head, as the Apache's gunner does. Aim is for that: it
reduces the sensitivity, for finer control. From the
moment you begin aiming, the view turns from where it then is by how far
your head turns from where it then is, half as far again by default, and
smoothed more steadily. Look roughly at the target, aim, and finish with
small, natural movements of your head.

The view doesn't move as aiming begins. When it ends, the view glides back
over a fifth of a second to where your head would otherwise have it.
Leaning isn't changed by aiming, and trim applies as ever. Recentering ends
aiming; pausing holds the view through it.

Bind a key or button to `aim` with `hold` after it to aim just while it is
held down, or without to turn aiming on, then off. The AIM switch in the
window does the same, and lights blue while aiming; the log says when it
begins and ends. Aiming's curves and smoothing are the `aim yaw`, `aim
pitch`, `aim roll`, `aim smoothing`, `aim steadiness`, and `aim
responsiveness` settings in `trackkat.conf`: a curve takes how far your head
has turned since aiming began to how far the view turns.

## One button, several things

A key or button can be pressed three ways, and bound to something for each,
to save buttons on a HOTAS: on its own, a press; with `hold` after it, held
down for 0.3 seconds; with `double`, pressed twice within 0.3 seconds.

```
recenter = F12, Throttle - HOTAS Warthog button 4 double
trim = Throttle - HOTAS Warthog button 4
aim = Throttle - HOTAS Warthog button 4 hold
```

In the window, type `hold` or `double` after a key or button in its list.
`mark` is bound the same ways, in `trackkat.conf` alone: the window has no
list for it, and keeps what the file says when it saves.

A key bound only as a press acts the moment it goes down, as it always has.
One that also has a hold or a double waits until the press can be nothing
else: until it comes up, if it has a hold, and until 0.3 seconds have gone
by with no second press, if it has a double. Recenter, pause, and trim on a
hold act once, when the 0.3 seconds are up; aim on a hold lasts until the
button comes up. A key bound with modifiers, such as `Ctrl+F9 hold`, goes
down with just those held, and comes up when the key itself does.

## Calibration

trackkat learns two things from how you move your head, and keeps them in
`trackkat.calibration`, in `%APPDATA%\trackkat`, so it carries on from where
it left off:

- **Which way is up.** The camera is usually tipped down at you, so its own
  axes would mix some of a left-right turn into roll. trackkat takes up to
  be the axis your head turns about when you glance left and right, told
  apart from the axes it nods and tilts about; slow drifts don't count.
- **Where your neck pivots.** The clip sits in front of, above, and to one
  side of the point your head turns about, so turning your head also moves
  the clip. trackkat finds the point that stays still while you turn (and
  your body doesn't) and measures movement there, so that turning your head
  doesn't also move the view, and only leaning does.

Both keep being refined while you play, with older movement counting less,
so if you move the camera they follow within a few minutes of looking
around, for any camera tipped or rolled less than about 45 degrees. Changes
apply slowly enough not to move the view, and trackkat logs a line when one
is worth mentioning:

```
Level learned: camera tipped 14.2 degrees down and 1.3 to its right, from 900 degrees of head turning.
Neck pivot learned: 95 mm below the clip, 80 behind it, and 60 to its right, from 40 head turns.
```

To calibrate level at once, lift the cover beside the calibration in
trackkat's window and click **Calibrate now**, then turn your head left and
right for five seconds. **Clear calibration**, under the same cover, forgets
what was learned. Delete `trackkat.calibration` to start afresh; it explains
its own format.

Click the head view to switch it between three views; trackkat keeps the
one you left it on as `head view` in `trackkat.conf`. **Virtual** shows
your head as games see it. **Physical** shows, dotted yellow, your own head
as the camera measures it, turning about your neck pivot (the gray dot);
once both are learned, with the clip where the learned pivot puts it, a
reflector out of sight drawn hollow and named below.
**Camera** shows the camera's whole picture, each blob at its size and
shape.

Single-frame jumps no head could make are ignored.

### Mirror images

Three reflectors fit two poses equally well: the clip as it is, and its
mirror image, tilted the other way toward the camera. On the mirror image,
yaw and pitch go the wrong way while leaning is right. The two meet where
the clip's triangle squarely faces the camera, as a clip whose post is bent
upright often does, and trackkat can come out of that, or find the clip
again after losing it, on the mirror image. It tells them apart by how your
head turns: about your neck, behind the clip, never about a point in front
of it. It goes only by turning seen clearly, each reflector a blob of its
own rather than run together or in pieces, as they are when seen at a
slant; it needs turning through different poses, fidgeting while looking
one way counting for no more than one glance; and it is slower to believe
a pose that would have your head turned further from your center than
heads usually turn. Within a second or two
of glancing about it corrects a mirror image, keeping your center where it
was, and logs:

```
Corrected a mirror-image pose.
```

trackkat learns nothing while it isn't sure which pose is your head's, or
while it doesn't see the reflectors clearly. A
calibration learned from mirror images, whose neck pivot is in front of the
clip, is forgotten when trackkat starts, with a line in the log saying so.

### The clip's shape

No single frame shows the shape of the clip's triangle: any triangle fits
any three points in the image. How your head moves does: turning about your
neck looks like turning about one still point only with the right shape.
So trackkat also estimates the shape, from the same clearly seen turning it
learns from, and reports it; it doesn't apply it. Movement shows the shape
but not its size, so the estimate keeps your left to right distance and
gives the two distances to the top to match. After a minute or two of
glancing about, once it is sure, it logs a line, and shows it under the
calibration:

```
Clip shape estimate: 70 99 108 mm, from 2 minutes of turning, keeping 70 mm left to right (movement shows the shape, not the size); configured 70 110 110. Try clip = 70 99 108 if tracking seems off.
```

or, when it is within 2 mm of `clip` in `trackkat.conf`, says so. In
simulation it comes within about a mm of the truth after a minute of
glancing 30 degrees each way, half that after five, falling short of a
far different shape by about a tenth of the difference; glancing only half
as far takes about twice as long and comes out up to a mm small. It starts
afresh each session and when `clip` changes. `tirprobe --replay` prints it
at the end, so a recording can tell you your clip's shape.

## Still lights

Something bright behind you that stays put, such as a lamp or a lit fire,
shows to the camera as a blob much like a reflector. While trackkat follows
the clip it ignores such blobs anyway, but to find the clip afresh, when it
starts or after losing it, it needs exactly three big blobs, and a fire
makes a fourth. So trackkat learns lights that stay within a few pixels for
three seconds, with any flickering bits around them, and from then on
ignores blobs there, except where it expects the clip (your head can pass
in front of the light). It learns them while it has lost the clip too, but
never from where the clip is or just was. A light no bigger than a
reflector could be one of the clip's, held still while trackkat can't see
the others, as when you look down at a phone: unless trackkat is following
the clip elsewhere, such a light must stay put for five minutes to be
learnt. The log says so:

```
Ignoring a bright light at (156, 297): it stayed put for 3 seconds, and at 813 pixels is bigger than the clip's reflectors (349).
No longer ignoring the light at (156, 297): it has been gone a while.
No longer ignoring the light at (300, 95): it moved off with the clip.
```

The first line says why the light was taken for one: here for being
bigger than a reflector; otherwise for staying put before the clip was
seen, while the clip was tracked elsewhere, or for longer than a head is
held still. The second line comes once the light has been out of sight for
45 seconds, the third when a light turns out to have been one of the clip's
reflectors, moving off with it, or going as the clip came into view. The status line notes blobs ignored, such as `+2 masked`, the
window's head view shows them gray among what the camera sees, its
camera view with the area ignored around each light dashed, and `tirprobe --blobs` marks them `masked`.

So with a fire lit, trackkat finds the clip about three seconds after it
starts, and at once after that. If you hold perfectly still while it
learns the fire, it learns your clip with it, sees that it was the clip,
and finds it anyway. A clip that sits still for a few seconds while
trackkat can't find it before it has seen it at all can be learnt as a
light: it is found anyway, and the light is forgotten as you move. If ignoring lights causes
trouble, clear **Ignore still lights** in the window, or set
`ignore lights = off`, which tirprobe follows too; `tirprobe --no-mask` turns
it off for tirprobe alone.

## Anti-cheat

Some anti-cheat, such as BattlEye in Arma Reforger, refuses to let a game
load trackkat's NPClient, which isn't signed, but allows NaturalPoint's own,
which is. With `npclient = trackir`, trackkat tells games to load
NaturalPoint's NPClient from its TrackIR software's folder, and stands in
for that software, which the DLL gets the pose from. So:

- NaturalPoint's TrackIR software must be installed, but not running: quit
  it, including from the tray. The camera stays on WinUSB.
- Games can't start trackkat this way, so start trackkat before the game,
  or check **Start with Windows**.
- When a game starts head tracking, trackkat's log says so, with the game's
  number:

  ```
  Game 3450 is using NaturalPoint's NPClient.
  ```

- NaturalPoint's DLL tells trackkat about a game only then, so trackkat
  notes the game in `trackkat.game` in `%APPDATA%\trackkat`. Restarted
  while the game runs, to update or after quitting, trackkat carries on
  serving it, and says so:

  ```
  Serving DCS (game 1006) again, which was running before trackkat restarted.
  ```

Tested: Arma Reforger (game 8310) tracks this way.

If the TrackIR software isn't found, trackkat says so and games load its own
NPClient. Back on `npclient = trackkat`, games load trackkat's again; either
way the change applies when the settings are saved.

## Testing NPClient

- `npclient.log`, in `%APPDATA%\trackkat`, records what each game asked
  NPClient for and what it got.
- `npgame` loads NPClient like a game does (as DCS, game 1003, by default,
  or `npgame <game id>`), unscrambles and checksums what it gets as a game
  must, and prints the pose. Run it while trackkat is running.
- `tools/test-npclient.sh` runs the same check under Wine, with a stand-in
  for trackkat. Given NaturalPoint's NPClient64.dll as `NP_GENUINE_DLL`, it
  checks trackkat's stand-in for NaturalPoint's software through that DLL
  too.
