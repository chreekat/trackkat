# How trackkat behaves

The details behind the [README](../README.md).

## Installing and updating

trackkat runs from wherever it was unzipped until you click **Install
trackkat** in its window (or run `trackkat.exe --install`). That copies it to
`%LOCALAPPDATA%\Programs\trackkat\<version>`, for you alone and without
admin, adds it to the Start menu and to Settings → Apps, points games at its
NPClient there, and switches to the installed copy. If a copy as new is
installed already, the window offers to switch to it instead.

Settings, calibration and `npclient.log` live in `%APPDATA%\trackkat`, for
any copy, installed or not, so installing and updating leave them alone.
Settings files beside `trackkat.exe`, where older versions kept them, are
moved there once.

Each build has a version, such as `0.1.142 (3f9c2e1)`: the build's number and
its commit. The window's title and the start of the log show it. An
installed trackkat looks for a newer build when it starts and every four
hours, downloads it in the background, checks its SHA-256 and unpacks it
beside the others. The window then offers **Restart to update**, as does the
tray menu; otherwise the new version starts the next time trackkat does. If
it can't reach GitHub, it says so on the console and tries again later.
**Keep up to date** (`updates = off`) stops this.

A game keeps the NPClient it loaded until it quits, and that DLL starts
`trackkat.exe` from its own folder, so an older version's folder is kept
while any game has its DLL, and deleted once none does. Its `trackkat.exe`,
started by such a game, hands over to the newest installed.

**Start with Windows** (`startup = on`) starts trackkat in the tray when you
log in, through `trackkat-tray.exe`, which starts it without a console
window. Games start trackkat themselves anyway, except in
[anti-cheat mode](#anti-cheat).

To uninstall, use Settings → Apps → trackkat. It quits trackkat, removes its
shortcut, apps entry, start at login and its NPClient pointer, and its
folders; files a running game holds go when you next log in. It asks
whether to keep your settings. `trackkat.exe --uninstall --quiet` keeps them
without asking.

## Tray icon

trackkat shows a cat in the system tray, its eyes green while tracking,
amber while the clip is out of sight, grey while looking for the camera,
red without one. Its tooltip says whether a game is connected or trackkat
is paused. Click it to open trackkat's window; right-click it to recenter, pause,
trim or quit. Quitting from the tray menu turns the camera's lights off and
lets it go.

## Started by hand, or by a game

**Started by hand**, trackkat opens its window and keeps its console, where
Enter recenters, `c` Enter calibrates, `j` Enter names the joystick buttons
you press for the next minute, and Ctrl-C quits. Running it again shows the
window of the one already running.

**Started with Windows** (`--tray`), trackkat shows only its tray icon, with
no console, until you quit it.

**Started by a game**, trackkat shows only its tray icon, with no console.
Once no game has been tracking for a minute, and its window isn't open, it
quits by itself, so a game restarted within the minute finds it still
running. It says when it starts counting down, and when it quits:

```
No game or window is using trackkat; quitting in 60 s unless one starts.
Quitting: no game or window used trackkat for 60 s.
```

If the camera is unplugged, or the PC sleeps, trackkat waits and picks it up
again when it comes back. Joysticks plugged in while it runs, or that come
back after sleep, work straight away.

## Recentering

Recenter (F12, or a game's own "recenter TrackIR" binding) makes the way you
face straight ahead. Recentering while the clip is out of sight centres the
view at once; the pose you hold when the clip comes back becomes the new
centre.

## Trim

Trim makes where you look in the game the new forward. In DCS, to work the
left MFD and the keyboard unit while entering waypoints, look at them and
press trim: the view now rests there, and you can still glance around from
it. Presses add up, so you can trim again from a trimmed view, and
recentering goes back to straight ahead. Only turning left and right and up
and down are trimmed, not tilting or leaning, and leaning follows the
trimmed view. trackkat logs the new forward, in degrees as games see them
(pitch positive down), and when recentering clears it:

```
Trimmed: forward is now yaw -42 pitch +18.
Trim cleared.
```

The view jumps when you trim: where you look becomes forward, and your
head, still turned toward it, turns the view on from there; look back
toward the screen to bring the trimmed view ahead. While paused, trimming
takes the held view as forward, and games stay held until you resume.

## Calibration

trackkat learns two things from how you move your head, and keeps them in
`trackkat.calibration`, in `%APPDATA%\trackkat`, so it carries on from where
it left off:

- **Which way is up.** The camera is usually tipped down at you, so its own
  axes would mix some of a left-right turn into roll. trackkat takes up to
  be the axis your head turns about when you glance left and right. Nods,
  tilts and slow drifts don't count.
- **Where your neck pivots.** The clip sits in front of, above and to one
  side of the point your head turns about, so turning your head also moves
  the clip. trackkat finds the point that stays still while you turn (and
  your body doesn't) and measures movement there, so that turning your head
  doesn't also move the view, and only leaning does.

Both keep being refined while you play, with older movement counting less,
so if you move the camera they follow within a few minutes of looking
around (if you tip it by more than about 30 degrees, calibrate level). Changes
apply slowly enough not to move the view, and trackkat logs a line when one
is worth mentioning:

```
Level learnt: camera tipped 14.2 degrees down and 1.3 to its right, from 900 degrees of head turning.
Neck pivot learnt: 95 mm below the clip, 80 behind it and 60 to its right, from 40 head turns.
```

To calibrate level at once, click **Calibrate level** in trackkat's window
(or type `c` and Enter in its console), then turn your head left and right
for five seconds. Delete `trackkat.calibration` to start afresh; it explains
its own format.

Single-frame jumps no head could make are ignored.

### Mirror images

Three reflectors fit two poses equally well: the clip as it is, and its
mirror image, tilted the other way toward the camera. On the mirror image,
yaw and pitch go the wrong way while leaning is right. The two meet where
the clip's triangle squarely faces the camera, as a clip whose post is bent
upright often does, and trackkat can come out of that, or find the clip
again after losing it, on the mirror image. It tells them apart by how your
head turns: about your neck, behind the clip, never about a point in front
of it. Within a second or two of glancing about it corrects a mirror image,
keeping your centre where it was, and logs:

```
Corrected a mirror-image pose.
```

trackkat learns nothing while it isn't sure which pose is your head's. A
calibration learnt from mirror images, whose neck pivot is in front of the
clip, is forgotten when trackkat starts, with a line in the log saying so.

## Anti-cheat

Some anti-cheat, such as BattlEye in Arma Reforger, refuses to let a game
load trackkat's NPClient, which isn't signed, but allows NaturalPoint's own,
which is. With `npclient = trackir`, trackkat tells games to load
NaturalPoint's NPClient from its TrackIR software's folder, and stands in
for that software, which the DLL gets the pose from. So:

- NaturalPoint's TrackIR software must be installed, but not running: quit
  it, including from the tray. The camera stays on WinUSB.
- Games can't start trackkat this way, so start trackkat before the game,
  or tick **Start with Windows**.
- When a game starts head tracking, trackkat's log says so, with the game's
  number:

  ```
  Game 3450 is using NaturalPoint's NPClient.
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
