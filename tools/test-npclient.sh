#!/bin/sh
# Check NPClient end to end without a camera or a game: fakepose publishes
# a known pose and npgame reads it through NPClient64.dll as a game would,
# unscrambling and checksumming it. Needs mingw-w64 and wine, or, on
# Windows, MSYS2's UCRT64 gcc, as CI runs it.
#
# Our DLL is checked as a game in Black Shark (1003, scrambled). Then
# fakepose stands in for NaturalPoint's software, and npgame reads through
# fakenpclient, which talks to it as NaturalPoint's DLL does, as DCS (1006,
# scrambled), while fakepose is restarted: the second fakepose must serve
# the game as the first did, though the game doesn't register again. Then,
# if NP_GENUINE_DLL names NaturalPoint's own NPClient64.dll (never in this
# repository), npgame reads through that DLL, as a game with plain data
# (1001) and as ArmA 3 (7503, scrambled).
set -eu
case $(uname -s) in
*_NT*) native=yes ;;
*) native= ;;
esac
# NaturalPoint's DLL finds its software by a window, which Wine keeps only
# with a display.
if [ -z "$native" ] && [ -z "${DISPLAY:-}" ] && command -v xvfb-run >/dev/null; then
    exec xvfb-run -a sh "$0" "$@"
fi
src=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"

if [ -n "$native" ]; then
    cc=gcc
    run() { "$@"; }
    winpath() { cygpath -w "$1"; }
else
    cc=x86_64-w64-mingw32-gcc
    run() { wine "$@"; }
    winpath() { winepath -w "$1"; }
fi
flags="-std=c11 -Wall -Wextra -Werror -O2"
$cc $flags -s -shared -static-libgcc -Wl,--kill-at -o NPClient64.dll "$src/npclient/npclient.c" "$src/cbits/npclient_common.c"
$cc $flags -static -I"$src/cbits" -o fakepose.exe "$src/tools/fakepose.c" "$src/cbits/windows.c" "$src/cbits/joystick.c" \
    "$src/cbits/trackir.c" "$src/cbits/npclient_common.c" -lhid -ladvapi32 -luser32
$cc $flags -static -o npgame.exe "$src/tools/npgame.c" -ladvapi32 -luser32
$cc $flags -s -shared -static-libgcc -Wl,--kill-at -o fakenp64.dll "$src/tools/fakenpclient.c" -luser32

if [ -z "$native" ]; then
    export WINEDEBUG=-all WINEPREFIX="$work/prefix"
    wineboot -i >/dev/null 2>&1
fi

# fakepose publishes pitch 10, roll -5 and x/y/z 12/-34/56 throughout.
pose='checksum ok .*pitch  +10.00 roll   -5.00 deg  x  +12.0 y  -34.0 z  +56.0 mm'

run ./fakepose.exe 6 >fakepose.out 2>&1 &
sleep 2
run ./npgame.exe 1003 "$(winpath "$work/NPClient64.dll")" | tee npgame.out
wait
cat fakepose.out
grep -q "status 0 frame 1 $pose" npgame.out
grep -q '^0 bad checksums' npgame.out
grep -q 'asked to recenter 1 times' fakepose.out
grep -q 'at most 1 games tracking, 0 at the end' fakepose.out
echo "NPClient OK"

# The first fakepose quits 2 s into the game, which reads for 6 s; the
# second starts as the first quits, given the game the first served.
run ./fakepose.exe 4 >first.out 2>&1 &
first=$!
sleep 2
run ./npgame.exe 1006 "$(winpath "$work/fakenp64.dll")" 6 >npgame.out 2>&1 &
wait $first
cat first.out
game=$(sed -n 's/^served game: //p' first.out | tr -d '\r')
echo "$game" | grep -q '^[1-9][0-9]* [0-9]* [1-9][0-9]* 1006 1$'
run ./fakepose.exe 7 "$game" >second.out 2>&1
wait
cat npgame.out second.out
grep -q '^version: 0x0500' npgame.out
grep -q "status 0 frame [0-9]* $pose" npgame.out
grep -q '^0 bad checksums' npgame.out
grep -q '^the frame count went back 0 times' npgame.out
grep -q 'at most 1 games tracking' first.out
grep -q 'at most 1 games tracking, 0 at the end' second.out
grep -q '^served game: 0 ' second.out
echo "Restarting the stand-in for NaturalPoint's software OK"

genuine=${NP_GENUINE_DLL:-}
if [ -z "$genuine" ] || [ ! -f "$genuine" ]; then
    echo "Skipping NaturalPoint's NPClient: set NP_GENUINE_DLL to its NPClient64.dll."
    exit 0
fi
cp "$genuine" genuine64.dll
run ./fakepose.exe 12 >fakepose.out 2>&1 &
sleep 2
for id in 1001 7503; do
    run ./npgame.exe $id "$(winpath "$work/genuine64.dll")" | tee npgame-$id.out
    grep -q '^version: 0x0500' npgame-$id.out
    grep -q "status 0 frame [0-9]* $pose" npgame-$id.out
    grep -q '^0 bad checksums' npgame-$id.out
done
wait
cat fakepose.out
grep -q 'asked to recenter 2 times' fakepose.out
grep -q 'at most 1 games tracking, 0 at the end' fakepose.out
echo "NaturalPoint's NPClient OK"
