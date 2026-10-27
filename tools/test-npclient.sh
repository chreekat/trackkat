#!/bin/sh
# Check NPClient end to end without a camera or a game: under Wine, fakepose
# publishes a known pose and npgame reads it through NPClient64.dll as a game
# would, unscrambling and checksumming it. Needs mingw-w64 and wine.
#
# Our DLL is checked as a game in DCS (1003, scrambled). Then, if
# NP_GENUINE_DLL names NaturalPoint's own NPClient64.dll (never in this
# repository), fakepose stands in for NaturalPoint's software and npgame
# reads through that DLL, as a game with plain data (1001) and as ArmA 3
# (7503, scrambled).
set -eu
# NaturalPoint's DLL finds its software by a window, which Wine keeps only
# with a display.
if [ -n "${NP_GENUINE_DLL:-}" ] && [ -z "${DISPLAY:-}" ] && command -v xvfb-run >/dev/null; then
    exec xvfb-run -a sh "$0" "$@"
fi
src=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"

cc=x86_64-w64-mingw32-gcc
flags="-std=c11 -Wall -Wextra -Werror -O2"
$cc $flags -s -shared -static-libgcc -Wl,--kill-at -o NPClient64.dll "$src/npclient/npclient.c" "$src/cbits/npclient_common.c"
$cc $flags -static -I"$src/cbits" -o fakepose.exe "$src/tools/fakepose.c" "$src/cbits/windows.c" "$src/cbits/joystick.c" \
    "$src/cbits/trackir.c" "$src/cbits/npclient_common.c" -lhid -ladvapi32 -luser32
$cc $flags -static -o npgame.exe "$src/tools/npgame.c" -ladvapi32 -luser32

export WINEDEBUG=-all WINEPREFIX="$work/prefix"
wineboot -i >/dev/null 2>&1

# fakepose publishes pitch 10, roll -5 and x/y/z 12/-34/56 throughout.
pose='checksum ok .*pitch  +10.00 roll   -5.00 deg  x  +12.0 y  -34.0 z  +56.0 mm'

wine fakepose.exe 6 >fakepose.out 2>&1 &
sleep 2
wine npgame.exe 1003 "$(winepath -w "$work/NPClient64.dll")" | tee npgame.out
wait
cat fakepose.out
grep -q "status 0 frame 1 $pose" npgame.out
grep -q '^0 bad checksums' npgame.out
grep -q 'asked to recenter 1 times' fakepose.out
grep -q 'at most 1 games tracking, 0 at the end' fakepose.out
echo "NPClient OK"

genuine=${NP_GENUINE_DLL:-}
if [ -z "$genuine" ] || [ ! -f "$genuine" ]; then
    echo "Skipping NaturalPoint's NPClient: set NP_GENUINE_DLL to its NPClient64.dll."
    exit 0
fi
cp "$genuine" genuine64.dll
wine fakepose.exe 12 >fakepose.out 2>&1 &
sleep 2
for id in 1001 7503; do
    wine npgame.exe $id "$(winepath -w "$work/genuine64.dll")" | tee npgame-$id.out
    grep -q '^version: 0x0500' npgame-$id.out
    grep -q "status 0 frame [0-9]* $pose" npgame-$id.out
    grep -q '^0 bad checksums' npgame-$id.out
done
wait
cat fakepose.out
grep -q 'asked to recenter 2 times' fakepose.out
grep -q 'at most 1 games tracking, 0 at the end' fakepose.out
echo "NaturalPoint's NPClient OK"
