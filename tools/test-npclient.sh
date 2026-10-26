#!/bin/sh
# Check NPClient end to end without a camera or a game: under Wine, fakepose
# publishes a known pose and npgame reads it through NPClient64.dll as a game
# would, unscrambling and checksumming it. Needs mingw-w64 and wine.
set -eu
src=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"

cc=x86_64-w64-mingw32-gcc
flags="-std=c11 -Wall -Wextra -Werror -O2"
$cc $flags -s -shared -static-libgcc -Wl,--kill-at -o NPClient64.dll "$src/npclient/npclient.c"
$cc $flags -static -I"$src/cbits" -o fakepose.exe "$src/tools/fakepose.c" "$src/cbits/windows.c" -ladvapi32 -luser32
$cc $flags -static -o npgame.exe "$src/tools/npgame.c" -ladvapi32

export WINEDEBUG=-all WINEPREFIX="$work/prefix"
wineboot -i >/dev/null 2>&1
wine fakepose.exe 6 >fakepose.out 2>&1 &
sleep 2
wine npgame.exe 1003 "$(winepath -w "$work/NPClient64.dll")" | tee npgame.out
wait
cat fakepose.out

# fakepose publishes pitch 10, roll -5 and x/y/z 12/-34/56 throughout.
grep -q 'status 0 frame 1 checksum ok .*pitch  +10.00 roll   -5.00 deg  x  +12.0 y  -34.0 z  +56.0 mm' npgame.out
grep -q '^0 bad checksums' npgame.out
grep -q 'asked to recenter 1 times' fakepose.out
echo "NPClient OK"
