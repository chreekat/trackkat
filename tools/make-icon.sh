#!/bin/sh
# Make the icons that icons/trackkat.rc builds into trackkat's programs, at
# each size Windows asks for: icons/trackkat.ico, the program's, from
# icons/trackkat.svg; and the tray icon in each of trackkat's states,
# icons/tray-<state>.ico, from icons/tray.svg with its eyes in the colours
# its comment lists. Needs rsvg-convert (librsvg) and ImageMagick:
#   nix shell nixpkgs#librsvg nixpkgs#imagemagick -c tools/make-icon.sh
set -e
cd "$(dirname "$0")/.."
tmp=$(mktemp -d)
trap 'rm -r "$tmp"' EXIT

# ico <svg> <out.ico> <sizes...>
ico() {
    svg=$1 out=$2
    shift 2
    pngs=""
    for size in "$@"; do
        rsvg-convert -w "$size" -h "$size" "$svg" -o "$tmp/$size.png"
        pngs="$pngs $tmp/$size.png"
    done
    convert $pngs "$out"
}

ico icons/trackkat.svg icons/trackkat.ico 16 20 24 32 40 48 64 256

# The comment's colours, glow then centre, in the order of the states
# below; the drawing itself shows the first.
colours=$(sed -n '/^<!--/,/-->/p' icons/tray.svg | grep -o '#[0-9a-f]\{6\}')
set -- $colours
glow=$1 centre=$2
for state in tracking clip-lost searching no-camera; do
    sed "s/$glow/$1/g; s/$centre/$2/g" icons/tray.svg > "$tmp/tray.svg"
    ico "$tmp/tray.svg" "icons/tray-$state.ico" 16 20 24 32 40 48
    shift 2
done
