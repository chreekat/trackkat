#!/bin/sh
# Make icons/trackkat.ico, the program's icon, from icons/trackkat.svg, at
# each size Windows asks for. Needs rsvg-convert (librsvg) and ImageMagick:
#   nix shell nixpkgs#librsvg nixpkgs#imagemagick -c tools/make-icon.sh
set -e
cd "$(dirname "$0")/.."
tmp=$(mktemp -d)
trap 'rm -r "$tmp"' EXIT
pngs=""
for size in 16 20 24 32 40 48 64 256; do
    rsvg-convert -w $size -h $size icons/trackkat.svg -o "$tmp/$size.png"
    pngs="$pngs $tmp/$size.png"
done
convert $pngs icons/trackkat.ico
