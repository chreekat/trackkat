#!/bin/sh
# Run Windows cabal under Wine, to build and test trackkat's Windows-only
# code (the window, the tray, the TrackIR's own driver) from Linux:
#
#     tools/wine.sh cabal build all --enable-tests
#     tools/wine.sh cabal test
#     tools/wine.sh cabal run trackkat
#
# Any other command runs under Wine as is: tools/wine.sh ghc --version.
# Needs wine on PATH, such as nixpkgs' wineWow64Packages.stable. The first
# run downloads CI's GHC and cabal, a curl for cabal to download with, and
# MSYS2's libusb into $TRACKKAT_WINE (~/.cache/trackkat-wine by default);
# builds go to dist-wine, beside Linux's dist-newstyle.
set -eu
src=$(cd "$(dirname "$0")/.." && pwd)
home=${TRACKKAT_WINE:-$HOME/.cache/trackkat-wine}
ghc=ghc-9.14.1-x86_64-unknown-mingw32
cabal=cabal-install-3.18.2.0-x86_64-mingw64
curl=curl-8.23.0_10-win64-mingw
libusb=mingw-w64-ucrt-x86_64-libusb-1.0.30-1-any.pkg.tar.zst

export WINEPREFIX="$home/prefix" WINEDEBUG=-all

fetch() { # url sha256
    file="$home/dl/${1##*/}"
    if [ ! -f "$file" ]; then
        mkdir -p "$home/dl"
        curl -fSL -o "$file.part" "$1"
        mv "$file.part" "$file"
    fi
    echo "$2  $file" | sha256sum -c --quiet
}

if [ ! -f "$home/ready" ]; then
    fetch "https://downloads.haskell.org/~ghc/9.14.1/$ghc.tar.xz" \
        4914e262a91a9cb65807ec33b3a2adc884894b1dcc33691d820bb9456f2794ba
    fetch "https://downloads.haskell.org/~cabal/cabal-install-3.18.2.0/$cabal.zip" \
        7430982fd2aedef41539155d819e7e24349c664206ecf8c6e53c1c31acdbe630
    fetch "https://curl.se/windows/dl-8.23.0_10/$curl.zip" \
        7157104332c15c80edba0359446584cb317c63f4c3cf5e569b76ecad5aa0e197
    fetch "https://repo.msys2.org/mingw/ucrt64/$libusb" \
        20106e6ff31b4581bb111b28a68b6b79a50dc3e5fc3033944e1c4b3ff8cd812e
    rm -rf "$home/tools" "$home/libusb"
    mkdir -p "$home/tools/cabal" "$home/libusb"
    tar -xJf "$home/dl/$ghc.tar.xz" -C "$home/tools"
    (cd "$home/tools/cabal" && unzip -oq "$home/dl/$cabal.zip")
    unzip -oq "$home/dl/$curl.zip" -d "$home/tools"
    tar --zstd -xf "$home/dl/$libusb" -C "$home/libusb"
    wineboot -i >/dev/null 2>&1
    # Else Wine's joystick support says on every start that it has no SDL.
    wine reg add 'HKLM\System\CurrentControlSet\Services\winebus' /v 'Enable SDL' /t REG_DWORD /d 0 /f >/dev/null 2>&1
fi

usb="Z:$home/libusb/ucrt64"
export WINEPATH="Z:$home/tools/$curl/bin;Z:$home/tools/$ghc/bin;Z:$home/tools/cabal;$usb/bin"
export CABAL_DIR="Z:$home/cabal"

if [ ! -f "$home/ready" ]; then
    wine cabal.exe user-config init >/dev/null 2>&1 || true
    # Windows cabal downloads with PowerShell, which Wine hasn't got.
    sed -i 's/^-- http-transport:.*/http-transport: curl/' "$home/cabal/config"
    printf 'extra-include-dirs: %s\\include\nextra-lib-dirs: %s\\lib\n' "$usb" "$usb" >>"$home/cabal/config"
    wine cabal.exe update
    touch "$home/ready"
fi

cd "$src"
case $1 in
cabal)
    shift
    case ${1:-} in
    build | test | run | repl | list-bin | exec | install | haddock | bench | clean)
        cmd=$1
        shift
        exec wine cabal.exe "$cmd" --builddir=dist-wine "$@"
        ;;
    *) exec wine cabal.exe "$@" ;;
    esac
    ;;
*)
    exec wine "$@"
    ;;
esac
