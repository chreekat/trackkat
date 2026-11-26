# Check a dist folder CI zips, before signing and after: on Windows, as
#
#     pwsh -File tools/check-dist.ps1 dist C:\msys64\ucrt64\bin\objdump.exe
param([string] $dist, [string] $objdump)
$ErrorActionPreference = 'Stop'
$dist = (Resolve-Path $dist).Path

# The icon, for Explorer and trackkat's window.
if (-not (& $objdump -h "$dist\trackkat.exe" | Select-String '\.rsrc')) { "::error::trackkat.exe has no icon"; exit 1 }

# Installing copies, and updates unpack, the files trackkat names.
$listed = & "$dist\trackkat.exe" --list-files | Sort-Object
$packed = Get-ChildItem $dist -Name | Sort-Object
$diff = Compare-Object $listed $packed
if ($diff) { "::error::$dist doesn't hold the files trackkat --list-files names: $($diff | Out-String)"; exit 1 }

# trackkat.exe is a GUI program, so it never opens a console window;
# tirprobe is a console program.
foreach ($t in @(@{ exe = "trackkat.exe"; subsystem = 2 }, @{ exe = "tirprobe.exe"; subsystem = 3 })) {
    $bytes = [IO.File]::ReadAllBytes("$dist\$($t.exe)")
    $subsystem = [BitConverter]::ToUInt16($bytes, [BitConverter]::ToInt32($bytes, 0x3C) + 92)
    "$($t.exe): PE subsystem $subsystem"
    if ($subsystem -ne $t.subsystem) { "::error::$($t.exe) has PE subsystem $subsystem, not $($t.subsystem)"; exit 1 }
}
