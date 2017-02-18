@echo off
setlocal

ghc -XDeriveGeneric -XDeriveAnyClass src\main.hs -odir  "compiled" -hidir "compiled"
echo .
echo .
move "src\main.exe" "compiled\main.exe"

pause