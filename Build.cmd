@echo off
setlocal

ghc src\main.hs -odir "compiled" -hidir "compiled"
move "src\main.exe" "compiled\main.exe"

echo ==============
echo Build Complete
echo ==============

pause