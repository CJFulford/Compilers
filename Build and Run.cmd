@echo off
setlocal

cd source
ghc main.hs -odir "compiled" -hidir "compiled"
move main.exe compiled/main.exe

echo .
echo .
echo Build Complete
echo .
echo .

cd compiled
main

pause