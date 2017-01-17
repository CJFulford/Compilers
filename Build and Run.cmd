@echo off
setlocal

cd source
ghc main.hs -odir "compiled" -hidir "compiled"

echo .
echo .
echo Build Complete
echo .
echo .

main

pause