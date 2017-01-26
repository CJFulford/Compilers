@echo off
setlocal

cd compiled

set v0=customTest1.m-

set list= %v0%

for %%i in (%list%) do (
main ../tests/%%i
echo .
echo .
)
pause