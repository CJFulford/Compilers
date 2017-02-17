@echo off
setlocal

cd compiled

set list="Parser Test Custom 1.m-"
set list=%list%;"Parser Test Custom 2.m-"
set list=%list%;"Parser Test Custom 3.m-"
set list=%list%;"Parser Test Custom 4.m-"
set list=%list%;"Parser Test Custom 5.m-"

for %%i in (%list%) do (
main ../tests/Parser/%%i
echo .
echo .
)

pause