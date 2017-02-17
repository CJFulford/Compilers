@echo off
setlocal

cd compiled

set list="Parser Test 1.m-"
set list=%list%;"Parser Test 2.m-"
set list=%list%;"Parser Test 3.m-"
set list=%list%;"Parser Test 4.m-"
set list=%list%;"Parser Test 5.m-"

for %%i in (%list%) do (
main ../tests/Parser/%%i
echo .
echo .
)
pause