@echo off
setlocal

cd compiled

set v0=customTest1.m-
set v1=customTest2.m-
set v2=customTest3.m-

set list= %v0% %v1% %v2%

for %%i in (%list%) do (
main ../tests/%%i
echo .
echo .
)
pause