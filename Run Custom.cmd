@echo off
setlocal

cd compiled

set v1=customTest1.m-
set v2=customTest2.m-
set v3=customTest3.m-
set v4=customTest4.m-
set v5=customTest5.m-
set v6=customTestForEmptyFile.m-

set list= %v1% %v2% %v3% %v4% %v5% %v6%

for %%i in (%list%) do (
main ../tests/%%i
echo .
echo .
)

:: test for wrong number of parameters
main

pause