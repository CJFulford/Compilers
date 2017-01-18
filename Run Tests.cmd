@echo off
setlocal

cd compiled

set v0=test1.m-
set v1=test2.m-
set v2=test3.m-
set v3=test4.m-
set v4=test5.m-
set v5=test6.m-
set v6=test9.m-

set list= %v0% %v1% %v2% %v3% %v4% %v5% %v6%

for %%i in (%list%) do (
echo.
echo.
echo ===================
echo %%i
echo ===================

main ../tests/%%i
)

echo .
echo .
echo ===================
echo Testing Complete
echo ===================


pause