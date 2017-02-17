@echo off
setlocal

cd compiled

set list="Lexer Test 1.m-"
set list=%list%;"Lexer Test 2.m-"
set list=%list%;"Lexer Test 3.m-"
set list=%list%;"Lexer Test 4.m-"
set list=%list%;"Lexer Test 5.m-"
set list=%list%;"Lexer Test 6.m-"
set list=%list%;"Lexer Test 9.m-"

for %%i in (%list%) do (
main ../tests/Lexer/%%i
echo .
echo .
)
pause