@echo off
setlocal

cd compiled

set list="Lexer Custom Test 1.m-"
set list=%list%;"Lexer Custom Test 2.m-"
set list=%list%;"Lexer Custom Test 3.m-"
set list=%list%;"Lexer Custom Test 4.m-"
set list=%list%;"Lexer Custom Test 5.m-"
set list=%list%;"Lexer Custom Test 6 - Empty.m-"


for %%i in (%list%) do (
main ../tests/Lexer/%%i
echo .
echo .
)

:: test for wrong number of parameters
main

pause