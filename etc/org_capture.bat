@echo off
set URL=%1
set URL=%URL:&=^&%
set URL=%URL:/?=?%
set URL=%URL:://=:///%
start "" "C:\msys64\mingw64\bin\emacsclientw.exe"  -na "C:\msys64\mingw64\bin\runemacs.exe" "%URL%"
