@echo off
set URL=%1
set URL=%URL:&=^&%
set URL=%URL:/?=?%
set URL=%URL:://=:///%
start "" "C:\Program Files\Emacs\x86_64\bin\emacsclientw.exe"  -na "C:\Program Files\Emacs\x86_64\bin\runemacs.exe" "%URL%"
