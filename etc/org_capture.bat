@echo off
set URL=%1
set URL=%URL:&=^&%
set URL=%URL:/?=?%
set URL=%URL:://=:///%
start "" "C:\Program Files\Emacs\emacs-28.0.50-snapshot\bin\emacsclientw.exe" "%URL%"
