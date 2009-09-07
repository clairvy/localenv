echo off
rem MAKE1.BAT for SKK.
rem Copyright (C) 1999 Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem
rem Author: Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem Maintainer: SKK Development Team mailto:skk@ring.gr.jp
rem Version: $Id: make1.bat,v 1.5 2008/10/11 09:25:19 skk-cvs Exp $
rem Created: March 23, 1999
rem Last Modified: $Date: 2008/10/11 09:25:19 $

rem --- argument
rem ---   elc : byte compile
rem ---   all, install : install
rem ---   clean : cleaning garbage file
rem ---   what-where : print where to install
rem ---

rem --- check calling from makeit.bat
if not "%SUBMAKEOK%"=="OK" goto prnusage
set SUBMAKEOK=

rem argument check

set arg1=%1

if "%arg1%"=="elc" goto compile
if "%arg1%"=="all" goto install
if "%arg1%"=="install" goto install
if "%arg1%"=="what-where" goto listing
if "%arg1%"=="clean" goto clean
echo Unrecognized argument: specify either 'elc', 'all',
echo 'install', 'clean' or 'what-where'.
goto pauseend

:compile
%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-compile
goto end

:install
%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-install
goto end

:listing
%EMACS% -batch -q -no-site-file -l SKK-MK -f SKK-MK-what-where
goto end

:clean
del leim-list.el skk-autoloads.el skk-setup.el auto-autoloads.el custom-load.el *.elc doc\skk.info* *~

rem --- This file should not be executed by itself. Use makeit.bat.
:prnusage
echo This file should not be executed by itself. Use makeit.bat.

rem --- If error occurs, stay display until any key is typed.
:pauseend
echo Type any key when you're done reading the error message.
pause

:end

