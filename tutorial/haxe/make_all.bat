@echo off
setlocal
if "%HOMEDRIVE%"=="" goto MISSINGVARS
if "%HOMEPATH%"=="" goto MISSINGVARS
if "%HAXEPATH%"=="" goto NOTINSTALLED

set path=%HAXEPATH%;%HAXEPATH%\..\neko;%path%

rem # invoke Thrift comnpiler
thrift -r -gen haxe   ..\tutorial.thrift

rem # invoke Haxe compiler for all targets
for %%a in (*.hxml) do (
	echo --------------------------
	echo Building %%a ...
	echo --------------------------
	haxe  --cwd .  %%a
)


echo.
echo done.
pause
goto eof

:NOTINSTALLED
echo FATAL: Either Haxe is not installed, or the HAXEPATH variable is not set.
pause
goto eof

:MISSINGVARS
echo FATAL: Unable to locate home folder.
echo.
echo Both HOMEDRIVE and HOMEPATH need to be set to point to your Home folder.
echo The current values are:
echo HOMEDRIVE=%HOMEDRIVE%
echo HOMEPATH=%HOMEPATH%
pause
goto eof

:eof
