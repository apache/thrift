::
:: Licensed under the Apache License, Version 2.0 (the "License");
:: you may not use this file except in compliance with the License.
:: You may obtain a copy of the License at
::
::     http://www.apache.org/licenses/LICENSE-2.0
::
:: Unless required by applicable law or agreed to in writing, software
:: distributed under the License is distributed on an "AS IS" BASIS,
:: WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
:: See the License for the specific language governing permissions and
:: limitations under the License.
::

::
:: Appveyor install script for MINGW on MSYS2
:: Installs third party packages we need for a cmake build
::

@ECHO OFF
SETLOCAL EnableDelayedExpansion

CD build\appveyor                          || EXIT /B
CALL cl_banner_install.bat                 || EXIT /B
CALL cl_setenv.bat                         || EXIT /B
CALL cl_showenv.bat                        || EXIT /B

SET PACKAGES=^
  --needed -S bison flex make ^
  mingw-w64-%MINGWPLAT%-boost ^
  mingw-w64-%MINGWPLAT%-cmake ^
  mingw-w64-%MINGWPLAT%-libevent ^
  mingw-w64-%MINGWPLAT%-openssl ^
  mingw-w64-%MINGWPLAT%-toolchain ^
  mingw-w64-%MINGWPLAT%-zlib

::mingw-w64-%MINGWPLAT%-qt5 : WAY too large (1GB download!) - tested in cygwin builds anyway

:: the following uninstall and system upgrade was causing issues; appveyor's is relatively new
:: Remove old packages that no longer exist to avoid an error
:: %BASH% -lc "pacman --noconfirm --remove libcatgets catgets || true" || EXIT /B

:: Remove incompatible packages 8.2.0-3 and 7.3.0-2 (mingw packaging bugs if you ask me!)
:: %BASH% -lc "pacman --noconfirm --remove mingw-w64-x86_64-gcc-ada mingw-w64-x86_64-gcc-objc || true" || EXIT /B
:: %BASH% -lc "pacman --noconfirm --remove mingw-w64-x86_64-gcc-ada mingw-w64-x86_64-gcc-objc || true" || EXIT /B

:: Upgrade things
:: %BASH% -lc "pacman --noconfirm -Syu %IGNORE%"                       || EXIT /B
:: %BASH% -lc "pacman --noconfirm -Su %IGNORE%"                        || EXIT /B
::
%BASH% -lc "pacman --noconfirm %PACKAGES%"                          || EXIT /B
