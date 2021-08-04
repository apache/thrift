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

CD build\appveyor || EXIT /B
CALL win_banner_install.bat || EXIT /B
CALL win_setenv.bat || EXIT /B
CALL win_showenv.bat || EXIT /B

SET PACKAGES=^
  base-devel ^
  mingw-w64-x86_64-toolchain ^
  bison ^
  flex ^
  make ^
  mingw-w64-%MINGWPLAT%-boost ^
  mingw-w64-%MINGWPLAT%-cmake ^
  mingw-w64-%MINGWPLAT%-libevent ^
  mingw-w64-%MINGWPLAT%-openssl ^
  mingw-w64-%MINGWPLAT%-toolchain ^
  mingw-w64-%MINGWPLAT%-zlib

::mingw-w64-%MINGWPLAT%-qt5 : WAY too large (1GB download!) - tested in cygwin builds anyway

:: Upgrade things
%BASH% -lc "pacman --noconfirm -Syu %IGNORE%" || EXIT /B
%BASH% -lc "pacman --noconfirm -Su %IGNORE%" || EXIT /B
%BASH% -lc "pacman --noconfirm --needed -S %PACKAGES%" || EXIT /B


:: These instructions are for a manual update of specific package versions.
:: Fall back to this in case the above does not work anymore (broken upstream).
:::: Updata the new key
::%BASH% -lc "curl -O http://repo.msys2.org/msys/x86_64/msys2-keyring-1~20210213-2-any.pkg.tar.xz" || EXIT /B
::%BASH% -lc "curl -O http://repo.msys2.org/msys/x86_64/msys2-keyring-1~20210213-2-any.pkg.tar.xz.sig" || EXIT /B
::%BASH% -lc "pacman-key --verify msys2-keyring-1~20210213-2-any.pkg.tar.xz.sig" || EXIT /B
::%BASH% -lc "pacman --noconfirm -U --config <(echo) msys2-keyring-1~20210213-2-any.pkg.tar.xz" || EXIT /B
:::: Upgrade things
::%BASH% -lc "pacman --noconfirm -Sy" || EXIT /B
::%BASH% -lc "pacman --noconfirm -Udd https://repo.msys2.org/msys/x86_64/pacman-5.2.2-5-x86_64.pkg.tar.xz" || EXIT /B
::%BASH% -lc "pacman --noconfirm --needed -S %PACKAGES%" || EXIT /B
