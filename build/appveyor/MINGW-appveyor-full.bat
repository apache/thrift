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
:: Appveyor script for MINGW on MSYS2
::

::
:: Installs third party packages we need for a cmake build
::

@ECHO ON
SETLOCAL EnableDelayedExpansion

CD build\appveyor || EXIT /B

SET APPVEYOR_SCRIPTS=%APPVEYOR_BUILD_FOLDER%\build\appveyor
SET BUILDDIR=%APPVEYOR_BUILD_FOLDER%\..\build\%PROFILE%\%PLATFORM%
SET INSTDIR=%APPVEYOR_BUILD_FOLDER%\..\install\%PROFILE%\%PLATFORM%
SET SRCDIR=%APPVEYOR_BUILD_FOLDER%


:: PLATFORM is x86 or x64
:: NORM_PLATFORM is 32 or 64
IF "%PLATFORM%" == "x86" (
    SET NORM_PLATFORM=32
) ELSE (
    SET NORM_PLATFORM=64
)

:: PLATFORM = x86 means MINGWPLAT i686
:: PLATFORM = x64 means MINGWPLAT x86_64
IF "%PLATFORM%" == "x86" (
  SET MINGWPLAT=i686
) ELSE (
  SET MINGWPLAT=x86_64
)


:: compiler and generator detection
SET COMPILER=gcc
SET GENERATOR=MinGW Makefiles


SET BASH=C:\msys64\usr\bin\bash.exe
!BASH! -lc "sed -i '/export PATH=\/mingw32\/bin/d' ~/.bash_profile && sed -i '/export PATH=\/mingw64\/bin/d' ~/.bash_profile && echo 'export PATH=/mingw%NORM_PLATFORM%/bin:$PATH' >> ~/.bash_profile" || EXIT /B

SET BUILDDIR=%BUILDDIR:\=/%
SET BUILDDIR=/c!BUILDDIR:~2!
SET INSTDIR=%INSTDIR:\=/%
SET INSTDIR=/c!INSTDIR:~2!
SET SRCDIR=%SRCDIR:\=/%
SET SRCDIR=/c!SRCDIR:~2!

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


::
:: Configure and build our software with cmake
::

SET CMAKEARGS=^
  -G'%GENERATOR%' ^
  -DCMAKE_BUILD_TYPE=%CONFIGURATION% ^
  -DCMAKE_INSTALL_PREFIX=%INSTDIR% ^
  -DCMAKE_MAKE_PROGRAM=/mingw%NORM_PLATFORM%/bin/mingw32-make ^
  -DCMAKE_C_COMPILER=/mingw%NORM_PLATFORM%/bin/gcc.exe ^
  -DCMAKE_CXX_COMPILER=/mingw%NORM_PLATFORM%/bin/g++.exe ^
  -DOPENSSL_ROOT_DIR=/mingw%NORM_PLATFORM% ^
  -DWITH_PYTHON=OFF

%BASH% -lc "mkdir -p %BUILDDIR% && cd %BUILDDIR% && cmake.exe %SRCDIR% %CMAKEARGS% && cmake --build . --config %CONFIGURATION% && cmake --install . --config %CONFIGURATION%" || EXIT /B


::
:: Execute our tests
::

SET DISABLED_TESTS_COMMAND=--exclude-regex '%DISABLED_TESTS%'

%BASH% -lc "cd %BUILDDIR% && ctest.exe --build-config %CONFIGURATION% --timeout 300 --extra-verbose %DISABLED_TESTS_COMMAND%" || EXIT /B
