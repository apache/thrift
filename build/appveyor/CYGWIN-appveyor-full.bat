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
:: Appveyor script for CYGWIN
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


:: compiler and generator detection
SET COMPILER=gcc
SET GENERATOR=Unix Makefiles

IF "%PLATFORM%" == "x64" (
  SET CYGWINROOT=C:\cygwin64
) ELSE (
  SET CYGWINROOT=C:\cygwin
)

IF "%PLATFORM%" == "x64" (
  SET SETUP=!CYGWINROOT!\setup-x86_64.exe
) ELSE (
  SET SETUP=!CYGWINROOT!\setup-x86.exe
)

SET BASH=!CYGWINROOT!\bin\bash.exe
SET BUILDDIR=%BUILDDIR:\=/%
SET BUILDDIR=/cygdrive/c!BUILDDIR:~2!
SET INSTDIR=%INSTDIR:\=/%
SET INSTDIR=/cygdrive/c!INSTDIR:~2!
SET SRCDIR=%SRCDIR:\=/%
SET SRCDIR=/cygdrive/c!SRCDIR:~2!


CALL win_showenv.bat || EXIT /B

::
:: Install apt-cyg for package management because its easier to use
:: than Cygwins setup.exe. But both are possible to use.
::

%BASH% -lc "wget https://rawgit.com/transcode-open/apt-cyg/master/apt-cyg && install apt-cyg /bin && rm -f apt-cyg" || EXIT /B
%BASH% -lc "apt-cyg update" || EXIT /B
%BASH% -lc "apt-cyg install unzip xz cmake make bison flex gcc-g++ libboost-devel libevent-devel openssl-devel zlib-devel" || EXIT /B


::
:: Configure and build our software with cmake
::

SET CMAKEARGS=^
  -G'%GENERATOR%' ^
  -DCMAKE_BUILD_TYPE=%CONFIGURATION% ^
  -DCMAKE_INSTALL_PREFIX=%INSTDIR% ^
  -DCMAKE_CXX_FLAGS="-D_GNU_SOURCE" ^
  -DWITH_JAVA=OFF ^
  -DWITH_PYTHON=OFF

:: -DCMAKE_CXX_EXTENSIONS=ON ^
:: -DCMAKE_CXX_STANDARD=11 ^


%BASH% -lc "mkdir -p %BUILDDIR% && cd %BUILDDIR% && cmake.exe %SRCDIR% %CMAKEARGS% && cmake --build . --config %CONFIGURATION% && cmake --install . --config %CONFIGURATION%" || EXIT /B


::
:: Execute our tests
::

SET DISABLED_TESTS_COMMAND=--exclude-regex '%DISABLED_TESTS%'

%BASH% -lc "cd %BUILDDIR% && ctest.exe --build-config %CONFIGURATION% --timeout 300 --extra-verbose %DISABLED_TESTS_COMMAND%" || EXIT /B
