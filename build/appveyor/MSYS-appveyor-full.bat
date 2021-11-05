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
:: Appveyor script for MSYS
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


ECHO Unsupported PROFILE=%PROFILE% or PLATFORM=%PLATFORM%
EXIT /B 1


SET BASH=C:\msys64\usr\bin\bash
SET CMAKE=/c/msys64/mingw64/bin/cmake.exe


CALL win_showenv.bat || EXIT /B

SET PACKAGES=^
  base-devel ^
  mingw-w64-x86_64-toolchain ^
  bison ^
  flex ^
  make ^
  mingw-w64-x86_64-cmake ^
  mingw-w64-x86_64-libevent ^
  mingw-w64-x86_64-openssl ^
  mingw-w64-x86_64-zlib

:: Upgrade things
%BASH% -lc "pacman --noconfirm -Syu %IGNORE%" || EXIT /B
%BASH% -lc "pacman --noconfirm -Su %IGNORE%" || EXIT /B
%BASH% -lc "pacman --noconfirm --needed -S %PACKAGES%" || EXIT /B


::
:: Configure and build our software with cmake
::

SET CMAKEARGS=^
  -G'%GENERATOR%' ^
  -DBoost_DEBUG=ON ^
  -DBoost_NAMESPACE=libboost ^
  -DBOOST_INCLUDEDIR=%BOOST_INCLUDEDIR% ^
  -DBOOST_LIBRARYDIR=%BOOST_LIBRARYDIR% ^
  -DCMAKE_BUILD_TYPE=%CONFIGURATION% ^
  -DCMAKE_C_COMPILER=gcc.exe ^
  -DCMAKE_CXX_COMPILER=g++.exe ^
  -DCMAKE_MAKE_PROGRAM=make.exe ^
  -DCMAKE_INSTALL_PREFIX=%INSTDIR_MSYS% ^
  -DLIBEVENT_ROOT=%LIBEVENT_ROOT% ^
  -DOPENSSL_LIBRARIES=%OPENSSL_LIBRARIES% ^
  -DOPENSSL_ROOT_DIR=%OPENSSL_ROOT% ^
  -DOPENSSL_USE_STATIC_LIBS=ON ^
  -DWITH_BOOST_STATIC=ON ^
  -DWITH_JAVA=OFF ^
  -DWITH_LIBEVENT=ON ^
  -DWITH_PYTHON=%WITH_PYTHON% ^
  -DWITH_SHARED_LIB=OFF ^
  -DWITH_STATIC_LIB=ON

%BASH% -lc "mkdir %BUILDDIR% && cd %BUILDDIR% && %CMAKE% %SRCDIR_MSYS% %CMAKEARGS% && %CMAKE% --build . --config %CONFIGURATION% && %CMAKE% --install . --config %CONFIGURATION%" || EXIT /B


::
:: Execute our tests
::

:: This test randomly fails on mingw; see Jira THRIFT-4106
SET DISABLED_TESTS=(concurrency_test)
SET DISABLED_TESTS_COMMAND=--exclude-regex '%DISABLED_TESTS%'

%BASH% -lc "cd %BUILDDIR% && ctest.exe --build-config %CONFIGURATION% --timeout 300 --extra-verbose %DISABLED_TESTS_COMMAND%" || EXIT /B
