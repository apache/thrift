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
:: Appveyor script for MSVC
::

::
:: Installs (or builds) third party packages we need
::

@ECHO ON
SETLOCAL EnableDelayedExpansion

CD build\appveyor || EXIT /B

SET APPVEYOR_SCRIPTS=%APPVEYOR_BUILD_FOLDER%\build\appveyor
SET BUILDDIR=%APPVEYOR_BUILD_FOLDER%\..\build\%PROFILE%\%PLATFORM%
SET INSTDIR=%APPVEYOR_BUILD_FOLDER%\..\install\%PROFILE%\%PLATFORM%
SET SRCDIR=%APPVEYOR_BUILD_FOLDER%


IF "%PROFILE%" == "MSVC2015" (
  IF "%PLATFORM%" == "x86" (
    CALL "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86 || EXIT /B
  ) ELSE (
    CALL "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.cmd" /x64 || EXIT /B
    CALL "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86_amd64 || EXIT /B
  )
) ELSE IF "%PROFILE%" == "MSVC2017" (
  IF "%PLATFORM%" == "x86" (
    CALL "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars32.bat" || EXIT /B
  ) ELSE (
    CALL "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars64.bat" || EXIT /B
  )
) ELSE IF "%PROFILE%" == "MSVC2019" (
  IF "%PLATFORM%" == "x86" (
    CALL "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat" || EXIT /B
  ) ELSE (
    CALL "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat" || EXIT /B
  )
) ELSE (
  ECHO Unsupported PROFILE=%PROFILE% or PLATFORM=%PLATFORM%
  EXIT /B 1
)


:: compiler and generator detection
IF /i "%PLATFORM%" == "x64" SET GENARCH= Win64
IF "%PROFILE%" == "MSVC2015" (
  SET GENERATOR=Visual Studio 14 2015!GENARCH!
  SET COMPILER=vc140
) ELSE IF "%PROFILE%" == "MSVC2017" (
  SET GENERATOR=Visual Studio 15 2017!GENARCH!
  SET COMPILER=vc141
) ELSE IF "%PROFILE%" == "MSVC2019" (
  SET GENERATOR=Visual Studio 16 2019!GENARCH!
  SET COMPILER=vc142
) ELSE (
  ECHO [error] unable to determine the CMake generator and compiler to use from MSVC profile %PROFILE%
  EXIT /B 1
)

:: PLATFORM is x86 or x64
:: NORM_PLATFORM is 32 or 64
IF "%PLATFORM%" == "x86" (
    SET NORM_PLATFORM=32
) ELSE (
    SET NORM_PLATFORM=64
)

:: FindBoost needs forward slashes so cmake doesn't see something as an escaped character
SET BOOST_ROOT=C:/Libraries/boost_%BOOST_VERSION:.=_%
SET BOOST_LIBRARYDIR=!BOOST_ROOT!/lib%NORM_PLATFORM%-msvc-%COMPILER:~-3,2%.%COMPILER:~-1,1%
SET OPENSSL_ROOT=C:\OpenSSL-Win%NORM_PLATFORM%
SET WIN3P=%APPVEYOR_BUILD_FOLDER%\thirdparty

IF "%PYTHON_VERSION%" == "" (
  SET WITH_PYTHON=OFF
) ELSE (
  SET WITH_PYTHON=ON
  IF /i "%PLATFORM%" == "x64" (SET PTEXT=-x64)
  SET PATH=C:\Python%PYTHON_VERSION:.=%!PTEXT!\scripts;C:\Python%PYTHON_VERSION:.=%!PTEXT!;!PATH!
)

IF "%CONFIGURATION%" == "Debug" (SET ZLIB_LIB_SUFFIX=d)

IF NOT "%QT_VERSION%" == "" (
  IF /i "%PLATFORM%" == "x64" (SET QTEXT=_64)
  SET PATH=C:\Qt\%QT_VERSION%\%PROFILE%!QTEXT!\bin;!PATH!
)


CALL win_showenv.bat || EXIT /B
MKDIR "%WIN3P%" || EXIT /B

choco feature enable -n allowGlobalConfirmation || EXIT /B

:: Things to install when NOT running in appveyor:
IF "%APPVEYOR_BUILD_ID%" == "" (
    cup -y chocolatey || EXIT /B
    cinst -y curl || EXIT /B
    cinst -y 7zip || EXIT /B
    cinst -y python3 || EXIT /B
    cinst -y openssl.light || EXIT /B
)

cinst -y jdk8 || EXIT /B
cinst -y winflexbison3 || EXIT /B

:: zlib - not available through chocolatey
CD "%APPVEYOR_SCRIPTS%" || EXIT /B
call build-zlib.bat || EXIT /B

:: libevent - not available through chocolatey
CD "%APPVEYOR_SCRIPTS%" || EXIT /B
call build-libevent.bat || EXIT /B

:: python packages (correct path to pip set above)
pip.exe ^
    install backports.ssl_match_hostname ^
            ipaddress ^
            six ^
            tornado ^
            twisted || EXIT /B

:: Adobe Flex SDK 4.6 for ActionScript
MKDIR "C:\Adobe\Flex\SDK\4.6" || EXIT /B
appveyor DownloadFile http://download.macromedia.com/pub/flex/sdk/flex_sdk_4.6.zip -FileName C:\Adobe\Flex\SDK\4.6\SDK.zip || EXIT /B
CD "C:\Adobe\Flex\SDK\4.6" || EXIT /B
7z x SDK.zip || EXIT /B
SETX FLEX_HOME "C:\Adobe\Flex\SDK\4.6"


::
:: Configure and build our software with cmake
::

MKDIR "%BUILDDIR%" || EXIT /B
CD "%BUILDDIR%" || EXIT /B

:: When libraries cannot be found, things might have been updated
:: so uncomment this and submit a pull request to see what's there
:: now...
:: DIR C:\Libraries
:: DIR C:\Libraries\boost_1_69_0\lib*
:: DIR C:\Libraries\boost_1_68_0\lib*
:: DIR C:\Libraries\boost_1_67_0\lib*
:: DIR C:\Libraries\boost_1_66_0\lib*
:: DIR C:\Libraries\boost_1_65_0\lib*
:: DIR C:\Libraries\boost_1_64_0\lib*
:: DIR C:\Libraries\boost_1_63_0\lib*
:: DIR C:\Libraries\boost_1_62_0\lib*
:: DIR C:\Libraries\boost_1_61_0\lib*
:: DIR C:\Libraries\boost_1_60_0\lib*

cmake.exe "%SRCDIR%" ^
  -G"%GENERATOR%" ^
  -DBISON_EXECUTABLE="C:\ProgramData\chocolatey\lib\winflexbison3\tools\win_bison.exe" ^
  -DBOOST_ROOT="%BOOST_ROOT%" ^
  -DBOOST_LIBRARYDIR="%BOOST_LIBRARYDIR%" ^
  -DBUILD_SHARED_LIBS="%BUILD_SHARED_LIBS%" ^
  -DCMAKE_BUILD_TYPE="%CONFIGURATION%" ^
  -DCMAKE_INSTALL_PREFIX="%INSTDIR%" ^
  -DFLEX_EXECUTABLE="C:\ProgramData\chocolatey\lib\winflexbison3\tools\win_flex.exe" ^
  -DLIBEVENT_ROOT="%WIN3P%\libevent-%LIBEVENT_VERSION%-stable" ^
  -DOPENSSL_ROOT_DIR="%OPENSSL_ROOT%" ^
  -DOPENSSL_USE_STATIC_LIBS=OFF ^
  -DZLIB_LIBRARY="%WIN3P%\zlib-inst\lib\zlib%ZLIB_LIB_SUFFIX%.lib" ^
  -DZLIB_ROOT="%WIN3P%\zlib-inst" ^
  -DWITH_PYTHON=%WITH_PYTHON% || EXIT /B

cmake.exe --build . --config "%CONFIGURATION%" || EXIT /B

cmake.exe --install . --config "%CONFIGURATION%" || EXIT /B

::
:: Execute our tests
::

:: Add directories to the path to find DLLs of third party libraries so tests run properly!
SET PATH=%BOOST_LIBRARYDIR:/=\%;%OPENSSL_ROOT%\bin;%WIN3P%\zlib-inst\bin;%PATH%
SET DISABLED_TESTS_COMMAND=--exclude-regex '%DISABLED_TESTS%'

ctest.exe --build-config %CONFIGURATION% --timeout 300 --extra-verbose %DISABLED_TESTS_COMMAND% || EXIT /B
