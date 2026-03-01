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
:: Build script example for inside the windows docker container
::
:: C:\build is the out-of-tree build directory
:: C:\install is the location where artifacts are placed
:: C:\thrift is where the sources are
::


IF EXIST "C:\Program Files\dotnet\dotnet.exe" (
  FOR /F "tokens=1" %%I IN ('"C:\Program Files\dotnet\dotnet.exe" --list-sdks 2^>NUL') DO (
    SET DOTNET_SDK_VERSION=%%I
    GOTO GOT_DOTNET_SDK
  )
)
:GOT_DOTNET_SDK
IF DEFINED DOTNET_SDK_VERSION (
  IF EXIST "C:\Program Files\dotnet\sdk\%DOTNET_SDK_VERSION%\Sdks" (
    SET MSBuildSDKsPath=C:\Program Files\dotnet\sdk\%DOTNET_SDK_VERSION%\Sdks
  )
)

:: Make and go into the out-of-tree directory
IF NOT EXIST C:\build (MKDIR C:\build)
cd c:\build

:: Generate the out-of-tree build files
cmake^
  -DCMAKE_GENERATOR_PLATFORM=x64^
  -DBOOST_ROOT=%BOOST_ROOT%^
  -DFLEX_HOME=%FLEX_HOME%^
  -DLIBEVENT_ROOT=%LIBEVENT_ROOT%^
  -DZLIB_ROOT=%ZLIB_ROOT%^
  -DCMAKE_BUILD_TYPE=Release^
  -DBUILD_SHARED_LIBS=OFF^
  -DCMAKE_INSTALL_PREFIX=C:\install^
  c:\thrift || EXIT /B

:: Build
cmake --build . --config Release || EXIT /B

:: Test
ctest -C Release || EXIT /B

:: Install (needed before netstd scripts so thrift.exe is in C:\install\bin)
cmake --install . || EXIT /B

:: Build and test .NET (netstd) library if dotnet CLI is available
IF /I "%THRIFT_BUILD_DOTNET%"=="OFF" GOTO SKIP_DOTNET
IF EXIST "C:\Program Files (x86)\dotnet\dotnet.exe" SET PATH=C:\Program Files (x86)\dotnet;%PATH%
IF EXIST "C:\Program Files\dotnet\dotnet.exe" SET PATH=C:\Program Files\dotnet;%PATH%
where dotnet >NUL 2>NUL
IF ERRORLEVEL 1 (
  ECHO dotnet CLI not found, skipping netstd build/test
  GOTO SKIP_DOTNET
)
SET HAS_DOTNET_SDK=
FOR /F "delims=" %%I IN ('dotnet --list-sdks 2^>NUL') DO SET HAS_DOTNET_SDK=1
IF NOT DEFINED HAS_DOTNET_SDK (
  ECHO dotnet SDK not found, skipping netstd build/test
  GOTO SKIP_DOTNET
)

PUSHD C:\thrift\lib\netstd
IF EXIST C:\install\bin\thrift.exe SET PATH=C:\install\bin;%PATH%
IF EXIST C:\build\bin\Release\thrift.exe SET PATH=C:\build\bin\Release;%PATH%
IF EXIST C:\build\compiler\cpp\Release\thrift.exe SET PATH=C:\build\compiler\cpp\Release;%PATH%
IF EXIST C:\build\compiler\cpp\thrift.exe SET PATH=C:\build\compiler\cpp;%PATH%
where thrift >NUL 2>NUL
IF ERRORLEVEL 1 (
  ECHO thrift compiler not found on PATH, skipping netstd build/test
  POPD
  GOTO SKIP_DOTNET
)
call build.cmd || EXIT /B
IF /I NOT "%THRIFT_TEST_DOTNET%"=="OFF" (
  call runtests.cmd || EXIT /B
)
POPD

:SKIP_DOTNET
