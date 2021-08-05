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

@ECHO OFF

:: PLATFORM is x86 or x64
:: NORM_PLATFORM is 32 or 64
:: MSVCPLAT is x86 or amd64
IF "%PLATFORM%" == "x86" (
    SET NORM_PLATFORM=32
) ELSE (
    SET NORM_PLATFORM=64
)
IF "%PLATFORM%" == "x86" (
    SET MSVCPLAT=x86
) ELSE (
    SET MSVCPLAT=amd64
)

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
) ELSE IF "%PROFILE%" == "MINGW" (
  REM Supported, nothing special to do here.
) ELSE IF "%PROFILE%" == "CYGWIN" (
  REM Supported, nothing special to do here.
) ELSE (
  ECHO Unsupported PROFILE=%PROFILE% or PLATFORM=%PLATFORM%
  EXIT /B 1
)

CALL win_setcompiler.bat || EXIT /B
CALL win_setgenerator.bat || EXIT /B

SET APPVEYOR_SCRIPTS=%APPVEYOR_BUILD_FOLDER%\build\appveyor
SET BUILDDIR=%APPVEYOR_BUILD_FOLDER%\..\build\%PROFILE%\%PLATFORM%
SET INSTDIR=%APPVEYOR_BUILD_FOLDER%\..\install\%PROFILE%\%PLATFORM%
SET SRCDIR=%APPVEYOR_BUILD_FOLDER%


IF "%PROFILE_CLASS%" == "MSVC" (

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

) ELSE IF "%PROFILE_CLASS%" == "MINGW" (

  :: PLATFORM = x86 means MINGWPLAT i686
  :: PLATFORM = x64 means MINGWPLAT x86_64
  IF "%PLATFORM%" == "x86" (
    SET MINGWPLAT=i686
  ) ELSE (
    SET MINGWPLAT=x86_64
  )

  SET BASH=C:\msys64\usr\bin\bash.exe
  !BASH! -lc "sed -i '/export PATH=\/mingw32\/bin/d' ~/.bash_profile && sed -i '/export PATH=\/mingw64\/bin/d' ~/.bash_profile && echo 'export PATH=/mingw%NORM_PLATFORM%/bin:$PATH' >> ~/.bash_profile" || EXIT /B

  SET BUILDDIR=%BUILDDIR:\=/%
  SET BUILDDIR=/c!BUILDDIR:~2!
  SET INSTDIR=%INSTDIR:\=/%
  SET INSTDIR=/c!INSTDIR:~2!
  SET SRCDIR=%SRCDIR:\=/%
  SET SRCDIR=/c!SRCDIR:~2!

) ELSE IF "%PROFILE_CLASS%" == "CYGWIN" (

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

)
