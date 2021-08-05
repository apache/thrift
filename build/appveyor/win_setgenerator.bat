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
:: Detect the compiler edition we're building in and then
:: set the GENERATOR environment variable to one of:
::
::  Visual Studio 15 2017 [arch] = Generates Visual Studio 2017 project files.
::                                 Optional [arch] can be "Win64" or "ARM".
::  Visual Studio 14 2015 [arch] = Generates Visual Studio 2015 project files.
::                                 Optional [arch] can be "Win64" or "ARM".
::  Visual Studio 12 2013 [arch] = Generates Visual Studio 2013 project files.
::                                 Optional [arch] can be "Win64" or "ARM".
::  Visual Studio 11 2012 [arch] = Generates Visual Studio 2012 project files.
::                                 Optional [arch] can be "Win64" or "ARM".
::  Visual Studio 10 2010 [arch] = Generates Visual Studio 2010 project files.
::                                 Optional [arch] can be "Win64" or "IA64".
::  MinGW Makefiles              = Generates makefiles for MinGW
::  MSYS Makefiles               = Generates makefiles for MSYS
::  Unix Makefiles               = Generates makefiles for CYGWIN
::
:: Honors any existing GENERATOR environment variable
::   setting instead of overwriting it, to allow it
::   to be forced if needed.
::
:: Sets ERRORLEVEL to 0 if GENERATOR can be determined,
::                 to 1 if it cannot.
::

IF DEFINED GENERATOR (
  ECHO [warn ] using existing environment variable GENERATOR
) ELSE IF "%PROFILE_CLASS%" == "MINGW" (
  SET GENERATOR=MinGW Makefiles
) ELSE IF "%PROFILE_CLASS%" == "CYGWIN" (
  SET GENERATOR=Unix Makefiles
) ELSE IF "%PROFILE_CLASS%" == "MSYS" (
  SET GENERATOR=MSYS Makefiles
) ELSE IF "%PROFILE_CLASS%" == "MSVC" (
  IF /i "%PLATFORM%" == "x64" SET GENARCH= Win64
  IF "%PROFILE%" == "MSVC2015" (
    SET GENERATOR=Visual Studio 14 2015!GENARCH!
  ) ELSE IF "%PROFILE%" == "MSVC2017" (
    SET GENERATOR=Visual Studio 15 2017!GENARCH!
  ) ELSE IF "%PROFILE%" == "MSVC2019" (
    SET GENERATOR=Visual Studio 16 2019!GENARCH!
  ) ELSE (
    ECHO [error] unable to determine the CMake generator to use from MSVC profile %PROFILE%
    EXIT /B 1
  )
) ELSE (
  ECHO [error] unable to determine the CMake generator to use from profile %PROFILE%
  EXIT /B 1
)

ECHO [info ] using CMake generator        %GENERATOR%
