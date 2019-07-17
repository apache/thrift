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
SETLOCAL EnableDelayedExpansion

CD build\appveyor                           || EXIT /B
CALL cl_banner_build.bat                    || EXIT /B
CALL cl_setenv.bat                          || EXIT /B
MKDIR "%BUILDDIR%"                          || EXIT /B
CD "%BUILDDIR%"                             || EXIT /B

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

@ECHO ON
  cmake "%SRCDIR%" ^
    -G"%GENERATOR%" ^
	-DBISON_EXECUTABLE=C:\ProgramData\chocolatey\lib\winflexbison3\tools\win_bison.exe ^
    -DBOOST_ROOT="%BOOST_ROOT%" ^
    -DBOOST_LIBRARYDIR="%BOOST_LIBRARYDIR%" ^
    -DBUILD_SHARED_LIBS="%BUILD_SHARED_LIBS%" ^
    -DCMAKE_BUILD_TYPE="%CONFIGURATION%" ^
    -DCMAKE_INSTALL_PREFIX="%INSTDIR%" ^
	-DFLEX_EXECUTABLE=C:\ProgramData\chocolatey\lib\winflexbison3\tools\win_flex.exe ^
    -DLIBEVENT_ROOT="%WIN3P%\libevent-%LIBEVENT_VERSION%-stable" ^
    -DOPENSSL_ROOT_DIR="%OPENSSL_ROOT%" ^
    -DOPENSSL_USE_STATIC_LIBS=OFF ^
    -DZLIB_LIBRARY="%WIN3P%\zlib-inst\lib\zlib%ZLIB_LIB_SUFFIX%.lib" ^
    -DZLIB_ROOT="%WIN3P%\zlib-inst" ^
    -DWITH_PYTHON=%WITH_PYTHON%             || EXIT /B
@ECHO OFF

cmake --build . ^
  --config "%CONFIGURATION%" ^
  --target INSTALL                          || EXIT /B
