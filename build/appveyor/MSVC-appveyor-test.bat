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
CD build\appveyor              || EXIT /B
CALL cl_banner_test.bat        || EXIT /B
CALL cl_setenv.bat             || EXIT /B
CD "%BUILDDIR%"                || EXIT /B

:: Add directories to the path to find DLLs of third party libraries so tests run
SET PATH=%BOOST_LIBRARYDIR%;%OPENSSL_ROOT%\bin;%WIN3P%\zlib-inst\bin;%PATH%

:: The stress test is long running on appveyor (2+ minutes)
:: and not terribly useful with one core, so we disable it
SET DISABLED_TESTS=StressTestNonBlocking

ctest -C %CONFIGURATION% --timeout 600 -VV -E "(%DISABLED_TESTS%)" || EXIT /B
