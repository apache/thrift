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

SET CMAKEARGS=^
  -G'%GENERATOR%' ^
  -DCMAKE_BUILD_TYPE=%CONFIGURATION% ^
  -DCMAKE_INSTALL_PREFIX=%INSTDIR% ^
  -DCMAKE_CXX_FLAGS="-D_GNU_SOURCE" ^
  -DWITH_JAVA=OFF ^
  -DWITH_PYTHON=OFF

:: -DCMAKE_CXX_EXTENSIONS=ON ^
:: -DCMAKE_CXX_STANDARD=11 ^

@ECHO ON
%BASH% -lc "export PATH=/cygdrive/c/Windows/system32:/cygdrive/c/Windows:/cygdrive/c/Windows/System32/Wbem:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0:/usr/bin && mkdir -p %BUILDDIR% && cd %BUILDDIR% && cmake %SRCDIR% %CMAKEARGS% && cmake --build . --config %CONFIGURATION% --target install -j `nproc`" || EXIT /B
@ECHO OFF
