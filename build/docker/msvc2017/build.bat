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

:: Install
cmake --install .
