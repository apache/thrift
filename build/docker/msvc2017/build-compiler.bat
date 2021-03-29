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
  -DBOOST_ROOT=C:\Libraries\boost_1_69_0^
  -DBOOST_LIBRARYDIR=C:\Libraries\boost_1_69_0\lib64-msvc-14.1^
  -DBUILD_LIBRARIES=OFF^
  -DCMAKE_BUILD_TYPE=Release^
  -DCMAKE_INSTALL_PREFIX=C:\install^
  -DWITH_MT=ON^
  c:\thrift || EXIT /B

:: Build
cmake --build . --target thrift-compiler --config Release || EXIT /B

:: Test
cmake --build . --target check || EXIT /B

:: Install
cmake --build . --target install