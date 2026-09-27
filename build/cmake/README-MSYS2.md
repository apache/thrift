<!---
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Building thrift on Windows (MSYS2 UCRT64)

Thrift uses cmake to make it easier to build the project on multiple platforms, however to build a fully functional and production ready thrift on Windows requires a number of third party libraries to be obtained.  Once third party libraries are ready, the right combination of options must be passed to cmake in order to generate the correct environment.

## MSYS2

Download and fully upgrade msys2 following the instructions at:

    https://www.msys2.org/

Run all of the following steps in a terminal for the UCRT64 environment of MSYS2, which puts `/ucrt64/bin` first on the PATH. The MSYS2 installer opens one when it finishes, and https://www.msys2.org/docs/environments/ describes the environments. MSYS2 is phasing out the MINGW64 environment that earlier versions of these instructions used.

Install the necessary toolchain items for C++:

    $ pacman --needed -S bison flex make mingw-w64-ucrt-x86_64-openssl \
                mingw-w64-ucrt-x86_64-boost mingw-w64-ucrt-x86_64-cmake \
                mingw-w64-ucrt-x86_64-libevent mingw-w64-ucrt-x86_64-toolchain \
                mingw-w64-ucrt-x86_64-zlib

Use cmake to create a MinGW makefile, out of tree (assumes you are in the top level of the thrift source tree):

    mkdir ../thrift-build
    cd ../thrift-build
    cmake -G"MinGW Makefiles" -DCMAKE_MAKE_PROGRAM=/ucrt64/bin/mingw32-make \
       -DCMAKE_C_COMPILER=/ucrt64/bin/gcc.exe \
       -DCMAKE_CXX_COMPILER=/ucrt64/bin/g++.exe \
       -DOPENSSL_ROOT_DIR=/ucrt64 \
       -DWITH_JAVA=OFF -DWITH_PYTHON=OFF \
       ../thrift

This builds the compiler and the C++ library with OpenSSL, libevent and zlib. The libraries are DLLs, which is the default on Windows; `-DBUILD_SHARED_LIBS=OFF` builds static libraries instead.

Build thrift (inside thrift-build):

    cmake --build .

Run the tests (inside thrift-build):

    ctest

## Tested With

The AppVeyor MINGW job builds thrift in the UCRT64 environment and runs its tests on every commit, with the packages and compilers shown here; see `build/appveyor/MINGW-appveyor-full.bat`.
