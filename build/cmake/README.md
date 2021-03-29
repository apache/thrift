# Apache Thrift - CMake Build

## Goal

Extend Apache Thrift's *make cross* approach to the build system.

Due to growing the field of operating system support, a proper executable
and library detection mechanism running on as much platforms as possible
becomes required. The other aspect to simplify the release process and
package generation process.

As nice side benefit of CMake is the generation of development environment
specific soultion files. => No solution files within source tree.

## Prerequisites

These are language-specific, however for C++ you must provide:

- Boost
- OpenSSL

You may optionally provide:

- libevent
- zlib

## Usage

To use CMake you first create an out-of-tree build directory, then use
CMake to generate a build framework, then build:

    mkdir /tmp/build
    cd /tmp/build
    cmake /location/to/thrift

if you use a specific toolchain pass it to cmake, the same for options:

    cmake -DCMAKE_TOOLCHAIN_FILE=../build/cmake/mingw32-toolchain.cmake ..
    cmake -DCMAKE_C_COMPILER=clang-3.5 -DCMAKE_CXX_COMPILER=clang++-3.5 ..
    cmake -DTHRIFT_COMPILER_HS=OFF ..
    cmake -DWITH_ZLIB=ON ..

and open the development environment you like with the solution or do this:

    make
    make check
    make cross
    make dist

or on Windows, the following will produce a solution file you can use
inside Visual Studio:

    cmake -G "Visual Studio 15 2017 Win64" \
      -DBOOST_ROOT=C:/3rdparty/boost_1_69_0 \
      -DBOOST_LIBRARYDIR=C:/3rdparty/boost_1_69_0/lib64-msvc-14.1^
      -DZLIB_ROOT=C:/3rdparty/zlib-1.2.11

<!--
To generate an installer and distribution package do this:

    cpack
-->

## TODO

* git hash or tag based versioning depending on source state
* build tutorial
* build test
* enable/disable
* make cross
* make dist (create an alias to make package_source)
* make doc
* cpack (C++ and make dist only ?)
  * thrift-compiler
  * libthrift
  * tutorial
  * test
* merge into /README.md