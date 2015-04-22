# Apache Thrift - CMake build

## Goal
Extend Apache Thrift's *make cross* approach to the build system.

Due to growing the field of operating system support, a proper executable
and library detection mechanism running on as much platforms as possible
becomes required. The other aspect to simplify the release process and
package generation process.

As nice side benefit of CMake is the generation of development environment
specific soultion files. => No solution files within source tree.


## Usage
just do this:

    mkdir build
    cmake ${THRIFT_SRC}

if you use a specific toolchain pass it to cmake, the same for options:

    cmake -DCMAKE_TOOLCHAIN_FILE=${THRIFT_SRC}/contrib/mingw32-toolchain.cmake ${THRIFT_SRC}
    cmake -DCMAKE_C_COMPILER=clang-3.5 -DCMAKE_CXX_COMPILER=clang++-3.5 ${THRIFT_SRC}
    cmake -DTHRIFT_COMPILER_HS=OFF ${THRIFT_SRC}
    cmake -DWITH_ZLIB=ON ${THRIFT_SRC}

and open the development environment you like with the solution or do this:

    make
    make check
    make cross
    make dist

to generate an installer and distribution package do this:

    cpack

## TODO
* git hash or tag based versioning depending on source state
* build tutorial
* build test
* with/without language lib/<lang>/
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

