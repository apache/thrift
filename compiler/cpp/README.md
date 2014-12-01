# Build compiler using CMake

Use the following steps to build using cmake:

    mkdir build
    cd build
    cmake ..
    make


### Create an eclipse project

    mkdir build_ec && cd build_ec
    cmake -G "Eclipse CDT4 - Unix Makefiles" ..
    make

Now open the folder build_ec using eclipse.


### Cross compile using mingw32 and generate a Windows Installer with CPack

    mkdir build_mingw32 && cd build_mingw32
    cmake -DCMAKE_TOOLCHAIN_FILE=../../../contrib/mingw32-toolchain.cmake ..
    cpack

## Build on windows

In order to build on windows a few additional steps are necessary:

1. Download winflexbison from http://sourceforge.net/projects/winflexbison/
2. Extract the winflex bison files to for e.g. C:\winflexbison
3. Make the CMake variables point to the correct binaries.
  * FLEX_EXECUTBALE = C:/winbuild/win_flex.exe
  * BISON_EXECUTBALE = C:/winbuild/win_bison.exe


### Create a Visual Studio project

    mkdir build_vs && cd build_vs
    cmake -G "Visual Studio 12" ..

Now open the folder build_vs using Visual Studio 2013.




# Building the Thrift IDL compiler in Windows

If you don't want to use CMake you can use the already available Visual Studio
2010 solution.
The Visual Studio project contains pre-build commands to generate the
thriftl.cc, thrifty.cc and thrifty.hh files which are necessary to build
the compiler. These depend on bison, flex and their dependencies to
work properly. If this doesn't work on a system, try these manual
pre-build steps.

Open compiler.sln and remove the Pre-build commands under the project's
 Properties -> Build Events -> Pre-Build Events.

Download flex & bison from http://jaisantonyk.wordpress.com/2008/03/16/lex-and-yaccbison-in-windows/
Download bison.simple in addition to bison.exe . This build of bison is easier to use
than the one on sourceforge which has a myriad of dependencies.
Place these binaries somewhere in the path.

From a command prompt:
> cd thrift/compiler/cpp
> flex -osrc\thriftl.cc src\thriftl.ll
In the generated thriftl.cc, comment out #include <unistd.h>

Place a copy of bison.simple in thrift/compiler/cpp
> bison -y -o "src/thrifty.cc" --defines src/thrifty.yy
> move src\thrifty.cc.hh  src\thrifty.hh

Bison might generate the yacc header file "thrifty.cc.h" with just one h ".h" extension; in this case you'll have to rename to "thrifty.h".

> move src\windows\version.h.in src\windows\version.h

Download inttypes.h from the interwebs and place it in an include path
location (e.g. thrift/compiler/cpp/src).

Build the compiler in Visual Studio.
