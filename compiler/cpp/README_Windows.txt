Building the Thrift IDL compiler in Windows
-------------------------------------------

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

Download inttypes.h from the interwebs and place it in an include path
location (e.g. thrift/compiler/cpp/src).

Build the compiler in Visual Studio.
