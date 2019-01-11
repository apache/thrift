# Building Thrift using Docker for Windows

The build image is very large (just under 30GB) so plan accordingly.
Once Microsoft supports build tools in nano, it should get better.

Install Docker for Windows and switch to Windows container mode.

Pull from docker hub:

    PS C:\> docker pull thrift/thrift-build:msvc2017

or build in a docker for windows environment:

    PS C:\Thrift> docker build -t thrift/thrift-build:msvc2017 -f build\docker\msvc2017\Dockerfile build\

The following directories are used inside the container:

    C:\Build     the out-of-tree build directory
    C:\Install   the install target directory
    C:\Thrift    the source tree

You can override these as docker volumes if desired.

### Compiler

To build a portable windows thrift compiler (with a statically linked
runtime) and get it placed into C:\install:

    docker run -v C:\thrift:C:\thrift^
           -v C:\install:C:\install^
           --rm -t thrift/thrift-build:msvc2017^
           C:\thrift\build\docker\msvc2017\build-compiler.bat

The end result is a portable windows thrift compiler located at

    C:\Install\bin\thrift.exe

If you run it through the [Dependency Walker](http://www.dependencywalker.com/)
you will see it only depends on KERNEL32.DLL which means the runtime is statically
linked, so the executable is portable and self-contained.  This is how the
windows thrift compiler is built for each Apache Thrift release.

### Libraries

To build, test everything and get the C++ SDK placed into C:\install:

    docker run -v C:\thrift:C:\thrift^
           -v C:\install:C:\install^
           -m 4096 --rm -t thrift/thrift-build:msvc2017^
           C:\thrift\build\docker\msvc2017\build.bat