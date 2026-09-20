# Building Thrift using Docker for Windows

The build image is large, so plan accordingly.

Install Docker for Windows and switch to Windows container mode, then build the
image from this directory's Dockerfile. Note that the build context is `build\`,
not the directory the Dockerfile is in, because the image also copies scripts
from `build\appveyor\`:

    PS C:\Thrift> docker build -t thrift-build:msvc -f build\docker\msvc\Dockerfile build\

There is no image to pull: the `thrift/thrift-build` repository on Docker Hub
carries the Linux images only. The `MSVC Build` GitHub Actions workflow builds
this image itself and caches it in GHCR under a tag derived from the hash of the
Dockerfile and the scripts it copies, so a run only rebuilds it when one of
those changes. See [`.github/workflows/msvc.yml`](../../../.github/workflows/msvc.yml).

The following directories are used inside the container:

    C:\Build     the out-of-tree build directory
    C:\Install   the install target directory
    C:\Thrift    the source tree

You can override these as docker volumes if desired.

### Compiler

To build the windows thrift compiler and get it placed into C:\install:

    docker run -v C:\thrift:C:\thrift^
           -v C:\install:C:\install^
           --rm -t thrift-build:msvc^
           C:\thrift\build\docker\msvc\build-compiler.bat

The end result is at

    C:\Install\bin\thrift.exe

It is a single self-contained executable, but it is not statically linked: it
needs the Visual C++ redistributable, which the ASF does not ship. Beyond that
it must depend on nothing but Windows system DLLs - no Boost, OpenSSL, zlib or
libevent. `build\windows\check-compiler-imports.ps1` checks exactly that, and
the `compiler-windows` job in
[`.github/workflows/cmake.yml`](../../../.github/workflows/cmake.yml) runs it on
every push. See [`doc/ReleaseManagement.md`](../../../doc/ReleaseManagement.md)
for how the compiler is built for a release.

### Libraries

To build, test everything and get the C++ SDK placed into C:\install:

    docker run -v C:\thrift:C:\thrift^
           -v C:\install:C:\install^
           -m 4096 --rm -t thrift-build:msvc^
           C:\thrift\build\docker\msvc\build.bat
