# Build compiler using CMake

Use the following steps to build using cmake

mkdir build
cd build
cmake ..
make

## Build on windows

In order to build on windows a few additional steps are necessary

1. Download winflexbison from http://sourceforge.net/projects/winflexbison/
2. Extract the winflex bison files to for e.g. C:\winflexbison
3. Make the CMake variables point to the correct binaries.
  * FLEX_EXECUTBALE = C:/winbuild/win_flex.exe
  * BISON_EXECUTBALE = C:/winbuild/win_bison.exe

