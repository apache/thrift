# Build and run compiler tests using CMake

Tests use free Catch library (<https://github.com/philsquared/Catch>) for easy test creation and usage

## How to add your tests

* Open **CMakeLists.txt**
* Set **On** to call of **THRIFT_ADD_COMPILER** for your language
``` cmake 
THRIFT_ADD_COMPILER(netcore "Enable compiler for .NET Core" ON)
```
* Create folder with name specified in list of languages in **CMakeLists.txt**
* Create tests in folder for your language (with extensions like *.c* - cc, cpp, etc)
  * Don't forget to add include of catch.hpp in your test file
  ``` C
  #include "../catch/catch.hpp"
  ```
* If you need - add files manually to **thrift_compiler_tests_manual_SOURCES** in **CMakeLists.txt** similar to 
``` cmake
# you can add some files manually there 
set(thrift_compiler_tests_manual_SOURCES
    # tests file to avoid main in every test file
    ${CMAKE_CURRENT_SOURCE_DIR}/tests_main.cc
)
```
* Run **cmake** with arguments for your environment and compiler 
* Enjoy

## Build and run tests on Windows

### Prerequisites:
- Install CMake - <https://cmake.org/download/>
- Install winflexbison - <https://sourceforge.net/projects/winflexbison/>
- Install VS2017 Community Edition - <https://www.visualstudio.com/vs/whatsnew/> (ensure that you installed workload "Desktop Development with C++" for VS2017)

### Generation of VS2017 project with CMake, build and run on Windows
- Run commands in command line in current directory:
``` cmake
mkdir cmake-vs
cd cmake-vs
cmake -G "Visual Studio 15 2017" ..
```
- Build solution and use executable **thrift_compiler_tests.exe** in bin folder

Notes: to get details about executed tests use command **thrift_compiler_tests.exe -s**

