## macOS Setup

This document describes how to install the Apache Thrift compiler on macOS using Homebrew or build it from source.

### Install via Homebrew

The easiest and recommended way to install the Apache Thrift compiler on macOS is using [Homebrew](https://brew.sh):

```bash
brew install thrift
```

Verify the installation:

```bash
thrift -version
```

### Building from Source using Homebrew Dependencies

To build the Apache Thrift compiler from source, install the build tools using Homebrew:

```bash
brew install automake libtool pkg-config cmake bison flex
```

> **Note**: The default `bison` binary shipped with macOS is outdated (v2.3) and incompatible with Thrift's grammar files. Homebrew installs a newer version as keg-only, so ensure it is available in your `PATH` or passed explicitly.

The recipes below build only the compiler. To build the C++ library and its tests as well, add `boost libevent openssl` to the `brew install` line and drop the compiler-only options.

#### Building with CMake

```bash
cmake -B cmake-build \
  -DBISON_EXECUTABLE="$(brew --prefix bison)/bin/bison" \
  -DBUILD_LIBRARIES=OFF -DBUILD_TESTING=OFF -DBUILD_TUTORIALS=OFF
cmake --build cmake-build
sudo cmake --install cmake-build
```

#### Building with Autotools

```bash
export PATH="$(brew --prefix bison)/bin:$PATH"

./bootstrap.sh
./configure --disable-libs --disable-tests
make
sudo make install
```

### Manual Dependency Installation (Alternative)

If you prefer not to use Homebrew, you can build the dependencies manually:

#### Install Boost
Download the Boost library from [boost.org](https://www.boost.org), extract and compile with:

```bash
./bootstrap.sh
sudo ./b2 threading=multi address-model=64 variant=release stage install
```

#### Install libevent
Download [libevent](https://libevent.org), extract and compile with:

```bash
./configure --prefix=/usr/local 
make
sudo make install
```

#### Building Apache Thrift
Download the latest release of [Apache Thrift](https://thrift.apache.org/download), extract and compile with:

```bash
./configure --prefix=/usr/local --with-boost=/usr/local --with-libevent=/usr/local
make
sudo make install
```

## Additional reading

For more information on the requirements see: [Apache Thrift Requirements](/docs/install)

For more information on building and installing Thrift see: [Building from source](/docs/BuildingFromSource)

