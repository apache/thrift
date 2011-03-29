#!/bin/sh
set -e

./configure \
  --without-cpp \
  --without-java \
  --without-csharp \
  --without-python \
  --without-ruby \
  --without-haskell \
  --without-perl \
  --without-php \
  --without-erlang \
  --build=i686-pc-linux-gnu \
  --host=i586-mingw32msvc \
  CPPFLAGS='-DMINGW'

make

# Check two locations to be compatible with libtool 1.5.26 or 2.2.6b.
if test -f compiler/cpp/.libs/thrift.exe
  then cp compiler/cpp/.libs/thrift.exe ./thrift.exe
  else cp compiler/cpp/thrift.exe ./thrift.exe
fi
i586-mingw32msvc-strip ./thrift.exe
echo
ls -l ./thrift.exe
