#!/bin/sh
set -e

./configure \
  --without-cpp \
  --without-c_glib \
  --without-java \
  --without-csharp \
  --without-python \
  --without-ruby \
  --without-haskell \
  --without-perl \
  --without-php \
  --without-erlang \
  --without-go \
  --without-d \
  --build=i686-pc-linux-gnu \
  --host=i586-mingw32msvc \
  CPPFLAGS='-DMINGW'

make

# Check two locations to be compatible with libtool 1.5.26 or 2.2.6b.
if test -f compiler/cpp/.libs/thrift.exe ; then
  i586-mingw32msvc-strip compiler/cpp/.libs/thrift.exe -o ./thrift.exe
else 
  i586-mingw32msvc-strip compiler/cpp/thrift.exe -o ./thrift.exe
fi
echo "Finished compiling with resulting exe"
ls -l ./thrift.exe
