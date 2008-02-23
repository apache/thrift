#!/bin/sh

./cleanup.sh

autoscan || exit 1
autoheader || exit 1
aclocal -I ./aclocal || exit 1

if libtoolize --version 1 >/dev/null 2>/dev/null; then
  libtoolize --automake || exit 1
elif glibtoolize --version 1 >/dev/null 2>/dev/null; then
  glibtoolize --automake || exit 1
fi

autoconf
automake -ac --add-missing --foreign || exit 1
