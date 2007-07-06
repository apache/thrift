#!/bin/sh

./cleanup.sh
autoscan
aclocal -I ../../lib/cpp/aclocal
libtoolize --automake
touch NEWS README AUTHORS ChangeLog
autoconf
automake -ac
