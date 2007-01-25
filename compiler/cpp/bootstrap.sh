#!/bin/sh

./cleanup.sh
autoscan
aclocal
libtoolize --automake
touch NEWS README AUTHORS ChangeLog
autoconf
automake -ac
