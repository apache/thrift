#!/bin/sh
autoscan
autoheader
aclocal
libtoolize --force
touch NEWS README AUTHORS ChangeLog
autoconf
automake -ac
