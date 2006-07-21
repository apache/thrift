#!/bin/sh

rm -rf \
AUTHORS \
COPYING \
ChangeLog \
INSTALL \
Makefile \
Makefile.in \
Makefile.orig \
NEWS \
README \
aclocal.m4 \
autom4te.cache \
autoscan.log \
config.guess \
config.h \
config.hin \
config.hin \
config.log \
config.status \
config.sub \
configure \
configure.scan \
depcomp \
install-sh \
libtool \
ltmain.sh \
missing \
stamp-h1


autoscan
autoheader
aclocal
libtoolize --force
touch NEWS README AUTHORS ChangeLog
autoconf
automake -ac
