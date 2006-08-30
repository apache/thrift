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
config.log \
config.status \
config.sub \
configure \
configure.scan \
depcomp \
.deps \
install-sh \
.libs \
libtool \
ltmain.sh \
Makefile.in \
missing

aclocal
touch NEWS README AUTHORS ChangeLog
autoconf
automake -ac
