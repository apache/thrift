#!/bin/sh

make distclean >/dev/null 2>&1
rm -rf \
AUTHORS \
ChangeLog \
INSTALL \
Makefile \
Makefile.in \
Makefile.orig \
aclocal.m4 \
autom4te.cache \
autoscan.log \
config.guess \
config.h \
config.hin \
config.hin~ \
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
missing \
ylwrap \
if/gen-* \
compiler/cpp/Makefile.in \
if/Makefile.in \
lib/Makefile.in \
lib/cpp/Makefile.in \
lib/py/Makefile.in
