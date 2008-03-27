#!/bin/sh

topsrcdir="`dirname $0`"
cd "$topsrcdir"

make -k clean >/dev/null 2>&1
make -k distclean >/dev/null 2>&1
find . -name Makefile.in -exec rm -f {} \;
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
config.status.lineno \
config.sub \
configure \
configure.lineno \
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
test/gen-*
