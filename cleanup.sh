#!/bin/sh

subdirs="compiler/cpp lib/cpp lib/php lib/py"

rm -rf \
AUTHORS \
ChangeLog \
INSTALL \
Makefile.am \
Makefile \
Makefile.in \
Makefile.orig \
NEWS \
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
missing

for subdir in ${subdirs}; do 
    if [ -x "${subdir}/cleanup.sh" ]; then 
	cwd="`pwd`"
	cd ${subdir}
	./cleanup.sh
	cd ${cwd}
    fi
done
