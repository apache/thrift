#!/bin/sh

subdirs="compiler/py lib/cpp lib/php"

rm -rf \
AUTHORS \
COPYING \
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

echo "SUBDIRS = ${subdirs}" > Makefile.am

aclocal
touch NEWS README AUTHORS ChangeLog
autoconf
automake -ac

for subdir in ${subdirs}; do 
    if [ -x "${subdir}/bootstrap.sh" ]; then 
	cwd="`pwd`"
	cd ${subdir}
	./bootstrap.sh
	cd ${cwd}
    fi
done

