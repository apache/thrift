#!/bin/sh

subdirs="compiler lib/cpp lib/php"

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

