#!/bin/sh

subdirs="compiler/cpp lib/cpp lib/py if"

./cleanup.sh
echo "SUBDIRS = ${subdirs}" > Makefile.am

aclocal
touch NEWS README AUTHORS ChangeLog
autoconf
automake -ac

for subdir in ${subdirs}; do 
    if [ -x "${subdir}/bootstrap.sh" ]; then 
	cwd="`pwd`"
	cd "${subdir}"
	./bootstrap.sh
	cd "${cwd}"
    fi
done

