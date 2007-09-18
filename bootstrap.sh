#!/bin/sh

subdirs="compiler/cpp lib/cpp lib/py if"

./cleanup.sh

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

