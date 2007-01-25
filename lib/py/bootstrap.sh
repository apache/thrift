#!/bin/sh

./cleanup.sh
aclocal
touch NEWS README AUTHORS ChangeLog
autoconf
automake -ac
