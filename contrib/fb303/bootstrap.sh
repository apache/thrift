#!/bin/sh


# To be safe include -I flag
aclocal -I ./aclocal
automake -a
autoconf
./configure --config-cache $*
