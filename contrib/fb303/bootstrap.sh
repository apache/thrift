#!/bin/sh


# To be safe include -I flag
aclocal
automake -a
autoconf
./configure --config-cache $*
