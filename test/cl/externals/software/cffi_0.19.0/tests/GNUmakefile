# -*- Mode: Makefile; tab-width: 3; indent-tabs-mode: t -*-
#
# Makefile --- Make targets for various tasks.
#
# Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

OSTYPE = $(shell uname)
ARCH   = $(shell uname -m)

CC             := gcc
CFLAGS         := -Wall -std=c99 -pedantic
SHLIB_CFLAGS   := -shared
SHLIB_EXT      := .so

ifneq ($(if $(filter Linux %BSD,$(OSTYPE)),OK), OK)
ifeq ($(OSTYPE), Darwin)
SHLIB_CFLAGS   := -dynamiclib
SHLIB_EXT      := .dylib
ifeq ($(shell sysctl -n hw.optional.x86_64), 1)
ARCH           := x86_64
CFLAGS_64      := -m64
endif
else
ifeq ($(OSTYPE), SunOS)
CFLAGS         := -m64 -fPIC -c -Wall -std=c99 -pedantic
else
# Let's assume this is win32
SHLIB_EXT      := .dll
endif
endif
endif

ifneq ($(ARCH), x86_64)
CFLAGS += -lm
endif

ifeq ($(ARCH), x86_64)
CFLAGS_64 += -fPIC
endif

# Are all G5s ppc970s?
ifeq ($(ARCH), ppc970)
CFLAGS_64 += -m64
endif

SHLIBS = libtest$(SHLIB_EXT) libtest2$(SHLIB_EXT) libfsbv$(SHLIB_EXT)

ifeq ($(ARCH), x86_64)
SHLIBS += libtest32$(SHLIB_EXT) libtest2_32$(SHLIB_EXT) libfsbv_32$(SHLIB_EXT)
endif

shlibs: $(SHLIBS)

libtest$(SHLIB_EXT): libtest.c
	$(CC) -o $@ $(SHLIB_CFLAGS) $(CFLAGS) $(CFLAGS_64) $<

libtest2$(SHLIB_EXT): libtest2.c
	$(CC) -o $@ $(SHLIB_CFLAGS) $(CFLAGS) $(CFLAGS_64) $<

libfsbv$(SHLIB_EXT): libfsbv.c
	$(CC) -o $@ $(SHLIB_CFLAGS) $(CFLAGS) $(CFLAGS_64) $<

ifeq ($(ARCH), x86_64)
libtest32$(SHLIB_EXT): libtest.c
	-$(CC) -m32 -o $@ $(SHLIB_CFLAGS) $(CFLAGS) $<

libtest2_32$(SHLIB_EXT): libtest2.c
	-$(CC) -m32 -o $@ $(SHLIB_CFLAGS) $(CFLAGS) $<

libfsbv_32$(SHLIB_EXT): libfsbv.c
	-$(CC) -m32 -o $@ $(SHLIB_CFLAGS) $(CFLAGS) $<
endif

clean:
	rm -f *.so *.dylib *.dll *.bundle

# vim: ft=make ts=3 noet
