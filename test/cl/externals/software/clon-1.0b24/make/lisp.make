### lisp.make --- Generic Makefile for Lisp source directories

## Copyright (C) 2010-2012, 2015 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

## This file is part of Clon

## Permission to use, copy, modify, and distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies.

## THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
## WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
## MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
## ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
## WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
## ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
## OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


### Commentary:

## Contents management by FCM version 0.1.

## Please use GNU Make with this makefile.


### Code:

# Should contain at least TOP_DIR and SUBDIRS settings
include local.inc

include $(TOP_DIR)/make/config.make
hack: all
include $(TOP_DIR)/make/include.make

all:

clean:
	$(MAKE) gen TARGET=clean
	-rm *~ $(EXTRA_CLEAN_FILES)

distclean: clean
	$(MAKE) gen TARGET=distclean
	-rm -fr $(BINLOC)-* $(EXTRA_DISTCLEAN_FILES)
	-rm -fr $(BINLOC_CACHE)/$(BINLOC)-*$(abspath $(PWD))

gen:
	@for i in $(SUBDIRS) ; do                 \
	   echo "making $(TARGET) in $${i} ..." ; \
	   ( cd $${i} && $(MAKE) $(TARGET) ) ;    \
	 done

.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: hack all clean distclean gen

### lisp.make ends here
