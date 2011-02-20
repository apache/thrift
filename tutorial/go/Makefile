# Copyright 2009 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

include $(GOROOT)/src/Make.inc

all: Make.deps install

TARG=thriftlib/tutorialcalculator

DIRS=\
  src/\

GOFILES=\


clean.dirs: $(addsuffix .clean, $(DIRS))
install.dirs: $(addsuffix .install, $(DIRS))
test.dirs: $(addsuffix .test, $(DIRS))


%.clean:
	+cd $* && $(MAKE) clean

%.install:
	+cd $* && $(MAKE) install

%.test:
	+cd $* && $(MAKE) test



Make.deps:
	./deps.bash

deps:
	./deps.bash

clean: clean.dirs

install: install.dirs

test:	test.dirs

