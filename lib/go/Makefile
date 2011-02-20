# Copyright 2009 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# After editing the DIRS= list or adding imports to any Go files
# in any of those directories, run:
#
#	./deps.bash
#
# to rebuild the dependency information in Make.deps.


include $(GOROOT)/src/Make.inc

all: Make.deps install

DIRS=\
     thrift/\

TEST=\
	$(filter-out $(NOTEST),$(DIRS))


clean.dirs: $(addsuffix .clean, $(DIRS))
install.dirs: $(addsuffix .install, $(DIRS))
nuke.dirs: $(addsuffix .nuke, $(DIRS))
test.dirs: $(addsuffix .test, $(DIRS))
check.dirs: $(addsuffix .check, $(DIRS))

%.clean:
	+cd $* && gomake clean

%.install:
	+cd $* && gomake install

%.nuke:
	+cd $* && gomake nuke

%.test:
	+cd $* && gomake test

%.check:
	+cd $* && gomake check

clean: clean.dirs

install: install.dirs

test:	test.dirs

check:	check.dirs

#nuke: nuke.dirs
#	rm -rf "$(GOROOT)"/pkg/thrift.*

echo-dirs:
	@echo $(DIRS)

Make.deps:
	./deps.bash

deps:
	./deps.bash

