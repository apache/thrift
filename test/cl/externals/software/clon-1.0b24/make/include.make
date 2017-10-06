### include.make --- Inclusion part

## Copyright (C) 2010-2012, 2015 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

## This file is part of Clon.

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


### Code:

PROJECT   := clon
PACKAGE   := net.didierverna.$(PROJECT)
ASDF_FILE := $(PACKAGE).asd

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/www/software/lisp/$(PROJECT)


SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_LOAD   := --load
SBCL_EVAL   := --eval
SBCL_DUMP   := --script

CMUCL_CACHE  := cmu
CMUCL_BINLOC := cmu
CMUCL_LOAD   := -load
CMUCL_EVAL   := -eval
CMUCL_DUMP   := -noinit -nositeinit $(CMUCL_LOAD)

CCL_CACHE  := ccl
CCL_BINLOC := openmcl
CCL_LOAD   := --load
CCL_EVAL   := --eval
CCL_DUMP   := --no-init $(CCL_LOAD)

ECL_CACHE  := ecl
ECL_BINLOC := ecl
ECL_LOAD   := -load
ECL_EVAL   := -eval
ECL_DUMP   := -norc $(ECL_LOAD)

CLISP_CACHE  := clisp
CLISP_BINLOC := clisp
CLISP_LOAD   := -i
CLISP_DUMP   := -norc $(CLISP_LOAD)

ABCL_CACHE  := abcl
ABCL_BINLOC := abcl
ABCL_LOAD   := --load
ABCL_EVAL   := --eval
#### NOTE: multiple usage of the eval option to avoid a funcall/intern mess.
ABCL_DUMP   := --batch							\
	       $(ABCL_EVAL) '(require "asdf")'				\
	       $(ABCL_EVAL) '(asdf:load-system :$(PACKAGE).setup)'	\
	       $(ABCL_EVAL) '($(PACKAGE).setup:configure :dump t)'	\
	       $(ABCL_LOAD)

ACL_CACHE  := acl
ACL_BINLOC := acl
ACL_LOAD   := -L
ACL_EVAL   := -e
ACL_DUMP   := -qq $(ACL_LOAD)

LW_CACHE  := lw
LW_BINLOC := lw
LW_LOAD   := -load
LW_EVAL   := -eval
LW_DUMP   := -init - -siteinit - $(LW_LOAD)

BINLOC := $($(LISP)_BINLOC)

ifeq ($(RESTRICTED),t)
#### NOTE: multiple usage of the eval option to avoid a funcall/intern mess.
CONFIG_1 := '(require "asdf")'
CONFIG_2 := '(asdf:load-system :$(PACKAGE).setup)'
CONFIG_3 := '($(PACKAGE).setup:configure :restricted t)'
  ifeq ($(LISP),CLISP)
EVAL_CONFIG := $($(LISP)_LOAD) $(TOP_DIR)/.clisp.cnf
  else
EVAL_CONFIG := $($(LISP)_EVAL) $(CONFIG_1)	\
	       $($(LISP)_EVAL) $(CONFIG_2)	\
	       $($(LISP)_EVAL) $(CONFIG_3)
  endif
else
CONFIG_1 :=
CONFIG_2 :=
CONFIG_3 :=
EVAL_CONFIG :=
endif

# This is a gross hack for compensating the lack of --eval option to clisp.
# This rule doesn't actually create any clisp.make file, but a clisp
# configuration file that will be loaded with -i, and which is redone every
# time make is called (several times actually). This is not a clean solution
# but it works. Every Makefile that needs to run $(LISP) needs to include
# clisp.make in order for this to work.
$(TOP_DIR)/make/clisp.make:
	echo $(CONFIG_1) >  $(TOP_DIR)/.clisp.cnf
	echo $(CONFIG_2) >> $(TOP_DIR)/.clisp.cnf
	echo $(CONFIG_3) >> $(TOP_DIR)/.clisp.cnf

# The rule below duplicates what the one above does, but it's needed for
# makefiles that include both version.inc and clisp.make. This is necessary
# because Make wants to redo things in the wrong order and hence would call
# clisp -i .clisp.cnf without this file having been created first.
$(TOP_DIR)/make/version.make: \
  $(TOP_DIR)/make/version.cl $(TOP_DIR)/setup/setup.lisp
ifeq ($(LISP),CLISP)
	echo $(CONFIG_1) >  $(TOP_DIR)/.clisp.cnf
	echo $(CONFIG_2) >> $(TOP_DIR)/.clisp.cnf
	echo $(CONFIG_3) >> $(TOP_DIR)/.clisp.cnf
endif
	$($(LISP)_PATH) $(EVAL_CONFIG)			\
	  $($(LISP)_LOAD) $(TOP_DIR)/make/version.cl	\
	  | tail -2 > $@



### include.make ends here
