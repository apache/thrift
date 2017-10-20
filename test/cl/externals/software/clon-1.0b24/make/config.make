### config.make --- Configuration part

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

## Installation prefix. This is used for installing Clon as follows:
# - $(PREFIX)/share/doc/clon/ for the PDF documentation
# - $(PREFIX)/share/info/ for the info documentation
# If any of these are unsatisfactory, you will need to edit the Makefiles, or
# do the installation by hand.
PREFIX := /usr/local

## Configuration:
# Uncomment the following line in order to activate restricted mode (see
# section A.1 of the user manual):
# RESTRICTED := t

## C compiler configuration for sb-grovel and cffi-grovel.
CC := gcc

## Preferred Common Lisp implementation.
# Choices are SBCL, CMUCL, CCL, ECL, CLISP, ABCL, ACL (Allegro) and LW
# (LispWorks).
LISP := SBCL

## Global Common Lisp binary cache location.
BINLOC_CACHE := ${HOME}/.cache/common-lisp

## Implementation paths. Note that regardless of the above setting, SBCL is
# required in order to generate the reference manual, so you must set
# SBCL_PATH properly. If you don't have SBCL installed at all, set it to
# nothing.
SBCL_PATH  := CC=$(CC) sbcl
CMUCL_PATH := lisp
CCL_PATH   := ccl
ECL_PATH   := ecl
# -q is needed to remove 'bye' for version.cl to work properly.
CLISP_PATH := CC=$(CC) clisp -q
ACL_PATH   := alisp
# Note: if you want to dump the demo examples, you should use a console image
# without multiprocessing here. See section 12.3.5 of the LispWorks user guide
# and reference manual.
LW_PATH    := lispworks

# For ABCL, we need something slightly different.
ABCL_JAR   := /usr/local/src/common-lisp/abcl/dist/abcl.jar
JAVA       := java
JAVAC      := javac
JAR        := jar
SED        := sed
ABCL_PATH  := abcl


## Programs for generating the documentation:
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
DVIPS    = dvips
CONVERT  = convert


### config.make ends here
