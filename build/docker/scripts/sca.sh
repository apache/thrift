#!/bin/sh
set -ev

#
# C/C++ static code analysis with cppcheck
# add --error-exitcode=1 to --enable=all as soon as everything is fixed
#
# Python code style check with flake8
#
# search for TODO etc within source tree
# some statistics about the code base
# some info about the build machine

# Compiler cppcheck (All)
cppcheck --force --quiet --inline-suppr --enable=all -j2 compiler/cpp/src

# C++ cppcheck (All)
cppcheck --force --quiet --inline-suppr --enable=all -j2 lib/cpp/src lib/cpp/test test/cpp tutorial/cpp

# C Glib cppcheck (All)
cppcheck --force --quiet --inline-suppr --enable=all -j2 lib/c_glib/src lib/c_glib/test test/c_glib/src tutorial/c_glib

# Silent error checks
cppcheck --force --quiet --inline-suppr --error-exitcode=1 -j2 compiler/cpp/src
cppcheck --force --quiet --inline-suppr --error-exitcode=1 -j2 lib/cpp/src lib/cpp/test test/cpp tutorial/cpp
cppcheck --force --quiet --inline-suppr --error-exitcode=1 -j2 lib/c_glib/src lib/c_glib/test test/c_glib/src tutorial/c_glib

# Python code style
flake8 --ignore=E501 lib/py
flake8 tutorial/py
flake8 --ignore=E501 test/py
flake8 test/py.twisted
flake8 test/py.tornado
flake8 --ignore=E501 test/test.py
flake8 --ignore=E501 test/crossrunner
flake8 test/features

# TODO etc
echo FIXMEs: `grep -r FIXME * | wc -l`
echo  HACKs: `grep -r HACK * | wc -l`
echo  TODOs: `grep -r TODO * | wc -l`

# LoC
sloccount .

# System Info
dpkg -l
uname -a
