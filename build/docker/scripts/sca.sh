#!/bin/bash

#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

set -ev

#
# Generate thrift files so the static code analysis includes an analysis
# of the files the thrift compiler spits out.  If running interactively
# set the NOBUILD environment variable to skip the boot/config/make phase.
#

if [[ -z "$NOBUILD" ]]; then
  ./bootstrap.sh
  ./configure --enable-tutorial=no
  make -j3 precross
fi

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
# See THRIFT-4371 : flex generated code triggers "possible null pointer dereference" in yy_init_buffer
cppcheck --force --quiet --inline-suppr --suppress="*:thrift/thriftl.cc" --error-exitcode=1 -j2 compiler/cpp/src
cppcheck --force --quiet --inline-suppr --error-exitcode=1 -j2 lib/cpp/src lib/cpp/test test/cpp tutorial/cpp
cppcheck --force --quiet --inline-suppr --error-exitcode=1 -j2 lib/c_glib/src lib/c_glib/test test/c_glib/src tutorial/c_glib

# Python code style
flake8

# PHP code style
composer install --quiet
./vendor/bin/phpcs

# TODO etc
echo FIXMEs: `grep -r FIXME * | wc -l`
echo  HACKs: `grep -r HACK * | wc -l`
echo  TODOs: `grep -r TODO * | wc -l`

# LoC
sloccount .

# System Info
dpkg -l
uname -a
