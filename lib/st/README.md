Thrift SmallTalk Software Library

Last updated Nov 2007

License
=======

Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.

Contains some contributions under the Thrift Software License.
Please see doc/old-thrift-license.txt in the Thrift distribution for
details.

Library
=======

To get started, just file in thrift.st with Squeak, run thrift -st
on the tutorial .thrift files (and file in the resulting code), and
then:

calc := CalculatorClient binaryOnHost: 'localhost' port: '9090'
calc addNum1: 10 num2: 15

Tested in Squeak 3.7, but should work fine with anything later.

Note that GNU Smalltalk cannot read thrift.st: the file is in the
Squeak/Pharo chunk format, which gst does not parse.

Tests
=====

The suites in test/ are SUnit test cases. lib/st is not part of the
autotools build - it is not in configure.ac and not in a SUBDIRS - so
there is no "make check" for it. test/run-tests.sh is the equivalent:
it files thrift.st and the suites into a Pharo image, runs them, and
fails if any test fails or if a suite registers a different number of
tests than expected.

  # Pharo, once:
  mkdir -p /tmp/pharo && cd /tmp/pharo
  curl -sSL https://get.pharo.org/64/130+vm | bash

  # then, from a Thrift checkout with a built compiler:
  lib/st/test/run-tests.sh --thrift compiler/cpp/thrift --pharo /tmp/pharo

Pharo prints a failed test and still exits 0, so the script parses the
run and pass counts rather than trusting the exit status.

The same script runs in the lib-st CI job.
