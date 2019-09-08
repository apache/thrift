Thrift D Software Library
=========================

License
-------

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

Testing
-------

D support in Thrift is covered by two sets of tests: first,
the unit test blocks contained in the D source files, and
second, the more extensive testing applications in the test/
subdirectory, which also make use of the Thrift compiler.
Both are built when running "make check", but only the
unit tests are immediately run, however – the separate test
cases typically run longer or require manual intervention.
It might also be prudent to run the independent tests,
which typically consist of a server and a client part,
against the other language implementations.

To build the unit tests on Windows, the easiest way might
be to manually create a file containing an empty main() and
invoke the compiler by running the following in the src/
directory (PowerShell syntax):

dmd -ofunittest -unittest -w $(dir -r -filter '*.d' -name)

Async and SSL
-------------
Using SSL with async is experimental (always has been) and
the unit test "async_test --ssl" hangs.  Use at your own
risk.
