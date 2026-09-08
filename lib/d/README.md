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

Breaking Changes
----------------

### 0.25.0

thrift.transport.http compares a header name in full instead of by prefix, so a
name that merely begins with one the transport knows no longer counts as that
name: "Content-Length-Foo" and "Content-LengthX" no longer set the content
length, and "Transfer-Encoding-Foo" no longer switches on chunked decoding. A
header name is the whole token before the colon (RFC 9110 5.1), and no
whitespace is allowed between the two, so "Content-Length : 5" is no longer read
as a content length either.

Content-Length is now read against RFC 9110 8.6's 1*DIGIT, and a value that is
not such a number is refused with a TTransportException. A sign, trailing text,
or a value that does not fit size_t was previously either accepted silently --
"5abc" gave 5, "0x10" gave 0 -- or left the transport as a std.conv exception
rather than a Thrift one.

thrift.transport.http reads at most maxBodySize bytes of message body, which
defaults to the 16 MB frame size limit the Thrift libraries use elsewhere. The
declared content length, and the number of chunks in a chunked body, are numbers
the peer chooses, and the body does not pass through the line buffer that
maxHttpBufferSize already bounds. A deployment that exchanges bodies larger than
that has to raise the limit on the transport.

thrift.internal.ssl.authorize() consults the certificate's common name only when
the certificate carries no DNS subjectAltName extension. Previously the common
name was consulted whenever no subjectAltName entry had returned ALLOW, so a
certificate whose subjectAltName named other hosts got a second chance from a
common name that matched -- TDefaultClientAccessManager.verify returns SKIP for a
name that does not match, which made "names other hosts" and "names no hosts" the
same state at the fallthrough. RFC 6125 6.4.4 and RFC 9525 6.3 make the
subjectAltName the identity once it is present. A certificate affected needs
reissuing with the host in its subjectAltName.

matchName(), which backs TDefaultClientAccessManager, no longer honours a
wildcard outside the leftmost label, per RFC 6125 6.4.3. Patterns such as
"thrift.*.*", "example.*.com" and "a.ev*.com" used to match and no longer do. A
wildcard in the leftmost label still covers at most one label, unchanged, so
"*.apache.org" matches "thrift.apache.org" but not "foo.bar.apache.org".
Certificates affected need reissuing with the host in their subjectAltName,
which is what every other TLS client already requires of them.
