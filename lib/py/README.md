Thrift Python Software Library

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

Using Thrift with Python
========================

Thrift is provided as a set of Python packages. The top level package is
thrift, and there are subpackages for the protocol, transport, and server
code. Each package contains modules using standard Thrift naming conventions
(i.e. TProtocol, TTransport) and implementations in corresponding modules
(i.e. TSocket).  There is also a subpackage reflection, which contains
the generated code for the reflection structures.

The Python libraries can be installed manually using the provided setup.py
file, or automatically using the install hook provided via autoconf/automake.
To use the latter, become superuser and do make install.

Breaking Changes
================

0.25.0
------

TTornadoStreamTransport and TTornadoServer now refuse a frame whose declared size
is negative or larger than max_frame_size, before reading it. The maximum defaults
to DEFAULT_MAX_FRAME_SIZE (16384000 bytes), the one TFramedTransport and
THeaderTransport already apply, and both classes take max_frame_size as a keyword
argument. A service that exchanges larger frames over Tornado needs it raised where
those frames are read: on TTornadoServer for requests, and on the client's
TTornadoStreamTransport for responses.

TSSLSocket now sets check_hostname on the SSL contexts it builds whenever it
verifies the peer, so OpenSSL matches the server name against the certificate
during the handshake, whatever protocol the client asked for.

Clients that leave ssl_version alone are unaffected, because PROTOCOL_TLS_CLIENT
already switched it on. A client that passes an explicit ssl_version got a context
with host-name matching off, and on Python 3.12 and later nothing else checked the
name; such a client now refuses a certificate that does not carry the host it
connected to. A caller-supplied validate_callback still runs on top of that check,
since it may be looking at something else entirely; cert_reqs=ssl.CERT_NONE
switches verification off altogether, as before.

The default validate_callback on Python 3.12 and later, which stood in for the
ssl.match_hostname removed in that release, now checks a peer certificate against
an IP address and raises for a name rather than reporting a success it cannot back.
Name matching belongs to OpenSSL on those versions. The only caller left in the
library is TSSLServerSocket, which validates a client certificate against the
address the connection arrived from.

THttpServer now checks a request's Content-Length before it reads the body: a
missing length is answered with 411, one that is not a non-negative number with
400, and one larger than max_body_size with 413. max_body_size is a new constructor
argument and defaults to DEFAULT_MAX_FRAME_SIZE (16384000 bytes), the limit
TFramedTransport and THeaderTransport apply. The body is read whole before it is
processed, so a length inside the message can no longer ask the connection for
more than the body carries.

TBinaryProtocol and TCompactProtocol, their accelerated variants and their
factories now refuse a string or binary field longer than string_length_limit by
default, before reading it. The default is DEFAULT_STRING_LENGTH_LIMIT in
thrift.protocol.TProtocol, 16384000 bytes, the frame size limit the framed and
header transports already apply. Pass string_length_limit=None to read strings of
any length, as before. Without strictRead, TBinaryProtocol now applies the same
limit to the method name of an old-style message header.

TNonblockingServer now closes a connection whose frame declares more than
max_frame_size, instead of collecting the frame. max_frame_size is a new constructor
argument and defaults to DEFAULT_MAX_FRAME_SIZE (16384000 bytes), the limit
TFramedTransport and THeaderTransport apply.
