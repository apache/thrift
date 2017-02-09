Thrift Perl Software Library

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

Summary
=======

Apache Thrift is a software framework for scalable cross-language services development.
It combines a software stack with a code generation engine to build services that work
efficiently and seamlessly between many programming languages.  A language-neutral IDL
is used to generate functioning client libraries and server-side handling frameworks.

For More Information
====================

See the [Apache Thrift Web Site](http://thrift.apache.org/) for more information.

Using Thrift with Perl
======================

Thrift requires Perl >= 5.6.0

Unexpected exceptions in a service handler are converted to
TApplicationException with type INTERNAL ERROR and the string
of the exception is delivered as the message.

On the client side, exceptions are thrown with die, so be sure
to wrap eval{} statments around any code that contains exceptions.

Please see tutoral and test dirs for examples.

The Perl ForkingServer ignores SIGCHLD allowing the forks to be
reaped by the operating system naturally when they exit.  This means
one cannot use a custom SIGCHLD handler in the consuming perl
implementation that calls serve().  It is acceptable to use
a custom SIGCHLD handler within a thrift handler implementation
as the ForkingServer resets the forked child process to use
default signal handling.

Dependencies
============

Bit::Vector       - comes with modern perl installations.
Class::Accessor
IO::Socket::INET  - comes with modern perl installations.
IO::Socket::SSL   - required if using SSL/TLS.
NET::SSLeay
Crypt::SSLeay     - for make cross
