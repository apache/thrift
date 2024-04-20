Thrift URIs
====================================================================

Last Modified: 2024-APR-21

<!--
--------------------------------------------------------------------

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

--------------------------------------------------------------------
-->

# Motivation and use case

Describing the endpoint specifics for a Thrift API today is a purely textual exercise, which leaves the client end with the task to set up and stack together a proper protocol/transport stack. That sometimes even leads to headaches, e.g. if the server requires e.g. framed protocol which might not be obvious. The use of a generally accepted, machine-readable and extensible Thrift URI format to describe client bindings could streamline that process.

# Thrift URI general format

Lets look at the general format:

	"thrift://" <protocol> "/" <transport> ["/" <layer>]* "?" <transport-specific-data>

The *scheme* of all Thrift URIs is "thrift://". Immediately followimng parts are the protocol being used and the endpoint transport.

Optionally, more path segments may be added, each one describing a particular layered transport, e.g. "framed". 

All query data (i.e. the part after the question mark) depend on the endpoint transport in the second segment, examples follow below. All data must be properly URL-encoded.

# Predefined identifiers

Each implementation shall register their implemented formats with the following keys internally:

## Protocols
|Code|Protocol|
|-|-|
|binary|Thrift Binary protocol|
|compact|Thrift Compact protocol|
|json|Standard Thrift JSON protocol|

TODO: cover multiplex protocol

## Endpoint Transports
|Code|Transport|
|-|-|
|http|http(s) transport|
|namedpipes|Named Pipes Transport|
|pipes|Simple Pipes Transport (i.e. via STDIN,STDOUT)|
|socket|Socket Transport|
|tlssocket|TLS Sockets Transport|
|file|File transport|
|memory|Memory Buffer Transport|

## Layered Transports
|Code|Transport|
|-|-|
|framed|Framed transport|
|buffered|Buffered transport|
|zlib|ZLib transport|

## TODO: multiplex protocol
....


# Extensibility

Consistent with the open and extensible nature of Thrift, the registration mechanism outlined above is intentionally designed to be open to any user-defined protocols and transports. That way, future developments as well as proprietary developments can be covered by the same mechanism. It is also expected, that depending on the implemented set of features, different languages supported by Thrift might support a different set of Thrift URI components.


# Transport specific data


## http - http(s) transport

The data part consists of the target URL. No other data are allowed.

Examples:
 * thrift://binary/http?https%3A%2F%2Fuser%3Apass%40example.com%2Fmyservice%3Farg%3Done%26arg%3Dtwo

## namedpipes - Named Pipes Transport

The data part consists of the target pipe, either name only or in full format:

Examples:
 * thrift://binary/namedpipes?mypipe
 * thrift://binary/namedpipes?mypath%5Cmyname
 * thrift://binary/namedpipes?%5C%5Cmyserver%5Cpipe%5Cmypath%5Cmyname

## pipes - Simple Pipes Transport (i.e. via STDIN,STDOUT)
....

## socket - Socket Transport

|key|argument|
|-|-|
|host|Host name or IP|
|port|Host port|

Examples:
 * thrift://compact/socket/framed?host=localhost&port=8080

## tlssocket - TLS Sockets Transport

|key|argument|
|-|-|
|host|Host name or IP|
|port|Host port|
|cert|path to certificate file|

Examples:
 * thrift://binary/tlssocket?host=localhost&port=8080&cert=C%3A%5CTemp%5Cclient.p12


## file - File transport

|key|argument|
|-|-|
|infile|Input file name|
|outfile|Output file name|

Examples:
 * thrift://json/file?infile=this.json&outfile=that.json


## memory - Memory Buffer Transport

No data expected.


# Q&A

## Is this a breaking change or not?

No, it is an extension to the Thrift ecosystem, intended to make implementation of Thrift clients easier and faster.

## Why are server transports not covered?

The use case is about client connections. A server end usually requires somewhat more sophisticated implementation efforts, and constructing the transport/protocol stacks is just a small part of the whole. It also highly depends on the nature of the server. A Thrift URI would not add much value at the server end. That does not mean that the spec cannot be extended in the future.


