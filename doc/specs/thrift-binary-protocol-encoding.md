Thrift Protocol Encoding for BinaryProtocol and CompactProtocol
====================================================================

Last Modified: 2016-Jun-29

! WARNING !

This document is _work in progress_ and should not (yet) be seen as an authoritative source of information.

This text is submitted to the Thrift community for review and improvements.

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

There are many ways to encode Thrift on the wire. This documents focuses on the wire encoding for services calls
(encoding and semantics) in the Thrift older *binary protocol* (which has not been documented before) and the
*compact protocol*. Both the regular socket transport (unframed) and the framed transport are described.

Note that no effort is made to group descriptions of behavior of the Thrift server and the encodings used in the
Thrift wire format. The order in which things are described is such that you can read the document from top to bottom.

The information here is mostly based on the Java implementation in the Apache thrift library (version 0.9.1) and
[THRIFT-110 A more compact format](https://issues.apache.org/jira/browse/THRIFT-110). Other implementation however,
should behave the same.

## Message exchange

Both the binary protocol and the compact protocol assume a transport layer that exposes a bi-directional byte stream,
for example a TCP socket. Both use the following message exchange:

1. Client sends a `TMessage` (type `Call`). The TMessage contains some metadata and the name of the method to invoke.
2. Client sends method arguments (a struct defined by the generate code).
3. Server sends a `TMessage` (type `Response` or `Exception`) to start the response.
4. Server sends completes response with a struct (a predefined struct or one defined by generated code).

The pattern is a simple half duplex protocol where the parties alternate in sending a `TMessage` followed by a struct.
What these are is described below.

Although the standard Apache Thrift Java clients do not support pipelining (sending multiple requests without waiting
for an response), the standard Apache Thrift Java servers do support it.

## TMessage

A *TMessage* contains the following information:

* _Message type_, a message types, one of `Call`, `Reply`, `Exception` and `Oneway`.
* _Sequence id_, an int32 integer.
* _Name_, a string (can be empty).

The *sequence id* is a simple message id assigned by the client. The server will use the same sequence id in the
TMessage of the response. The client uses this number to detect out of order responses. Each client has a int32 field
which is increased for each message. The sequence id simply wraps around when it overflows.

The *name* indicates the service method name to invoke. The server uses the same name in the TMessage of the response.

When the *multiplexed protocol* is used, the name contains the service name, a colon `:` and the method name. The
multiplexed protocol is not compatible with other protocols.

The *message type* indicates what kind of message is sent.

Clients send requests with TMessages of type `Call` or `Oneway` (step 1 in the protocol exchange). Servers send
responses with TMessages of type `Exception` or `Reply`.

### Oneway

Type `Oneway` is only used starting from Apache Thrift 0.9.3. Earlier versions do _not_ send TMessages of type `Oneway`,
even for service methods defined with the `oneway` modifier.

When client sends a request with type `Oneway`, the server must _not_ send a response (steps 3 and 4 are skipped).

## Integer encoding

In the _binary protocol_ integers are encoded with the most significant byte first (big endian byte order, aka network
order). An `int8` needs 1 byte, an `int16` 2, an `int32` 4 and an `int64` needs 8 bytes.

The CPP version has the option to use the binary protocol with little endian order. Little endian gives a small but
noticeable performance boost because contemporary CPUs use little endian when storing integers to RAM.

The _compact protocol_ uses multiple encodings for ints: the _zigzag int_, and the _var int_.

Values of type `int32` and `int64` are first transformed to a *zigzag int*. A zigzag int folds positive and negative
numbers into the positive number space. When we read 0, 1, 2, 3, 4 or 5 from the wire, this is translated to 0, -1, 1,
-2 or 2 respectively. Here are the (scala) formulas to convert from int32/int64 to a zigzag int and back:

```scala
def intToZigZag(n: Int): Int = (n << 1) ^ (n >> 31)
def zigzagToInt(n: Int): Int = (n >>> 1) ^ - (n & 1)
def longToZigZag(n: Long): Long = (n << 1) ^ (n >> 63)
def zigzagToLong(n: Long): Long = (n >>> 1) ^ - (n & 1)
```

The zigzag int is then encoded as a *var int*. Var ints take 1 to 5 bytes (int32) or 1 to 10 bytes (int64). The most
significant bit of each byte indicates if more bytes follow. The concatenation of the least significant 7 bits from each
byte form the number, where the first byte has the most significant bits (so they are in big endian or network order).

Var ints are sometimes used directly inside the compact protocol to represent positive numbers.

To encode an `int16` as zigzag int, it is first converted to an `int32` and then encoded as such. The type `int8` simply
uses a single byte as in the binary protocol.

## Enum encoding

The generated code encodes `Enum`s by taking the ordinal value and then encoding that as an int32.

## String encoding

*String*s are first encoded to UTF-8, and then send as Binary. Binary encoding is described later.

## Double encoding

Values of type `double` are first converted to a int64 according to the IEEE 754 floating-point "double format" bit
layout. Most run-times provide a library to make this conversion. Both the binary protocol as the compact protocol then
encode the int64 in 8 bytes in big endian order.

## TMessage encoding

A `TMessage` on the wire looks as follows:

```
Binary protocol (strict, 12+ bytes):
+--------+--------+--------+--------+--------+--------+--------+--------+--------+...+--------+--------+--------+--------+--------+
|1vvvvvvv|vvvvvvvv|unused  |00000mmm| name length                       | name                | seq id                            |
+--------+--------+--------+--------+--------+--------+--------+--------+--------+...+--------+--------+--------+--------+--------+

Binary protocol (old, 9+ bytes):
+--------+--------+--------+--------+--------+...+--------+--------+--------+--------+--------+--------+
| name length                       | name                |00000mmm| seq id                            |
+--------+--------+--------+--------+--------+...+--------+--------+--------+--------+--------+--------+
```

Where:

* `vvvvvvvvvvvvvvv` is the version, an unsigned 15 bit number fixed to `1` (in binary: `000 0000 0000 0001`). The leading bit must be `1`.
* `unused` is an ignored byte.
* `mmm` is the message type, an unsigned 3 bit integer. The 5 leading bits must be `0` as some clients (checked for java in 0.9.1) take the whole byte.
* `name length` is the byte length of the name field, a signed 32 bit integer encoded in network (big endian) order (must be >= 0).
* `name` is the method name to invoke, a UTF-8 encoded string.
* `seq id` is the sequence id, a signed 32 bit integer encoded in network (big endian) order.

Because name length must be positive (therefore the first bit is always `0`), the first bit allows the receiver to see
whether the strict format or the old format is used. Therefore a server and client using the different variants of the
binary protocol can transparently talk with each other. However, when strict mode is enforced, the old format is
rejected.

```
Compact protocol (4+ bytes):
+--------+--------+--------+...+--------+--------+...+--------+--------+...+--------+
|pppppppp|mmmvvvvv| seq id              | name length         | name                |
+--------+--------+--------+...+--------+--------+...+--------+--------+...+--------+
```

Where:

* `pppppppp` is the protocol id, fixed to `1000 0010`, 0x82
* `vvvvv` is the version, an unsigned 5 bit integer, fixed to `00001`, 0x1
* `mmm` is the message type, an unsigned 3 bit integer
* `seq id` is the sequence id, a signed 32 bit integer encoded as a var int.
* `name length` is the byte length of the name field, a signed 32 bit integer encoded as a var int (must be >= 0).
* `name` is the method name to invoke, a UTF-8 encoded string.

A server could automatically determine whether a client talks the binary protocol or the compact protocol by
investigating the first byte. If the value is `1000 0001` or `0000 0000` (assuming a name shorter then ±16 MB) it is the
binary protocol. When the value is `1000 0010` it is talking the compact protocol.

Message types are encoded with the following values:

* _Call_: 1
* _Reply_: 2
* _Exception_: 3
* _Oneway_: 4

For a method name of 32 bytes, the binary protocol (strict) needs 44 bytes and the compact protocol needs 36 to 40 bytes.

## Method arguments, return types and exceptions

TODO: method arguments are encoded as a struct

TODO: return value are encoded as a ?

Both the binary protocol and compact protocol encode the result the same. The result is encoded as a struct with a
single field. The field-id of that field is `0`. The type of the field corresponds to the type of the service method
return type. When the return value is `null`, an empty struct is encoded.

TODO: describe TupleProtocol also?

TODO: exceptions are encoded as a struct with what field?

### Structs and Unions

Both the binary protocol and the compact protocol encode structs as a sequence of fields, followed by a stop field. Each
field starts with a field header and is followed by the encoded field value. The encoding follows this BNF (`*` means 0
or more times, parenthesis are used for grouping):

```
struct        => ( field-header field-value )* stop-field
field-header  => field-type field-id
```

Because each field header contains the field-id (as defined by the IDL), the fields can be encoded in any order.
Thrift's type system is not extensible, you can only encode the primitive types and structs. Therefore is also possible
to handle unknown fields while decoding; these are simply ignored. While decoding the field type can be used to
determine how to decode the field value.

In the binary protocol the field header is always 3 bytes long. In the compact protocol the field header packs a lot
more cleverness. In most cases it is 1 byte long. In special cases it can grow to 2 or 3 bytes. When your field-ids are
very large it can even grow to 4 bytes.

Unions are encoded exactly the same as structs.

### Struct encoding in binary protocol

In the binary protocol field headers and the stop field are encoded as follows:

```
Binary protocol field header:
+--------+--------+--------+
|tttttttt| field id        |
+--------+--------+--------+

Binary protocol stop field:
+--------+
|00000000|
+--------+
```

Where:

* `tttttttt` the field-type, a signed 8 bit integer.
* `field id` the field-id, a signed 16 bit integer in big endian order.

The following field-types are used:

* `BOOL`, encoded as `2`
* `BYTE`, encoded as `3`
* `DOUBLE`, encoded as `4`
* `I16`, encoded as `6`
* `I32`, encoded as `8`
* `I64`, encoded as `10`
* `STRING`, used for binary and string fields, encoded as `11`
* `STRUCT`, used for structs and union fields, encoded as `12`
* `MAP`, encoded as `13`
* `SET`, encoded as `14`
* `LIST`, encoded as `15`

### Value encoding in binary protocol

Field and element values are always encoded the same way in the binary protocol.

Int8, int16, int32, int64 and Double encoding are described earlier in 'Integer encoding' and 'Double encoding'.

Values of *Bool* type are first converted to an int8. True is converted to `1`, false to `0`. Regular int8 encoding is
used in both protocols.

*Binary* is encoded by an int32 to indicate the length, followed by the bytes.

### Set and List encoding in binary protocol

List and sets are encoded the same: a header indicating the size and the element-type of the elements, followed by the
encoded elements.

```
Binary protocol list header (5 bytes):
+--------+--------+--------+--------+--------+
|tttttttt| size                              |
+--------+--------+--------+--------+--------+
```

Where:

* `tttt` is the element-type, encoded as an int8
* `size` is the size, encoded as an int32, positive values only

The element-type values are the same as field-types. The full list is included in the struct section above.

The maximum list/set size is configurable. By default there is no limit (meaning the limit is the maximum int32 value:
2147483647).

### Map encoding in binary protocol

Maps are encoded with a header indicating the size, the element-type of the keys and the element-type of the elements,
followed by the encoded elements. The encoding follows this BNF (`*` indicates 0 or more times, parenthesis are used for
grouping):

```
map  =>  key-element-type value-element-type size ( key value )*
```

```
Binary protocol map header (6 bytes):
+--------+--------+--------+--------+--------+--------+
|kkkkkkkk|vvvvvvvv| size                              |
+--------+--------+--------+--------+--------+--------+
```

Where:

* `kkkkkkkk` is the key element-type, encoded as an int8
* `vvvvvvvv` is the value element-type, encoded as an int8
* `size` is the size of the map, encoded as an int32, positive values only

The element-type values are the same as field-types. The full list is included in the struct section above.

The maximum map size is configurable. By default there is no limit (meaning the limit is the maximum int32 value:
2147483647).

### Struct encoding in compact protocol

```
Compact protocol field header (short form):
+--------+
|ddddtttt|
+--------+

Compact protocol field header (1 to 3 bytes, long form):
+--------+--------+...+--------+
|0000tttt| field id            |
+--------+--------+...+--------+

Compact protocol stop field:
+--------+
|00000000|
+--------+
```

Where:

* `dddd` is the field id delta, an unsigned 4 bits integer, strictly positive.
* `tttt` is field-type id, an unsigned 4 bit integer.
* `field id` the field id, a signed 16 bit integer encoded as zigzag int.

The following field-types can be encoded:

* `BOOLEAN_TRUE`, encoded as `1`
* `BOOLEAN_FALSE`, encoded as `2`
* `BYTE`, encoded as `3`
* `I16`, encoded as `4`
* `I32`, encoded as `5`
* `I64`, encoded as `6`
* `DOUBLE`, encoded as `7`
* `BINARY`, used for binary and string fields, encoded as `8`
* `LIST`, encoded as `9`
* `SET`, encoded as `10`
* `MAP`, encoded as `11`
* `STRUCT`, used for both structs and union fields, encoded as `12`

### Field value encoding in compact protocol

Int8, int16, int32, int64 and Double encoding are described earlier in 'Integer encoding' and 'Double encoding'.

There is _no_ need to encode values of *Bool* type; the field-type already tells us if the value is `true` or `false`.

*Binary* is encoded by an zigzag int (int32) to indicate the length, followed by the bytes.

### List and Set encoding in compact protocol

List and sets are encoded the same: a header indicating the size and the element-type of the elements, followed by the
encoded elements.

```
Compact protocol list header (short form):
+--------+
|sssstttt|
+--------+

Compact protocol list header (1 to 6 bytes, long form):
+--------+--------+...+--------+
|1111tttt| size                |
+--------+--------+...+--------+
```

Where:

* `ssss` is the size, 4 bit unsigned int, values `0` - `14`
* `tttt` is the element-type, a 4 bit unsigned int
* `size` is the size, a var int (int32), positive values `15` or higher

In the compact protocol element-types are _different_ from field-types. The element-type values are the same as the
_binary protocol_ field-types. The full list is included in the struct section for the binary protocol.

The maximum list/set size is configurable. By default there is no limit (meaning the limit is the maximum int32 value:
2147483647).

### Map encoding in compact protocol

Maps are encoded with a header indicating the size, the type of the keys and the element-type of the elements, followed
by the encoded elements. The encoding follows this BNF (`|` indicates choice, `+` indicates 1 or more times, parenthesis
are used for grouping):

```
map           => empty-map | non-empty-map
empty-map     => 0
non-empty-map => size key-element-type value-element-type (key value)+
```

```
Compact protocol map header (empty map):
+--------+
|00000000|
+--------+

Compact protocol map header (2 to 6 bytes, non empty map):
+--------+...+--------+--------+
| size                |kkkkvvvv|
+--------+...+--------+--------+
```

Where:

* `size` is the size, a var int (int32), strictly positive values
* `kkkk` is the key element-type, a 4 bit unsigned int
* `vvvv` is the value element-type, a 4 bit unsigned int

In the compact protocol element-types are _different_ from field-types. The element-type values are the same as the
_binary protocol_ field-types. The full list is included in the struct section for the binary protocol.

The maximum map size is configurable. By default there is no limit (meaning the limit is the maximum int32 value:
2147483647).

### List, set and map element-value encoding in the compact protocol

Encoding for element values is the same as encoding of field values with an exception for the boolean type. Booleans are
first converted to an int8 (`true` as `1`, `false` as `0`) and then then encoded as an int8.

## Framed vs. unframed transport

The first thrift binary wire format was unframed. This means that information is sent out in a single stream of bytes.
With unframed transport the (generated) processors will read directly from the socket (though Apache Thrift does try to
grab all available bytes from the socket in a buffer when it can).

Later, Thrift introduced the framed transport.

With framed transport the full request and response (the TMessage and the following struct) are first written to a
buffer. Then when the struct is complete (transport method `flush` is hijacked for this), the length of the buffer is
written to the socket first, followed by the buffered bytes. The combination is called a _frame_. On the receiver side
the complete frame is first read in a buffer before the message is passed to a processor.

The length prefix is a 4 byte signed int, send in network (big endian) order.
The following must be true: `0` <= length <= `16384000` (16M).

Framed transport was introduced to ease the implementation of async processors. An async processor is only invoked when
all data is received. Unfortunately, framed transport is not ideal for large messages as the entire frame stays in
memory until the message has been processed. In addition, the java implementation merges the incoming data to a single,
growing byte array. Every time the byte array is full it needs to be copied to a new larger byte array.

Framed and unframed transports are not compatible with each other.
