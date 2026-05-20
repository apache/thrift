Thrift Compact protocol encoding
================================

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

This documents describes the encoding for RPC using the Thrift *compact protocol*.

The information here is _mostly_ based on the Java implementation in the Apache thrift library (version 0.9.1) and
[THRIFT-110 A more compact format](https://issues.apache.org/jira/browse/THRIFT-110). Other implementation however,
should behave the same.

For background on Thrift see the [Thrift whitepaper (pdf)](https://thrift.apache.org/static/files/thrift-20070401.pdf).

# Contents

* Compact protocol
  * ZigZag encoding
  * Varint encoding
  * Base types
  * Message
  * Struct
  * List and Set
  * Map
* BNF notation used in this document

# Compact protocol

### ZigZag encoding

Integers that *might* be negative are transformed using
[ZigZag](https://en.wikipedia.org/wiki/Variable-length_quantity#Zigzag_encoding)
encoding.  Zigzag encoding maps signed integers to another domain, one where the sign bit
is encoded in the least significant bit (LSB). For example 0 maps to 0, -1 to 1, 1 to 2, -2 to 3,
etc., hence the term zigzag. Mapping the sign bit to the LSB is important for compactness when
the absolute value of the value is small.

Here are the (Scala) formulas to convert from `int64` to a zigzag `int64` and back:

```scala
def ToZigzag(n: Long): Long = (n << 1) ^ (n >> 63)
def FromZigzag(n: Long): Long = (n >>> 1) ^ - (n & 1)
```

### Varint encoding

Integers larger than a single byte are encoded using varint
(aka [ULEB128](https://en.wikipedia.org/wiki/LEB128)) encoding.

A ULEB128 value is encoded 7 bits at a time,
starting from the LSB. Each 7 bits are encoded as 8 bits with the top bit set
if it is not the last byte.

For example, the integer 50399 is encoded as follows:

```
50399 =          11000100 11011111  (LSB)
      =  0000011  0001001  1011111  (7-bit groups)
      = 00000011 10001001 11011111  (add continuation bits)
      =     0x03     0x89     0xDF  (hex)
→ 0xDF 0x89 0x03 (write to ram LSB first)
```

## Base types

### Integer encoding

Values of type `i8` and `byte` are encoded as one byte.
Values of type `i16`, `i32` and `i64` are first transformed to using [ZigZag](#zigzag-encoding)
and that result is then [varint](#varint-encoding) encoded before being written.

### Enum encoding

`enum`s are encoded as their integer value as defined by the Thrift IDL.

### Binary encoding

`binary` is sent as follows:

```
Binary protocol, binary data, 1+ bytes:
+--------+...+--------+--------+...+--------+
| byte length         | bytes               |
+--------+...+--------+--------+...+--------+
```

Where:

* `byte length` is the length of the byte array (non-negative), encoded as a
   [varint](#varint-encoding) (**not**  [ZigZag'ed](#zigzag-encoding)).
* `bytes` are the bytes of the byte array.

### String encoding

`string`s are written as [binary](#binary-encoding) and must be encoded as UTF-8. Strings do
 not include a NULL(0) terminator.

### Double encoding

Values of type `double` are written in IEEE 754 format (the standard representation on almost
all systems) in little-endian byte order (8 bytes).

### Boolean encoding

`bool`s are encoded differently depending on whether a field value (in a struct) or an
element value (in a set, list or map). Field values are encoded directly in the field header.
Element values of type `bool` are sent as an `i8`; true as `1` and false as `2`.

### Universal unique identifier encoding

Values of `uuid` type are not encoded. That is, the 128 bits that make up the source `uuid`
are written without modification.  UUIDs not stored in the
[standard big-endian format](https://en.wikipedia.org/wiki/Universally_unique_identifier#Encoding)
should be converted to such before encoding with the compact protocol.

## Message

A `Message` on the wire looks as follows:

```
Compact protocol Message (4+ bytes):
+--------+--------+--------+...+--------+--------+...+--------+
|pppppppp|mmmvvvvv| seq id              | name                |
+--------+--------+--------+...+--------+--------+...+--------+
```

Where:

* `pppppppp` is the protocol id, fixed to `1000 0010`, 0x82.
* `mmm` is the message type, an unsigned 3-bit value.
* `vvvvv` is the version, an unsigned 5-bit integer, fixed to `00001`.
* `seq id` is the sequence id, a signed 32-bit value encoded as a [varint](#varint-encoding).
   The value is **not** [ZigZag'ed](#zigzag-encoding).
* `name` is the method name to invoke, encoded as a [string](#string-encoding).

Message types are encoded with the following values:

* _Call_: 1
* _Reply_: 2
* _Exception_: 3
* _Oneway_: 4

### Struct

A `struct` is a sequence of zero or more fields, followed by a stop field. Each field
other than a stop field starts with a field header and is followed by the encoded field value.
The encoding can be summarized by the following BNF:

```
struct        ::= ( field-header field-value )* stop-field
field-header  ::= field-type field-id
```

Because each field header contains the field-id (as defined by the Thrift IDL file), the fields can be encoded in any
order. Thrift's type system is not extensible; you can only encode the primitive types and structs. Therefore it is also
possible to handle unknown fields while decoding by ignoring them. The field type is used to determine how to decode field values.

Note that the fields are identified by their integer value and not their name.

The default Java implementation (Apache Thrift 0.9.1) has undefined behavior when it tries to decode a field that has
another field-type than what is expected. Theoretically this could be detected at the cost of some additional checking.
Other implementation may perform this check and then either ignore the field, or return a protocol exception.

A `union` is encoded the same as a struct with the additional restriction that at most 1 field may be encoded.

An `exception` is encoded exactly the same as a struct.

### Struct encoding

```
Compact protocol field header (short form) and field value:
+--------+--------+...+--------+
|ddddtttt| field value         |
+--------+--------+...+--------+

Compact protocol field header (1 to 3 bytes, long form) and field value:
+--------+--------+...+--------+--------+...+--------+
|0000tttt| field id            | field value         |
+--------+--------+...+--------+--------+...+--------+

Compact protocol stop-field:
+--------+
|00000000|
+--------+
```

Where:

* `dddd` is the field id delta, an unsigned 4-bit integer.
* `tttt` is field-type, an unsigned 4-bit integer (see below).
* `field id` the field id (i16), [ZigZag'ed](#zigzag-encoding) [varint](#varint-encoding) encoded.
* `field-value` is the encoded field value.

The field id delta can be computed by `current-field-id - previous-field-id`, or just
`current-field-id` if this is the first of the struct. The short form should be used when
the field id delta is in the range 1 - 15 (inclusive).

The following field-types can be encoded:

* `BOOLEAN_TRUE`, encoded as `1`
* `BOOLEAN_FALSE`, encoded as `2`
* `I8`, encoded as `3`
* `I16`, encoded as `4`
* `I32`, encoded as `5`
* `I64`, encoded as `6`
* `DOUBLE`, encoded as `7`
* `BINARY`, used for binary and string fields, encoded as `8`
* `LIST`, encoded as `9`
* `SET`, encoded as `10`
* `MAP`, encoded as `11`
* `STRUCT`, used for both structs and union fields, encoded as `12`
* `UUID`, encoded as `13`

Note that because there are 2 specific field types for the boolean values, the encoding of a boolean field value has no
length (0 bytes).

## List and Set

`list`s and `set`s are encoded the same: a header indicating the size and the element-type of
the elements, followed by the encoded elements.

```
Compact protocol list header (1 byte, short form) and elements:
+--------+--------+...+--------+
|sssstttt| elements            |
+--------+--------+...+--------+

Compact protocol list header (2+ bytes, long form) and elements:
+--------+--------+...+--------+--------+...+--------+
|1111tttt| size                | elements            |
+--------+--------+...+--------+--------+...+--------+
```

Where:

* `ssss` is the size, 4-bit unsigned values `0` - `14`
* `tttt` is the element-type, a 4-bit unsigned value
* `size` is the size, an unsigned 32-bit [varint](#varint-encoding),
   `15` or higher (**not** [ZigZag encoded](#zigzag-encoding)).
* `elements` are the encoded elements

The short form should be used when the length is in the range 0 - 14 (inclusive).

The following element-types are used (see note 1 below):

* `BOOL`, encoded as `1` or `2` (see note 2 below)
* `I8`, encoded as `3`
* `I16`, encoded as `4`
* `I32`, encoded as `5`
* `I64`, encoded as `6`
* `DOUBLE`, encoded as `7`
* `BINARY`, used for binary and string fields, encoded as `8`
* `LIST`, encoded as `9`
* `SET`, encoded as `10`
* `MAP`, encoded as `11`
* `STRUCT`, used for structs and union fields, encoded as `12`
* `UUID`, encoded as `13`

*Note*:
1. Although field-types and element-types lists are currently very similar, there is _no guarantee_ that this will
remain true after new types are added.
2. For historical and compatibility reasons, a reader should be capable to deal with *both* cases.
The only valid value in the original spec was `2`, but due to an widespread implementation bug the defacto
standard across large parts of the library became `1` instead. As a result, both values are now allowed.

The maximum list/set size is configurable. By default there is no limit (meaning the limit
is the maximum signed 32-bit integer value: 2147483647).

## Map

Maps are encoded with a header indicating the size, the type of the keys and the element-type of the elements, followed
by the encoded elements. The encoding follows this BNF:

```
map           ::= empty-map | non-empty-map
empty-map     ::= `0`
non-empty-map ::= size key-element-type value-element-type (key value)+
```

```
Compact protocol map header (1 byte, empty map):
+--------+
|00000000|
+--------+

Compact protocol map header (2+ bytes, non empty map) and key value pairs:
+--------+...+--------+--------+--------+...+--------+
| size                |kkkkvvvv| key value pairs     |
+--------+...+--------+--------+--------+...+--------+
```

Where:

* `size` is the 32-bit unsigned size, [varint](#varint-encoding) encoded
   (**not** [ZigZag'ed](#zigzag-encoding)).
* `kkkk` is the key element-type, a 4-bit unsigned value
* `vvvv` is the value element-type, a 4-bit unsigned value
* `key value pairs` are the encoded keys and values

The element-types are the same as for lists. The full list is included in the
'List and set' section.

The maximum map size is configurable. By default there is no limit
(meaning the limit is the maximum 32-bit value: 2147483647).

# BNF notation used in this document

The following BNF notation is used:

* a plus `+` appended to an item represents repetition; the item is repeated 1 or more times
* a star `*` appended to an item represents optional repetition; the item is repeated 0 or more times
* a pipe `|` between items represents choice, the first matching item is selected
* parenthesis `(` and `)` are used for grouping multiple items
