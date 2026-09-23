Thrift TConfiguration
====================================================================

Last Modified: 2026-Sep-23

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

Starting with THRIFT-5021 the need to centralize certain limit settings that are used throughout the whole protocol / transport stack became an obvious need. Previous patches already added some of these limits, but they were not consistently managed and just randomly distributed across the code base. 

# Design goals

Following the tradition of similar experience across languages in Thrift, any implementation should meet these design goals:

 * There MUST be a standard CTOR (or equivalent thereof) that provides a default TConfiguration instance. 
 * The default values used SHOULD be implemented as outlined below.
 * For backwards compatibility, the protocol / transport stack should accept null TConfiguration argument, in which case it should fallback to a default instance automatically. This is to prevent from code-breaking changes as much as possible.

# Implementation

The new TConfiguration class or struct currently holds these settings:

## MaxMessageSize

The MaxMessageSize member defines the maximum size of a (received) message, in bytes. The default value is represented by a constant named DEFAULT_MAX_MESSAGE_SIZE, whose value is 100 * 1024 * 1024 bytes.

## MaxFrameSize

MaxFrameSize limits the size of one frame of data for the TFramedTransport. Since all implementations currently send messages in one frame only if TFramedTransport is used, this value may interfere with MaxMessageSize. In the case of an conflict, the smaller value of the two is used (see remark below). The default value is called DEFAULT_MAX_FRAME_SIZE and has a value of 16384000 bytes.

## RecursionLimit

The RecursionLimit defines, how deep structures may be nested into each other. The default named DEFAULT_RECURSION_DEPTH allows for structures nested up to 64 levels deep. 

## MaxContainerSize

MaxContainerSize limits the number of elements a single list, set or map may declare when it is read. It exists because MaxMessageSize cannot cover that case on its own. MaxMessageSize is counted in bytes, while what a container costs is its element count multiplied by the in-memory footprint of one element, and the two are only loosely related. A list of structs whose smallest possible encoding on the wire is a single byte stays comfortably inside any byte budget while still asking for one object per declared element. The two settings are therefore independent levers, and an implementation SHOULD offer both.

Unlike the three settings above, MaxContainerSize has no finite default. The default is "no limit of its own", because any finite default would reject messages that the same binding accepted before. This describes the setting as it is introduced, not a position on which default is right for a given deployment; §10 of doc/thrift-threat-model.md asks operators to set finite limits before exposing a server to untrusted peers. The element count stays bounded by MaxMessageSize and, where the transport knows how many bytes of the current message are left, by that remainder.

The Lua implementation departs from this. It has no MaxMessageSize, so nothing else would bound the count. Its maxContainerSize, set on the protocol object like its string size limit, therefore defaults to 16384000 elements: the value of its string and frame size limits, and of the older D limit described below. Every element takes at least one byte on the wire, so that default refuses no message that fits into one frame of the default size.

The value that switches the limit off is any value less than or equal to zero. An implementation in a language that has a natural spelling for an absent value, such as an option type, MAY use that spelling instead. The setting SHOULD be consulted where the declared element count is read, before the collection is sized, and a violation SHOULD be reported the way the other limits are, as a protocol exception of type SIZE_LIMIT.

# Further considerations

## MaxFrameSize vs. MaxMessageSize

The difference between the two options is, that MaxFrameSize exists much longer and it is used only in conjunction with TFramedTransport. In contrast, MaxMessageSize is intended to be a general device to be used with any transport or protocol. 

In order to combine both approaches in the most optimal way when using TFramedTransport, it is recommended that the implementation SHOULD update the remaining number of bytes to read based on the received frame size value for the current message.

For calculation purposes it is important to know, that MaxFrameSize excludes the 4 bytes that hold the frame size, while MaxMessageSize is always looking at the whole data. Hence, when updating the remaining read byte count, the known message size should be set to frameSize + sizeof(i32).

## MaxContainerSize and the older per-protocol limits

Several bindings gained a container size limit, usually with a string size limit next to it, long before TConfiguration existed. Those live on the protocol object instead of in the configuration: setContainerSizeLimit() in C++, a containerLengthLimit constructor argument in Java, container_length_limit in Python, containerSizeLimit in D, a container_limit property in C glib, container_limit_ in the Node.js compact protocol.

Where such a knob already exists, is documented, and is actually consulted on the path that sizes the collection, it meets the intent of MaxContainerSize, and the binding SHOULD NOT grow a second way to set the same limit. The MaxContainerSize member is meant for the bindings that have no such knob today, and for bindings added from here on.

All three conditions matter. A knob a caller cannot reach on a given path, because the factory that builds the protocol does not pass it on or because a layered protocol constructs its inner protocol without it, does not bound that path however well documented it is elsewhere in the binding.

Those older knobs do not agree on their defaults either. Most are off unless the caller sets them; the D implementation ships a finite container size limit of 16384000 elements and a string size limit of the same value, and the Python implementation ships a finite string length limit. That predates this document and is left as it stands.

## Error handling

If any limit is exceeded, an error should be thrown. Additionally, it may be helpful to check larger memory allocations against the remaining max number of bytes before the allocation attempt takes place.

# Q&A

## Is this a breaking change or not?

There is actually two answers to that question. 

1. If done right, it should not be a breaking change vis-á-vis compiling your source code that uses Thrift. 

1. It may, however, be a breaking change in the way it limits the accepted overall size of messages or the accepted frame size. This behaviour is by design. If your application hits any of these limits during normal operation, it may require you to instantiate an actual TConfiguration and tweak the settings according to your needs.

## Why is there no MaxStringSize?

Because MaxMessageSize already covers it. The declared length of a string or a binary is its length in bytes, which is the unit MaxMessageSize is counted in, so the existing budget bounds it directly, and a separate setting would only be a second spelling of the same limit. That is not true of a container, whose declared element count says very little about how many bytes it will occupy on the wire — see the MaxContainerSize section above.

Some bindings do expose a string size limit next to their older per-protocol container limit. Those are left alone; this answer is about what belongs in TConfiguration.

## Is splitting the general transport base class into Endpoint and Layered transport base classes necessary?

No, it's not. However, it turned out that this split is a great help when it comes to managing the TConfiguration instance that is passed through the stack. Having two distinct base classes for each of the different transport types not only allows to implement a shared solution for this. 

The added benefit is, that a clear distinction between the two transport types makes the Thrift architectural idea much more clear to "newbie" developers.

## I want to contribute an implementation of TConfiguration and I am not sure whether to pick class or struct?

Short answer: Pick whatever is more efficient in the language of your choice. 

Technically, remember that the instance is passed down the stack and should therefore be cheap on copying. To ensure this and to make sure all pieces of the protocol / transport stack are really pointing to the same TConfiguration instance, we want to pass the instance **by reference** rather than by value. 

For example, in the C# language a class is a suitable choice for this, because classes are naturally reference parameters, while structs are not. 

