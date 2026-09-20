'Tests that TBinaryProtocol decodes only what it actually received.

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

 A transport read: may answer fewer bytes than it was asked for. That is not an
 error condition -- it is ordinary behaviour for a socket -- and TTransport
 provides readAll: to loop until the bytes have all arrived.

 What makes this worth testing carefully is that the wrong behaviour is not an
 exception but a WRONG ANSWER. intFromByteArray: takes its width from the size
 of the buffer it was handed rather than from the number of bytes that were
 asked for, so two bytes delivered for an i32 decode to a well-formed integer
 that happens to be wrong. A test that only checked for a raise would pass on
 the broken code. These tests therefore assert the decoded VALUE.

 To run (Squeak/Pharo):
   file in thrift.st, then this file, then run the TProtocolShortReadTest suite.
   Or: lib/st/test/run-tests.sh
'!

"Holds the whole wire content but never answers more than maxChunk bytes at a
 time, which is what a socket does. Answers nothing once exhausted.

 The content is a ByteArray, because that is what a real transport answers:
 TSocket connects its stream 'binary'. The protocol decodes by shifting the
 elements as integers, which a String's Characters do not understand."
TTransport subclass: #TShortReadTransport
	instanceVariableNames: 'contents pos maxChunk'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TShortReadTransport methodsFor: 'initialization'!
contents: aByteArray maxChunk: anInteger
	contents := aByteArray.
	maxChunk := anInteger.
	pos := 1! !

!TShortReadTransport methodsFor: 'reading'!
read: anInteger
	| take out |
	take := (anInteger min: maxChunk) min: contents size - pos + 1.
	take <= 0 ifTrue: [^ ByteArray new].
	out := contents copyFrom: pos to: pos + take - 1.
	pos := pos + take.
	^ out! !

!TShortReadTransport methodsFor: 'writing'!
write: aString! !

TestCase subclass: #TProtocolShortReadTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TProtocolShortReadTest methodsFor: 'private'!
protocolOn: anArray maxChunk: anInteger
	| transport |
	transport := TShortReadTransport new
		contents: anArray asByteArray maxChunk: anInteger.
	^ TBinaryProtocol new transport: transport! !

!TProtocolShortReadTest methodsFor: 'private'!
i32Bytes: anInteger
	^ Array
		with: ((anInteger bitShift: -24) bitAnd: 16rFF)
		with: ((anInteger bitShift: -16) bitAnd: 16rFF)
		with: ((anInteger bitShift: -8) bitAnd: 16rFF)
		with: (anInteger bitAnd: 16rFF)! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadI32IsAssembledAcrossShortReads
	"One byte at a time. Unfixed, readInt: decodes the single byte it got."
	| proto |
	proto := self protocolOn: (self i32Bytes: 16r01020304) maxChunk: 1.
	self assert: proto readI32 equals: 16r01020304! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadI32HalfDeliveredIsNotDecodedAsIfComplete
	"Two of the four bytes per call. Unfixed this yields a well-formed but wrong
	 integer rather than an error, which is the whole point of this suite."
	| proto |
	proto := self protocolOn: (self i32Bytes: 16r7FFFFFFF) maxChunk: 2.
	self assert: proto readI32 equals: 16r7FFFFFFF! !

!TProtocolShortReadTest methodsFor: 'tests'!
testNegativeI32IsAssembledAcrossShortReads
	"The sign is taken from the first byte of the buffer, so a short read gets
	 the sign right and the magnitude wrong -- the most misleading shape."
	| proto |
	proto := self protocolOn: (self i32Bytes: 16rFFFFFFFF) maxChunk: 3.
	self assert: proto readI32 equals: -1! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadI16IsAssembledAcrossShortReads
	| proto |
	proto := self protocolOn: #(16r12 16r34) maxChunk: 1.
	self assert: proto readI16 equals: 16r1234! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadI32RaisesWhenThePeerStops
	"Only three bytes will ever arrive. There is no correct answer, so the only
	 acceptable outcome is an error -- never a three byte integer."
	| proto |
	proto := self protocolOn: #(16r00 16r00 16r01) maxChunk: 2.
	self should: [proto readI32] raise: TTransportError! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadStringIsAssembledAcrossShortReads
	| proto wire |
	wire := (self i32Bytes: 5) , #(104 101 108 108 111) asArray.
	proto := self protocolOn: wire maxChunk: 2.
	self assert: proto readString equals: 'hello'! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadStringTruncatedByThePeerRaises
	"The peer declared five bytes and sent three. Unfixed, readString answers a
	 three character string as if it were complete."
	| proto wire |
	wire := (self i32Bytes: 5) , #(104 101 108) asArray.
	proto := self protocolOn: wire maxChunk: 2.
	self should: [proto readString] raise: TTransportError! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadDoubleIsAssembledAcrossShortReads
	"readDouble goes through readRawInt: twice."
	| proto wire |
	wire := #(16r3F 16rF0 16r00 16r00 16r00 16r00 16r00 16r00).
	proto := self protocolOn: wire maxChunk: 3.
	self assert: proto readDouble equals: 1.0! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadAllKeepsTheSpeciesTheTransportAnswered
	"A real transport answers a ByteArray. readAll: used to accumulate into a
	 String, which raises as soon as it is handed one."
	| transport out |
	transport := TShortReadTransport new contents: #[1 2 3 4] maxChunk: 2.
	out := transport readAll: 4.
	self assert: out class equals: ByteArray.
	self assert: out equals: #[1 2 3 4]! !

!TProtocolShortReadTest methodsFor: 'tests'!
testReadByteRaisesRatherThanFailingToUnderstand
	"An exhausted transport answers an empty string, and 'first' on that is a
	 doesNotUnderstand rather than a transport error."
	| proto |
	proto := self protocolOn: #() maxChunk: 4.
	self should: [proto readByte] raise: TTransportError! !
