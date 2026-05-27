'Recursion-depth limit test for the *generated* Smalltalk struct read/write path.

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

 This drives the recursion-depth limit through the generated struct read/write
 code (DeepClient sendEchoA: / recvEcho), not by poking the depth counter in
 isolation. The recursive types in test/Recursive.thrift cannot be used: the
 Smalltalk generator inline-expands nested struct serialization and recurses
 without bound at code-generation time on a recursive type, so a finite chain of
 distinct struct types (A1..A65) is used to nest past the limit (64) instead.

 To run (Squeak/Pharo, or GNU Smalltalk with SUnit):
   thrift --gen st RecursionDepthTest.thrift
   file in: thrift.st, the generated RecursionDepthTest.st, then this file.
   Run the TProtocolRecursionDepthTest suite.
'!

"A minimal in-memory transport so the round-trip needs no socket: writeByte:
 hands us an Array, writeString: a String; we normalise both to bytes and replay
 them as a ByteArray (first -> integer for readByte, asString -> String)."
TTransport subclass: #TMemoryBuffer
	instanceVariableNames: 'bytes pos'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TMemoryBuffer class methodsFor: 'instance creation'!
new
	^ super new init! !

!TMemoryBuffer methodsFor: 'init'!
init
	bytes := OrderedCollection new.
	pos := 0! !

!TMemoryBuffer methodsFor: 'transport'!
write: aCollection
	aCollection do: [:each | bytes add: each asInteger]! !

!TMemoryBuffer methodsFor: 'transport'!
read: anInteger
	| chunk |
	chunk := bytes copyFrom: pos + 1 to: pos + anInteger.
	pos := pos + anInteger.
	^ chunk asByteArray! !

!TMemoryBuffer methodsFor: 'transport'!
flush! !

!TMemoryBuffer methodsFor: 'transport'!
open
	^ self! !

!TMemoryBuffer methodsFor: 'transport'!
close! !

!TMemoryBuffer methodsFor: 'transport'!
isOpen
	^ true! !

TestCase subclass: #TProtocolRecursionDepthTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TProtocolRecursionDepthTest methodsFor: 'support'!
recursionLimit
	"The limit enforced by TProtocol>>incrementRecursionDepth."
	^ 64! !

!TProtocolRecursionDepthTest methodsFor: 'support'!
clientOn: aTransport
	^ DeepClient new
		inProtocol: (TBinaryProtocol new transport: aTransport);
		yourself! !

!TProtocolRecursionDepthTest methodsFor: 'support'!
chainOfDepth: depth
	"An A1->A2->..->A`depth` chain of distinct structs; deeper links stay nil.
	 Writing it drives `depth` nested struct writers, i.e. depth levels deep."
	| node |
	node := nil.
	depth to: 1 by: -1 do: [:i | | cur |
		cur := (Smalltalk at: ('A', i printString) asSymbol) new.
		i = 65 ifTrue: [cur x: 1] ifFalse: [cur f: node].
		node := cur].
	^ node! !

!TProtocolRecursionDepthTest methodsFor: 'support'!
craftReplyOfDepth: n
	"Craft an echo_result REPLY whose success value nests n structs deep, using
	 raw protocol primitives so the depth counter is bypassed on the way in (a
	 normal write of an over-limit value would itself be rejected). Reading it
	 back drives the generated struct reader n (+1 for the result) levels deep."
	| transport proto emit |
	transport := TMemoryBuffer new.
	proto := TBinaryProtocol new transport: transport.
	proto writeMessageBegin: (TCallMessage new name: 'echo'; seqid: 0; yourself).
	proto writeFieldBegin: (TField new name: 'success'; type: TType struct; id: 0).
	emit := nil.
	emit := [:i |
		i < n ifTrue: [
			proto writeFieldBegin: (TField new name: 'f'; type: TType struct; id: 1).
			emit value: i + 1].
		proto writeFieldStop].
	emit value: 1.
	proto writeFieldStop.
	proto writeMessageEnd.
	^ transport! !

!TProtocolRecursionDepthTest methodsFor: 'tests'!
testWriteAtLimitRoundTrips
	"A chain exactly at the limit serializes without error."
	self
		shouldnt: [(self clientOn: TMemoryBuffer new) sendEchoA: (self chainOfDepth: self recursionLimit)]
		raise: TProtocolError! !

!TProtocolRecursionDepthTest methodsFor: 'tests'!
testWriteOverLimitIsRejected
	"A chain one level past the limit is rejected with TProtocolError."
	self
		should: [(self clientOn: TMemoryBuffer new) sendEchoA: (self chainOfDepth: self recursionLimit + 1)]
		raise: TProtocolError! !

!TProtocolRecursionDepthTest methodsFor: 'tests'!
testReadWithinLimitRoundTrips
	"A shallow payload is read back through the generated reader without error."
	self
		shouldnt: [(self clientOn: (self craftReplyOfDepth: 3)) recvEcho]
		raise: TProtocolError! !

!TProtocolRecursionDepthTest methodsFor: 'tests'!
testReadOverLimitIsRejected
	"A payload nested past the limit is rejected with TProtocolError on read."
	self
		should: [(self clientOn: (self craftReplyOfDepth: self recursionLimit + 6)) recvEcho]
		raise: TProtocolError! !
