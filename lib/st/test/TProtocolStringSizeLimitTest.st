'String-size limit test for the Smalltalk binary protocol.

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

 readString sizes its read from a length the peer declared, and issues that read
 before any of those bytes have arrived. These tests therefore assert on the
 number of bytes the protocol ASKS THE TRANSPORT FOR, not merely on whether an
 exception was raised. That distinction is the whole point: an unbounded
 readString also ends in an exception once the peer stops sending, so a test
 that only checks for a raise passes on unbounded code too. What separates the
 two is that a bounded readString never asks for the oversized payload at all --
 four bytes for the length prefix, and nothing more.

 For reference, the numbers this suite pins down: with a declared length of
 2147483647 an unbounded readString asks the transport for 2147483651 bytes; a
 bounded one asks for 4.

 Unlike TProtocolRecursionDepthTest this needs no generated code -- it exercises
 the library alone.

 To run (Squeak/Pharo):
   file in thrift.st, then this file, then run the
   TProtocolStringSizeLimitTest suite.
'!

"A transport that records how many bytes it was ASKED for, and serves at most
 what it actually holds. The requested count is the observation every test here
 turns on."
TTransport subclass: #TCountingTransport
	instanceVariableNames: 'requested bytes pos'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TCountingTransport class methodsFor: 'instance creation'!
on: aByteArray
	^ self new setBytes: aByteArray! !

!TCountingTransport methodsFor: 'init'!
setBytes: aByteArray
	bytes := aByteArray.
	pos := 0.
	requested := 0! !

!TCountingTransport methodsFor: 'accessing'!
requested
	"Total bytes asked of this transport, whether or not they were there."
	^ requested! !

!TCountingTransport methodsFor: 'transport'!
read: anInteger
	| avail |
	requested := requested + anInteger.
	avail := (bytes size - pos) min: anInteger.
	^ [bytes copyFrom: pos + 1 to: pos + avail] ensure: [pos := pos + avail]! !

!TCountingTransport methodsFor: 'transport'!
write: aCollection! !

!TCountingTransport methodsFor: 'transport'!
flush! !

!TCountingTransport methodsFor: 'transport'!
open
	^ self! !

!TCountingTransport methodsFor: 'transport'!
close! !

!TCountingTransport methodsFor: 'transport'!
isOpen
	^ true! !

TestCase subclass: #TProtocolStringSizeLimitTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TProtocolStringSizeLimitTest methodsFor: 'support'!
defaultLimit
	"The default enforced by TProtocol>>stringSizeLimit."
	^ 16384000! !

!TProtocolStringSizeLimitTest methodsFor: 'support'!
i32: anInteger
	"anInteger as four big-endian bytes."
	^ ByteArray
		with: ((anInteger bitShift: -24) bitAnd: 16rFF)
		with: ((anInteger bitShift: -16) bitAnd: 16rFF)
		with: ((anInteger bitShift: -8) bitAnd: 16rFF)
		with: (anInteger bitAnd: 16rFF)! !

!TProtocolStringSizeLimitTest methodsFor: 'support'!
stringOf: declaredSize payload: aByteArray
	"A binary-protocol string: the declared length, then whatever payload we
	 choose to supply -- deliberately far less than declared in most tests, since
	 the question is what the protocol asks for before the payload arrives."
	^ (self i32: declaredSize), aByteArray! !

!TProtocolStringSizeLimitTest methodsFor: 'support'!
stringOf: declaredSize
	^ self stringOf: declaredSize payload: #[65 66]! !

!TProtocolStringSizeLimitTest methodsFor: 'support'!
protocolOn: aByteArray
	^ TBinaryProtocol new transport: (TCountingTransport on: aByteArray)! !

!TProtocolStringSizeLimitTest methodsFor: 'support'!
codeFrom: aBlock
	"The TProtocolError code aBlock signals, or nil if it signals none."
	^ [aBlock value. nil] on: TProtocolError do: [:e | e code]! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testDefaultLimitIsTheDocumentedValue
	self
		assert: TBinaryProtocol new stringSizeLimit = self defaultLimit
		description: 'the default string size limit should be 16384000'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testOverLimitSignalsSizeLimit
	"sizeLimit, not negativeSize -- the two must stay distinguishable."
	| proto |
	proto := self protocolOn: (self stringOf: 16r7FFFFFFF).
	self
		assert: (self codeFrom: [proto readString]) = TProtocolError sizeLimit
		description: 'an over-limit length should signal sizeLimit'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testOverLimitNeverAsksForThePayload
	"The fix in one assertion. Four bytes for the length prefix and nothing more;
	 unbounded, this transport is asked for 2147483651."
	| proto |
	proto := self protocolOn: (self stringOf: 16r7FFFFFFF).
	self codeFrom: [proto readString].
	self
		assert: proto transport requested = 4
		description: 'the oversized read must never be issued'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testJustOverTheDefaultLimitIsRejected
	"The boundary, from above."
	| proto |
	proto := self protocolOn: (self stringOf: self defaultLimit + 1).
	self
		assert: (self codeFrom: [proto readString]) = TProtocolError sizeLimit
		description: 'limit + 1 should be rejected'.
	self
		assert: proto transport requested = 4
		description: 'limit + 1 should be rejected before reading'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testWithinLimitRoundTrips
	"A legitimate string is unaffected: read in full, four bytes of prefix plus
	 its two bytes of payload."
	| proto |
	proto := self protocolOn: (self stringOf: 2).
	self assert: proto readString = 'AB' description: 'a short string should read back'.
	self
		assert: proto transport requested = 6
		description: 'a short string should cost prefix + payload and no more'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testEmptyStringRoundTrips
	| proto |
	proto := self protocolOn: (self stringOf: 0 payload: #[]).
	self assert: proto readString = '' description: 'a zero length should read as empty'.
	self
		assert: proto transport requested = 4
		description: 'a zero length should issue no payload read'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testNegativeSizeStillSignalsNegativeSize
	"Pre-existing behaviour, guarded: the negative check runs before the size
	 check, so a negative length keeps its own error code."
	| proto |
	proto := self protocolOn: (self stringOf: -1 payload: #[]).
	self
		assert: (self codeFrom: [proto readString]) = TProtocolError negativeSize
		description: 'a negative length should still signal negativeSize'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testConfiguredLimitIsHonoured
	"The bound is caller-settable, so a deployment moving larger payloads has a
	 route -- and a smaller one is enforced just as the default is."
	| proto |
	proto := self protocolOn: (self stringOf: 11).
	proto stringSizeLimit: 10.
	self
		assert: (self codeFrom: [proto readString]) = TProtocolError sizeLimit
		description: 'a configured limit should reject limit + 1'.
	self
		assert: proto transport requested = 4
		description: 'a configured limit should reject before reading'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testAtConfiguredLimitIsAccepted
	"The boundary, from below: exactly at the limit is allowed through."
	| proto |
	proto := self protocolOn: (self stringOf: 10 payload: #[65 66 67 68 69 70 71 72 73 74]).
	proto stringSizeLimit: 10.
	self
		assert: proto readString size = 10
		description: 'a length exactly at the limit should be accepted'! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testNonPositiveLimitIsRejected
	"A zero or negative limit would silently disable the bound, which is the
	 defect this change exists to remove. The positive case is asserted first so
	 that this test cannot pass merely because the accessor is absent -- a bare
	 should:raise:Error is satisfied by a doesNotUnderstand."
	| proto |
	proto := TBinaryProtocol new.
	proto stringSizeLimit: 1024.
	self
		assert: proto stringSizeLimit = 1024
		description: 'a positive limit should be accepted'.
	self should: [proto stringSizeLimit: 0] raise: Error.
	self should: [proto stringSizeLimit: -1] raise: Error! !

!TProtocolStringSizeLimitTest methodsFor: 'tests'!
testMessageNameIsBounded
	"Reachability: readMessageBegin takes the method name through readString, so
	 the bound applies before any handler is chosen. Eight bytes -- the version
	 word and the name length -- and nothing more."
	| proto |
	proto := self protocolOn: (self i32: 16r80010001), (self i32: 16r7FFFFFFF), #[65 66].
	self
		assert: (self codeFrom: [proto readMessageBegin]) = TProtocolError sizeLimit
		description: 'an over-limit message name should be rejected'.
	self
		assert: proto transport requested = 8
		description: 'an over-limit message name should be rejected before reading'! !
