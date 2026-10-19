'Container-size limit and skip-type tests for the Smalltalk binary protocol.

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

 skip iterates a container over the element count its header declares, calling
 skipType: once per element. For that to stay bounded by the bytes received,
 every element type skip meets must read at least one byte, descend a nested
 value, or be refused. Two things are checked here:

   1. readListBegin / readSetBegin / readMapBegin refuse a declared count above
      containerSizeLimit before any element is read -- asserted on the number of
      bytes the protocol ASKS THE TRANSPORT FOR (five, the container header),
      not merely on whether an exception was raised, since an unbounded skip
      ends in an exception too once the peer stops sending.
   2. skipType: refuses the stop type and any unrecognised type with a
      TProtocolError (invalidData), rather than returning with no read (which
      let a container of such elements iterate its whole declared count while
      reading nothing) or signalling a bare Error (which on: TError cannot
      catch).

 Like TProtocolStringSizeLimitTest this needs no generated code -- it exercises
 the library alone.
'!

"A transport that records how many bytes it was ASKED for, and serves at most
 what it actually holds. The requested count is the observation the size tests
 turn on."
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

TestCase subclass: #TProtocolContainerSizeLimitTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TProtocolContainerSizeLimitTest methodsFor: 'support'!
defaultLimit
	"The default enforced by TProtocol>>containerSizeLimit."
	^ 16384000! !

!TProtocolContainerSizeLimitTest methodsFor: 'support'!
i32: anInteger
	"anInteger as four big-endian bytes."
	^ ByteArray
		with: ((anInteger bitShift: -24) bitAnd: 16rFF)
		with: ((anInteger bitShift: -16) bitAnd: 16rFF)
		with: ((anInteger bitShift: -8) bitAnd: 16rFF)
		with: (anInteger bitAnd: 16rFF)! !

!TProtocolContainerSizeLimitTest methodsFor: 'support'!
listHeaderElem: etype size: sz
	"A binary-protocol list/set header: one element-type byte, then the declared
	 size as four bytes. Five bytes, the whole container header."
	^ (ByteArray with: etype), (self i32: sz)! !

!TProtocolContainerSizeLimitTest methodsFor: 'support'!
mapHeaderKey: kt value: vt size: sz
	^ (ByteArray with: kt with: vt), (self i32: sz)! !

!TProtocolContainerSizeLimitTest methodsFor: 'support'!
protocolOn: aByteArray
	^ TBinaryProtocol new transport: (TCountingTransport on: aByteArray)! !

!TProtocolContainerSizeLimitTest methodsFor: 'support'!
codeFrom: aBlock
	"The TProtocolError code aBlock signals, or nil if it signals none."
	^ [aBlock value. nil] on: TProtocolError do: [:e | e code]! !

!TProtocolContainerSizeLimitTest methodsFor: 'support'!
raisedBy: aBlock
	"The exception aBlock signals (any Error), or nil if it signals none."
	^ [aBlock value. nil] on: Error do: [:e | e]! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testDefaultLimitIsTheDocumentedValue
	self
		assert: TBinaryProtocol new containerSizeLimit = self defaultLimit
		description: 'the default container size limit should be 16384000'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testOverLimitListSignalsSizeLimit
	"sizeLimit, not negativeSize -- the two must stay distinguishable."
	| proto |
	proto := self protocolOn: (self listHeaderElem: TType byte size: 16r7FFFFFFF).
	self
		assert: (self codeFrom: [proto readListBegin]) = TProtocolError sizeLimit
		description: 'an over-limit list size should signal sizeLimit'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testJustOverTheDefaultLimitIsRejected
	"The boundary, from above."
	| proto |
	proto := self protocolOn: (self listHeaderElem: TType byte size: self defaultLimit + 1).
	self
		assert: (self codeFrom: [proto readListBegin]) = TProtocolError sizeLimit
		description: 'limit + 1 should be rejected'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testOverLimitSkipReadsOnlyTheHeader
	"The fix in one assertion. A complete, well-formed list of limit + 1 one-byte
	 elements is supplied, so an unbounded skip really does consume all of it:
	 five bytes of header plus eleven of payload, sixteen in all. Bounded, the
	 payload read is never issued -- five bytes and nothing more."
	| proto |
	proto := self protocolOn:
		(self listHeaderElem: TType byte size: 11), #[1 2 3 4 5 6 7 8 9 10 11].
	proto containerSizeLimit: 10.
	self
		assert: (self codeFrom: [proto skip: TType list]) = TProtocolError sizeLimit
		description: 'an over-limit list should be refused when skipped'.
	self
		assert: proto transport requested = 5
		description: 'the element reads must never be issued'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testWithinLimitListRoundTrips
	"A legitimate container is unaffected: skipped in full, five bytes of header
	 plus two one-byte elements."
	| proto |
	proto := self protocolOn: (self listHeaderElem: TType byte size: 2), #[65 66].
	self
		assert: (self codeFrom: [proto skip: TType list]) isNil
		description: 'a short list should skip without error'.
	self
		assert: proto transport requested = 7
		description: 'a short list should cost header + elements and no more'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testNegativeSizeStillSignalsNegativeSize
	"Pre-existing behaviour, guarded: the negative check runs before the size
	 check, so a negative count keeps its own error code."
	| proto |
	proto := self protocolOn: (self listHeaderElem: TType byte size: -1).
	self
		assert: (self codeFrom: [proto readListBegin]) = TProtocolError negativeSize
		description: 'a negative count should still signal negativeSize'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testConfiguredLimitIsHonoured
	"The bound is caller-settable, so a deployment moving larger containers has a
	 route -- and a smaller one is enforced just as the default is."
	| proto |
	proto := self protocolOn: (self listHeaderElem: TType byte size: 11).
	proto containerSizeLimit: 10.
	self
		assert: (self codeFrom: [proto readListBegin]) = TProtocolError sizeLimit
		description: 'a configured limit should reject limit + 1'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testNonPositiveLimitIsRejected
	"A zero or negative limit would silently disable the bound. The positive case
	 is asserted first so that this test cannot pass merely because the accessor
	 is absent -- a bare should:raise:Error is satisfied by a doesNotUnderstand."
	| proto |
	proto := TBinaryProtocol new.
	proto containerSizeLimit: 1024.
	self
		assert: proto containerSizeLimit = 1024
		description: 'a positive limit should be accepted'.
	self should: [proto containerSizeLimit: 0] raise: Error.
	self should: [proto containerSizeLimit: -1] raise: Error! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testStopElementListIsRefused
	"A list whose element type is stop (0) reads nothing per element. Refused with
	 a TProtocolError, so skip cannot iterate the declared count for free."
	| proto raised |
	proto := self protocolOn: (self listHeaderElem: TType stop size: 1000000).
	raised := self raisedBy: [proto skip: TType list].
	self
		assert: (raised isKindOf: TError)
		description: 'skipping a stop-element list should signal a TError'.
	self
		assert: raised code = TProtocolError invalidData
		description: 'and the code should be invalidData'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testStopKeyMapIsRefused
	"The map key type reaches the same guard."
	| proto raised |
	proto := self protocolOn:
		(self mapHeaderKey: TType stop value: TType stop size: 1000000).
	raised := self raisedBy: [proto skip: TType map].
	self
		assert: (raised isKindOf: TError)
		description: 'skipping a stop-key map should signal a TError'.
	self
		assert: raised code = TProtocolError invalidData
		description: 'and the code should be invalidData'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testStructOfStopListsIsRefusedEarly
	"Breadth: a struct of sibling stop-element lists. The recursion guard only
	 sees depth two or three; without refusing the element type the cost would be
	 fields times count. Refused on the first element of the first field instead."
	| s proto raised |
	s := WriteStream on: ByteArray new.
	1 to: 50 do: [:i |
		s nextPut: TType list.
		s nextPut: 0; nextPut: i.
		s nextPut: TType stop.
		s nextPutAll: (self i32: 200000)].
	s nextPut: TType stop.
	proto := self protocolOn: s contents.
	raised := self raisedBy: [proto skip: TType struct].
	self
		assert: (raised isKindOf: TError)
		description: 'a struct of stop-element lists should be refused'.
	self
		assert: raised code = TProtocolError invalidData
		description: 'and the code should be invalidData'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testUnknownTypeSignalsTError
	"An unrecognised wire type is refused with a TProtocolError, so a caller doing
	 on: TError do: catches it -- a bare Error does not belong to that hierarchy."
	| proto raised |
	proto := self protocolOn: #[].
	raised := self raisedBy: [proto skip: 99].
	self
		assert: (raised isKindOf: TError)
		description: 'an unknown type should signal a TError'.
	self
		assert: raised code = TProtocolError invalidData
		description: 'and the code should be invalidData'! !

!TProtocolContainerSizeLimitTest methodsFor: 'tests'!
testVoidTypeSignalsTError
	"void (1) carries no payload and is not a field or element type to skip; like
	 any unrecognised type it is refused with a TProtocolError."
	| proto raised |
	proto := self protocolOn: #[].
	raised := self raisedBy: [proto skip: TType void].
	self
		assert: (raised isKindOf: TError)
		description: 'the void type should signal a TError'.
	self
		assert: raised code = TProtocolError invalidData
		description: 'and the code should be invalidData'! !
