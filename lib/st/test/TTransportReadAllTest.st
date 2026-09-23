'Tests for TTransport>>readAll:.

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

 readAll: asks read: for the bytes it still needs and appends whatever comes
 back, until it has enough. A read: that answers nothing appends nothing, and
 asking an exhausted source again cannot change that, so the loop has no way to
 end.

 The failure mode is a hang, not an exception, which is why the stalled cases
 below run inside valueWithin:onTimeout:. Without it a regression would not
 report a failure at all -- it would sit there until the CI job hit its own
 timeout, which is a much worse way to find out.

 To run (Squeak/Pharo):
   file in thrift.st, then this file, then run the TTransportReadAllTest suite.
   Or: lib/st/test/run-tests.sh
'!

"Answers nothing, ever. Stands in for a transport at end of stream, or one whose
 peer has stopped sending -- the shape any third-party transport can have."
TTransport subclass: #TStalledTransport
	instanceVariableNames: 'callCount'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TStalledTransport methodsFor: 'testing'!
callCount
	^ callCount ifNil: [0]! !

!TStalledTransport methodsFor: 'reading'!
read: anInteger
	callCount := self callCount + 1.
	^ ''! !

"Answers at most chunkSize bytes at a time, so readAll: has to loop to collect
 what it was asked for. This is the case the loop exists for, and it has to keep
 working."
TTransport subclass: #TChunkedTransport
	instanceVariableNames: 'contents pos chunkSize callCount'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TChunkedTransport methodsFor: 'initialization'!
contents: aString chunkSize: anInteger
	contents := aString.
	chunkSize := anInteger.
	pos := 1.
	callCount := 0! !

!TChunkedTransport methodsFor: 'testing'!
callCount
	^ callCount! !

!TChunkedTransport methodsFor: 'reading'!
read: anInteger
	| take out |
	callCount := callCount + 1.
	take := (anInteger min: chunkSize) min: contents size - pos + 1.
	take <= 0 ifTrue: [^ ''].
	out := contents copyFrom: pos to: pos + take - 1.
	pos := pos + take.
	^ out! !

TestCase subclass: #TTransportReadAllTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TTransportReadAllTest methodsFor: 'private'!
within: aBlock
	"Run aBlock, failing rather than hanging if readAll: does not terminate."
	^ aBlock valueWithin: 10 seconds onTimeout: [
		self fail: 'readAll: did not terminate']! !

!TTransportReadAllTest methodsFor: 'tests'!
testStalledTransportSignalsRatherThanSpinning
	| transport |
	transport := TStalledTransport new.
	self within: [
		self should: [transport readAll: 4] raise: TTransportError]! !

!TTransportReadAllTest methodsFor: 'tests'!
testStalledTransportStopsAskingAfterTheFirstEmptyRead
	"Not merely that it stops, but that it stops at once: a read: that answered
	 nothing cannot answer differently for the same request."
	| transport |
	transport := TStalledTransport new.
	self within: [
		[transport readAll: 4] on: TTransportError do: [:e | e return: nil]].
	self assert: transport callCount equals: 1! !

!TTransportReadAllTest methodsFor: 'tests'!
testPartialThenStalledSignals
	"Bytes arrived, then the source dried up. readAll: cannot deliver what it was
	 asked for, and must say so instead of looping."
	| transport |
	transport := TChunkedTransport new contents: 'ab' chunkSize: 1.
	self within: [
		self should: [transport readAll: 4] raise: TTransportError]! !

!TTransportReadAllTest methodsFor: 'tests'!
testChunkedTransportStillAssembles
	"The case the loop exists for."
	| transport |
	transport := TChunkedTransport new contents: 'abcdefgh' chunkSize: 3.
	self within: [
		self assert: (transport readAll: 8) equals: 'abcdefgh'].
	self assert: transport callCount equals: 3! !

!TTransportReadAllTest methodsFor: 'tests'!
testExactlyOneReadWhenTheTransportAnswersEverything
	| transport |
	transport := TChunkedTransport new contents: 'abcd' chunkSize: 99.
	self within: [
		self assert: (transport readAll: 4) equals: 'abcd'].
	self assert: transport callCount equals: 1! !

!TTransportReadAllTest methodsFor: 'tests'!
testZeroBytesNeedsNoRead
	| transport |
	transport := TStalledTransport new.
	self within: [
		self assert: (transport readAll: 0) equals: ''].
	self assert: transport callCount equals: 0! !
