'Tests that a oneway call is sent with message type ONEWAY, not CALL.

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

 A server that is told a call is CALL replies to it; one told ONEWAY does not.
 Sending CALL for a oneway function therefore makes the server write a reply
 nobody reads. The tests drive the generated FireClient (THRIFT-2710) through a
 protocol that records the message it is asked to begin.

 To run (Squeak/Pharo):
   thrift --gen st OnewayTest.thrift
   file in: thrift.st, the generated OnewayTest.st, then this file.
   Run the TProtocolOnewayTest suite. Or: lib/st/test/run-tests.sh
'!

"A transport that discards what is written to it."
TTransport subclass: #TSinkTransport
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TSinkTransport methodsFor: 'transport'!
write: aCollection! !

!TSinkTransport methodsFor: 'transport'!
flush! !

"A binary protocol that remembers the last message it was asked to begin."
TBinaryProtocol subclass: #TRecordingProtocol
	instanceVariableNames: 'lastMessage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TRecordingProtocol methodsFor: 'recording'!
lastMessage
	^ lastMessage! !

!TRecordingProtocol methodsFor: 'recording'!
writeMessageBegin: aMessage
	lastMessage := aMessage.
	^ super writeMessageBegin: aMessage! !

TestCase subclass: #TProtocolOnewayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Tests'!

!TProtocolOnewayTest methodsFor: 'support'!
protocol
	^ TRecordingProtocol new transport: TSinkTransport new; yourself! !

!TProtocolOnewayTest methodsFor: 'tests'!
testOnewayCallIsSentAsOneway
	| proto |
	proto := self protocol.
	(FireClient new inProtocol: proto; yourself) sendFireX: 1.
	self assert: proto lastMessage type = 4.
	self assert: proto lastMessage name = 'fire'! !

!TProtocolOnewayTest methodsFor: 'tests'!
testRegularCallIsStillSentAsCall
	| proto |
	proto := self protocol.
	(FireClient new inProtocol: proto; yourself) sendPingX: 1.
	self assert: proto lastMessage type = 1.
	self assert: proto lastMessage name = 'ping'! !
