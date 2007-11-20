SystemOrganization addCategory: #Thrift!
SystemOrganization addCategory: #'Thrift-Protocol'!
SystemOrganization addCategory: #'Thrift-Test'!
SystemOrganization addCategory: #'Thrift-Transport'!

Error subclass: #TError
	instanceVariableNames: 'code'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift'!

!TError class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:28'!
signalWithCode: anInteger
	self new code: anInteger; signal! !

!TError methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:28'!
code
	^ code! !

!TError methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:28'!
code: anInteger
	code := anInteger! !

TError subclass: #TProtocolError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

!TProtocolError class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 18:39'!
badVersion
	^ 4! !

!TProtocolError class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 18:39'!
invalidData
	^ 1! !

!TProtocolError class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 18:39'!
negativeSize
	^ 2! !

!TProtocolError class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 18:40'!
sizeLimit
	^ 3! !

!TProtocolError class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 18:40'!
unknown
	^ 0! !

TError subclass: #TTransportError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Transport'!

TTransportError subclass: #TTransportClosedError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Transport'!

Error subclass: #Xception
	instanceVariableNames: 'errorCode message'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!Xception methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
errorCode
    ^ errorCode! !

!Xception methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
errorCode: anI32
    errorCode := anI32! !

!Xception methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
message
    ^ message! !

!Xception methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
message: aString
    message := aString! !

Error subclass: #Xception2
	instanceVariableNames: 'errorCode structThing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!Xception2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
errorCode
    ^ errorCode! !

!Xception2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
errorCode: anI32
    errorCode := anI32! !

!Xception2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
structThing
    ^ structThing! !

!Xception2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
structThing: aXtruct
    structThing := aXtruct! !

Object subclass: #Bonk
	instanceVariableNames: 'message type'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!Bonk methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
message
    ^ message! !

!Bonk methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
message: aString
    message := aString! !

!Bonk methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
type
    ^ type! !

!Bonk methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
type: anI32
    type := anI32! !

Object subclass: #EmptyStruct
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

Object subclass: #Insanity
	instanceVariableNames: 'userMap xtructs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!Insanity methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
userMap
    ^ userMap! !

!Insanity methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
userMap: a
    userMap := a! !

!Insanity methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
xtructs
    ^ xtructs! !

!Insanity methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
xtructs: a
    xtructs := a! !

Object subclass: #TClient
	instanceVariableNames: 'iprot oprot seqid remoteSeqid'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift'!

TClient subclass: #SecondServiceClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!SecondServiceClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
blahBlah
    ""
    self sendBlahBlah.
    ^ self recvBlahBlah success 
! !

!SecondServiceClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvBlahBlah
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp118 temp117|
      temp117 := TResult new.
      iprot readStructBegin.
      [temp118 := iprot readFieldBegin.
      temp118 type = TType stop] whileFalse: [|temp119|
        temp118 id = 0 ifTrue: [
          temp119 := true.
          temp117 success: iprot readVoid].
        temp119 ifNil: [iprot skip: temp118 type]].
      oprot readStructEnd.
      temp117] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!SecondServiceClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendBlahBlah
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'blahBlah'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'BlahBlah_args').
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!TClient methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 23:03'!
inProtocol: aProtocol
	iprot := aProtocol.
	oprot ifNil: [oprot := aProtocol]! !

!TClient methodsFor: 'as yet unclassified' stamp: 'pc 10/26/2007 04:28'!
nextSeqid
	^ seqid
		ifNil: [seqid := 0]
		ifNotNil: [seqid := seqid + 1]! !

!TClient methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 22:51'!
outProtocol: aProtocol
	oprot := aProtocol! !

!TClient methodsFor: 'as yet unclassified' stamp: 'pc 10/28/2007 15:32'!
validateRemoteMessage: aMsg
	remoteSeqid
		ifNil: [remoteSeqid := aMsg seqid]
		ifNotNil: 
			[(remoteSeqid + 1) = aMsg seqid ifFalse:
				[TProtocolError signal: 'Bad seqid: ', aMsg seqid asString,
							'; wanted: ', remoteSeqid asString].
			remoteSeqid := aMsg seqid]! !

TClient subclass: #ThriftTestClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestByte
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp7 temp6|
      temp6 := TResult new.
      iprot readStructBegin.
      [temp7 := iprot readFieldBegin.
      temp7 type = TType stop] whileFalse: [|temp8|
        temp7 id = 0 ifTrue: [
          temp8 := true.
          temp6 success: iprot readByte].
        temp8 ifNil: [iprot skip: temp7 type]].
      oprot readStructEnd.
      temp6] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestDouble
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp19 temp18|
      temp18 := TResult new.
      iprot readStructBegin.
      [temp19 := iprot readFieldBegin.
      temp19 type = TType stop] whileFalse: [|temp20|
        temp19 id = 0 ifTrue: [
          temp20 := true.
          temp18 success: iprot readDouble].
        temp20 ifNil: [iprot skip: temp19 type]].
      oprot readStructEnd.
      temp18] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestEnum
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp56 temp55|
      temp55 := TResult new.
      iprot readStructBegin.
      [temp56 := iprot readFieldBegin.
      temp56 type = TType stop] whileFalse: [|temp57|
        temp56 id = 0 ifTrue: [
          temp57 := true.
          temp55 success: iprot readI32].
        temp57 ifNil: [iprot skip: temp56 type]].
      oprot readStructEnd.
      temp55] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestException
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp97 temp96|
      temp96 := TResult new.
      iprot readStructBegin.
      [temp97 := iprot readFieldBegin.
      temp97 type = TType stop] whileFalse: [|temp98|
        temp97 id = 0 ifTrue: [
          temp98 := true.
          temp96 success: iprot readVoid].
        temp97 id = -2 ifTrue: [
          temp98 := true.
          temp96 exception: [|temp100 temp99|
            temp99 := Xception new.
            iprot readStructBegin.
            [temp100 := iprot readFieldBegin.
            temp100 type = TType stop] whileFalse: [|temp101|
              temp100 id = 1 ifTrue: [
                temp101 := true.
                temp99 errorCode: iprot readI32].
              temp100 id = 2 ifTrue: [
                temp101 := true.
                temp99 message: iprot readString].
              temp101 ifNil: [iprot skip: temp100 type]].
            oprot readStructEnd.
            temp99] value].
        temp98 ifNil: [iprot skip: temp97 type]].
      oprot readStructEnd.
      temp96] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestI16
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp10 temp9|
      temp9 := TResult new.
      iprot readStructBegin.
      [temp10 := iprot readFieldBegin.
      temp10 type = TType stop] whileFalse: [|temp11|
        temp10 id = 0 ifTrue: [
          temp11 := true.
          temp9 success: iprot readI16].
        temp11 ifNil: [iprot skip: temp10 type]].
      oprot readStructEnd.
      temp9] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestI32
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp13 temp12|
      temp12 := TResult new.
      iprot readStructBegin.
      [temp13 := iprot readFieldBegin.
      temp13 type = TType stop] whileFalse: [|temp14|
        temp13 id = 0 ifTrue: [
          temp14 := true.
          temp12 success: iprot readI32].
        temp14 ifNil: [iprot skip: temp13 type]].
      oprot readStructEnd.
      temp12] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestI64
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp16 temp15|
      temp15 := TResult new.
      iprot readStructBegin.
      [temp16 := iprot readFieldBegin.
      temp16 type = TType stop] whileFalse: [|temp17|
        temp16 id = 0 ifTrue: [
          temp17 := true.
          temp15 success: iprot readI64].
        temp17 ifNil: [iprot skip: temp16 type]].
      oprot readStructEnd.
      temp15] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestInsanity
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp72 temp71|
      temp71 := TResult new.
      iprot readStructBegin.
      [temp72 := iprot readFieldBegin.
      temp72 type = TType stop] whileFalse: [|temp73|
        temp72 id = 0 ifTrue: [
          temp73 := true.
          temp71 success: [|temp74 temp75| 
            temp74 := iprot readMapBegin.
            temp75 := Dictionary new.
            temp74 size timesRepeat: [
              temp75 at: iprot readI64 put: [|temp76 temp77| 
                temp76 := iprot readMapBegin.
                temp77 := Dictionary new.
                temp76 size timesRepeat: [
                  temp77 at: iprot readI32 put: [|temp79 temp78|
                    temp78 := Insanity new.
                    iprot readStructBegin.
                    [temp79 := iprot readFieldBegin.
                    temp79 type = TType stop] whileFalse: [|temp80|
                      temp79 id = 1 ifTrue: [
                        temp80 := true.
                        temp78 userMap: [|temp81 temp82| 
                          temp81 := iprot readMapBegin.
                          temp82 := Dictionary new.
                          temp81 size timesRepeat: [
                            temp82 at: iprot readI32 put: iprot readI64].
                          iprot readMapEnd.
                          temp82] value].
                      temp79 id = 2 ifTrue: [
                        temp80 := true.
                        temp78 xtructs: [|temp83 temp84| temp83 := iprot readListBegin.
                          temp84 := OrderedCollection new.
                          temp83 size timesRepeat: [
                            temp84 add: [|temp86 temp85|
                              temp85 := Xtruct new.
                              iprot readStructBegin.
                              [temp86 := iprot readFieldBegin.
                              temp86 type = TType stop] whileFalse: [|temp87|
                                temp86 id = 1 ifTrue: [
                                  temp87 := true.
                                  temp85 stringThing: iprot readString].
                                temp86 id = 4 ifTrue: [
                                  temp87 := true.
                                  temp85 byteThing: iprot readByte].
                                temp86 id = 9 ifTrue: [
                                  temp87 := true.
                                  temp85 i32Thing: iprot readI32].
                                temp86 id = 11 ifTrue: [
                                  temp87 := true.
                                  temp85 i64Thing: iprot readI64].
                                temp87 ifNil: [iprot skip: temp86 type]].
                              oprot readStructEnd.
                              temp85] value].
                          iprot readListEnd.
                          temp84] value].
                      temp80 ifNil: [iprot skip: temp79 type]].
                    oprot readStructEnd.
                    temp78] value].
                iprot readMapEnd.
                temp77] value].
            iprot readMapEnd.
            temp75] value].
        temp73 ifNil: [iprot skip: temp72 type]].
      oprot readStructEnd.
      temp71] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestList
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp51 temp50|
      temp50 := TResult new.
      iprot readStructBegin.
      [temp51 := iprot readFieldBegin.
      temp51 type = TType stop] whileFalse: [|temp52|
        temp51 id = 0 ifTrue: [
          temp52 := true.
          temp50 success: [|temp53 temp54| temp53 := iprot readListBegin.
            temp54 := OrderedCollection new.
            temp53 size timesRepeat: [
              temp54 add: iprot readI32].
            iprot readListEnd.
            temp54] value].
        temp52 ifNil: [iprot skip: temp51 type]].
      oprot readStructEnd.
      temp50] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestMap
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp39 temp38|
      temp38 := TResult new.
      iprot readStructBegin.
      [temp39 := iprot readFieldBegin.
      temp39 type = TType stop] whileFalse: [|temp40|
        temp39 id = 0 ifTrue: [
          temp40 := true.
          temp38 success: [|temp41 temp42| 
            temp41 := iprot readMapBegin.
            temp42 := Dictionary new.
            temp41 size timesRepeat: [
              temp42 at: iprot readI32 put: iprot readI32].
            iprot readMapEnd.
            temp42] value].
        temp40 ifNil: [iprot skip: temp39 type]].
      oprot readStructEnd.
      temp38] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestMapMap
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp62 temp61|
      temp61 := TResult new.
      iprot readStructBegin.
      [temp62 := iprot readFieldBegin.
      temp62 type = TType stop] whileFalse: [|temp63|
        temp62 id = 0 ifTrue: [
          temp63 := true.
          temp61 success: [|temp64 temp65| 
            temp64 := iprot readMapBegin.
            temp65 := Dictionary new.
            temp64 size timesRepeat: [
              temp65 at: iprot readI32 put: [|temp66 temp67| 
                temp66 := iprot readMapBegin.
                temp67 := Dictionary new.
                temp66 size timesRepeat: [
                  temp67 at: iprot readI32 put: iprot readI32].
                iprot readMapEnd.
                temp67] value].
            iprot readMapEnd.
            temp65] value].
        temp63 ifNil: [iprot skip: temp62 type]].
      oprot readStructEnd.
      temp61] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestMulti
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp91 temp90|
      temp90 := TResult new.
      iprot readStructBegin.
      [temp91 := iprot readFieldBegin.
      temp91 type = TType stop] whileFalse: [|temp92|
        temp91 id = 0 ifTrue: [
          temp92 := true.
          temp90 success: [|temp94 temp93|
            temp93 := Xtruct new.
            iprot readStructBegin.
            [temp94 := iprot readFieldBegin.
            temp94 type = TType stop] whileFalse: [|temp95|
              temp94 id = 1 ifTrue: [
                temp95 := true.
                temp93 stringThing: iprot readString].
              temp94 id = 4 ifTrue: [
                temp95 := true.
                temp93 byteThing: iprot readByte].
              temp94 id = 9 ifTrue: [
                temp95 := true.
                temp93 i32Thing: iprot readI32].
              temp94 id = 11 ifTrue: [
                temp95 := true.
                temp93 i64Thing: iprot readI64].
              temp95 ifNil: [iprot skip: temp94 type]].
            oprot readStructEnd.
            temp93] value].
        temp92 ifNil: [iprot skip: temp91 type]].
      oprot readStructEnd.
      temp90] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestMultiException
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp103 temp102|
      temp102 := TResult new.
      iprot readStructBegin.
      [temp103 := iprot readFieldBegin.
      temp103 type = TType stop] whileFalse: [|temp104|
        temp103 id = 0 ifTrue: [
          temp104 := true.
          temp102 success: [|temp106 temp105|
            temp105 := Xtruct new.
            iprot readStructBegin.
            [temp106 := iprot readFieldBegin.
            temp106 type = TType stop] whileFalse: [|temp107|
              temp106 id = 1 ifTrue: [
                temp107 := true.
                temp105 stringThing: iprot readString].
              temp106 id = 4 ifTrue: [
                temp107 := true.
                temp105 byteThing: iprot readByte].
              temp106 id = 9 ifTrue: [
                temp107 := true.
                temp105 i32Thing: iprot readI32].
              temp106 id = 11 ifTrue: [
                temp107 := true.
                temp105 i64Thing: iprot readI64].
              temp107 ifNil: [iprot skip: temp106 type]].
            oprot readStructEnd.
            temp105] value].
        temp103 id = -3 ifTrue: [
          temp104 := true.
          temp102 exception: [|temp109 temp108|
            temp108 := Xception new.
            iprot readStructBegin.
            [temp109 := iprot readFieldBegin.
            temp109 type = TType stop] whileFalse: [|temp110|
              temp109 id = 1 ifTrue: [
                temp110 := true.
                temp108 errorCode: iprot readI32].
              temp109 id = 2 ifTrue: [
                temp110 := true.
                temp108 message: iprot readString].
              temp110 ifNil: [iprot skip: temp109 type]].
            oprot readStructEnd.
            temp108] value].
        temp103 id = -4 ifTrue: [
          temp104 := true.
          temp102 exception: [|temp112 temp111|
            temp111 := Xception2 new.
            iprot readStructBegin.
            [temp112 := iprot readFieldBegin.
            temp112 type = TType stop] whileFalse: [|temp113|
              temp112 id = 1 ifTrue: [
                temp113 := true.
                temp111 errorCode: iprot readI32].
              temp112 id = 2 ifTrue: [
                temp113 := true.
                temp111 structThing: [|temp115 temp114|
                  temp114 := Xtruct new.
                  iprot readStructBegin.
                  [temp115 := iprot readFieldBegin.
                  temp115 type = TType stop] whileFalse: [|temp116|
                    temp115 id = 1 ifTrue: [
                      temp116 := true.
                      temp114 stringThing: iprot readString].
                    temp115 id = 4 ifTrue: [
                      temp116 := true.
                      temp114 byteThing: iprot readByte].
                    temp115 id = 9 ifTrue: [
                      temp116 := true.
                      temp114 i32Thing: iprot readI32].
                    temp115 id = 11 ifTrue: [
                      temp116 := true.
                      temp114 i64Thing: iprot readI64].
                    temp116 ifNil: [iprot skip: temp115 type]].
                  oprot readStructEnd.
                  temp114] value].
              temp113 ifNil: [iprot skip: temp112 type]].
            oprot readStructEnd.
            temp111] value].
        temp104 ifNil: [iprot skip: temp103 type]].
      oprot readStructEnd.
      temp102] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestNest
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp28 temp27|
      temp27 := TResult new.
      iprot readStructBegin.
      [temp28 := iprot readFieldBegin.
      temp28 type = TType stop] whileFalse: [|temp29|
        temp28 id = 0 ifTrue: [
          temp29 := true.
          temp27 success: [|temp31 temp30|
            temp30 := Xtruct2 new.
            iprot readStructBegin.
            [temp31 := iprot readFieldBegin.
            temp31 type = TType stop] whileFalse: [|temp32|
              temp31 id = 1 ifTrue: [
                temp32 := true.
                temp30 byteThing: iprot readByte].
              temp31 id = 2 ifTrue: [
                temp32 := true.
                temp30 structThing: [|temp34 temp33|
                  temp33 := Xtruct new.
                  iprot readStructBegin.
                  [temp34 := iprot readFieldBegin.
                  temp34 type = TType stop] whileFalse: [|temp35|
                    temp34 id = 1 ifTrue: [
                      temp35 := true.
                      temp33 stringThing: iprot readString].
                    temp34 id = 4 ifTrue: [
                      temp35 := true.
                      temp33 byteThing: iprot readByte].
                    temp34 id = 9 ifTrue: [
                      temp35 := true.
                      temp33 i32Thing: iprot readI32].
                    temp34 id = 11 ifTrue: [
                      temp35 := true.
                      temp33 i64Thing: iprot readI64].
                    temp35 ifNil: [iprot skip: temp34 type]].
                  oprot readStructEnd.
                  temp33] value].
              temp31 id = 3 ifTrue: [
                temp32 := true.
                temp30 i32Thing: iprot readI32].
              temp32 ifNil: [iprot skip: temp31 type]].
            oprot readStructEnd.
            temp30] value].
        temp29 ifNil: [iprot skip: temp28 type]].
      oprot readStructEnd.
      temp27] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestSet
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp45 temp44|
      temp44 := TResult new.
      iprot readStructBegin.
      [temp45 := iprot readFieldBegin.
      temp45 type = TType stop] whileFalse: [|temp46|
        temp45 id = 0 ifTrue: [
          temp46 := true.
          temp44 success: [|temp47 temp48| temp47 := iprot readSetBegin.
            temp48 := Set new.
            temp47 size timesRepeat: [
              temp48 add: iprot readI32].
            iprot readSetEnd.
            temp48] value].
        temp46 ifNil: [iprot skip: temp45 type]].
      oprot readStructEnd.
      temp44] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestString
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp4 temp3|
      temp3 := TResult new.
      iprot readStructBegin.
      [temp4 := iprot readFieldBegin.
      temp4 type = TType stop] whileFalse: [|temp5|
        temp4 id = 0 ifTrue: [
          temp5 := true.
          temp3 success: iprot readString].
        temp5 ifNil: [iprot skip: temp4 type]].
      oprot readStructEnd.
      temp3] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestStruct
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp22 temp21|
      temp21 := TResult new.
      iprot readStructBegin.
      [temp22 := iprot readFieldBegin.
      temp22 type = TType stop] whileFalse: [|temp23|
        temp22 id = 0 ifTrue: [
          temp23 := true.
          temp21 success: [|temp25 temp24|
            temp24 := Xtruct new.
            iprot readStructBegin.
            [temp25 := iprot readFieldBegin.
            temp25 type = TType stop] whileFalse: [|temp26|
              temp25 id = 1 ifTrue: [
                temp26 := true.
                temp24 stringThing: iprot readString].
              temp25 id = 4 ifTrue: [
                temp26 := true.
                temp24 byteThing: iprot readByte].
              temp25 id = 9 ifTrue: [
                temp26 := true.
                temp24 i32Thing: iprot readI32].
              temp25 id = 11 ifTrue: [
                temp26 := true.
                temp24 i64Thing: iprot readI64].
              temp26 ifNil: [iprot skip: temp25 type]].
            oprot readStructEnd.
            temp24] value].
        temp23 ifNil: [iprot skip: temp22 type]].
      oprot readStructEnd.
      temp21] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestTypedef
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp59 temp58|
      temp58 := TResult new.
      iprot readStructBegin.
      [temp59 := iprot readFieldBegin.
      temp59 type = TType stop] whileFalse: [|temp60|
        temp59 id = 0 ifTrue: [
          temp60 := true.
          temp58 success: iprot readI64].
        temp60 ifNil: [iprot skip: temp59 type]].
      oprot readStructEnd.
      temp58] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
recvTestVoid
    | f msg res | 
    msg := oprot readMessageBegin.
    self validateRemoteMessage: msg.
    res := [|temp1 temp0|
      temp0 := TResult new.
      iprot readStructBegin.
      [temp1 := iprot readFieldBegin.
      temp1 type = TType stop] whileFalse: [|temp2|
        temp1 id = 0 ifTrue: [
          temp2 := true.
          temp0 success: iprot readVoid].
        temp2 ifNil: [iprot skip: temp1 type]].
      oprot readStructEnd.
      temp0] value.
    oprot readMessageEnd.
    oprot transport flush.
    res exception ifNotNil: [res exception signal].
    ^ res! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestByteThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testByte'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestByte_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType byte; id: 1).
    iprot writeByte: thing asInteger.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestDoubleThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testDouble'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestDouble_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType double; id: 1).
    iprot writeDouble: thing asFloat.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestEnumThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testEnum'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestEnum_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType i32; id: 1).
    iprot writeI32: thing.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestExceptionArg: arg
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testException'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestException_args').
    oprot writeFieldBegin: (TField new name: 'arg'; type: TType string; id: -1).
    iprot writeString: arg.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestI16Thing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testI16'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestI16_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType i16; id: 1).
    iprot writeI16: thing asInteger.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestI32Thing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testI32'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestI32_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType i32; id: 1).
    iprot writeI32: thing asInteger.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestI64Thing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testI64'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestI64_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType i64; id: 1).
    iprot writeI64: thing asInteger.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestInsanityArgument: argument
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testInsanity'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestInsanity_args').
    oprot writeFieldBegin: (TField new name: 'argument'; type: TType struct; id: 1).
    [oprot writeStructBegin: (TStruct new name: 'Insanity').
      oprot writeFieldBegin: (TField new name: 'userMap'; type: TType map; id: 1).
      [oprot writeMapBegin: (TMap new keyType: TType i32; valueType: TType i64; size: argument userMap size).
        argument userMap keysAndValuesDo: [:temp68 :temp69 |
          iprot writeI32: temp68.
          iprot writeI64: temp69 asInteger].
        oprot writeMapEnd] value.
      oprot writeFieldEnd.
      oprot writeFieldBegin: (TField new name: 'xtructs'; type: TType list; id: 2).
      [oprot writeListBegin: (TList new elemType: TType struct; size: argument xtructs size).
        argument xtructs do: [:temp70|
          [oprot writeStructBegin: (TStruct new name: 'Xtruct').
            oprot writeFieldBegin: (TField new name: 'string_thing'; type: TType string; id: 1).
            iprot writeString: temp70 stringThing.
            oprot writeFieldEnd.
            oprot writeFieldBegin: (TField new name: 'byte_thing'; type: TType byte; id: 4).
            iprot writeByte: temp70 byteThing asInteger.
            oprot writeFieldEnd.
            oprot writeFieldBegin: (TField new name: 'i32_thing'; type: TType i32; id: 9).
            iprot writeI32: temp70 i32Thing asInteger.
            oprot writeFieldEnd.
            oprot writeFieldBegin: (TField new name: 'i64_thing'; type: TType i64; id: 11).
            iprot writeI64: temp70 i64Thing asInteger.
            oprot writeFieldEnd.
            oprot writeFieldStop; writeStructEnd] value
].
        oprot writeListEnd] value.
      oprot writeFieldEnd.
      oprot writeFieldStop; writeStructEnd] value.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestListThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testList'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestList_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType list; id: 1).
    [oprot writeListBegin: (TList new elemType: TType i32; size: thing size).
      thing do: [:temp49|
        iprot writeI32: temp49 asInteger
].
      oprot writeListEnd] value.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestMapMapHello: hello
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testMapMap'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestMapMap_args').
    oprot writeFieldBegin: (TField new name: 'hello'; type: TType i32; id: 1).
    iprot writeI32: hello asInteger.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestMapThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testMap'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestMap_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType map; id: 1).
    [oprot writeMapBegin: (TMap new keyType: TType i32; valueType: TType i32; size: thing size).
      thing keysAndValuesDo: [:temp36 :temp37 |
        iprot writeI32: temp36 asInteger.
        iprot writeI32: temp37 asInteger].
      oprot writeMapEnd] value.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestMultiArg0: arg0 arg1: arg1 arg2: arg2 arg3: arg3 arg4: arg4 arg5: arg5
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testMulti'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestMulti_args').
    oprot writeFieldBegin: (TField new name: 'arg0'; type: TType byte; id: -1).
    iprot writeByte: arg0 asInteger.
    oprot writeFieldEnd.
    oprot writeFieldBegin: (TField new name: 'arg1'; type: TType i32; id: -2).
    iprot writeI32: arg1 asInteger.
    oprot writeFieldEnd.
    oprot writeFieldBegin: (TField new name: 'arg2'; type: TType i64; id: -3).
    iprot writeI64: arg2 asInteger.
    oprot writeFieldEnd.
    oprot writeFieldBegin: (TField new name: 'arg3'; type: TType map; id: -4).
    [oprot writeMapBegin: (TMap new keyType: TType i16; valueType: TType string; size: arg3 size).
      arg3 keysAndValuesDo: [:temp88 :temp89 |
        iprot writeI16: temp88 asInteger.
        iprot writeString: temp89].
      oprot writeMapEnd] value.
    oprot writeFieldEnd.
    oprot writeFieldBegin: (TField new name: 'arg4'; type: TType i32; id: -5).
    iprot writeI32: arg4.
    oprot writeFieldEnd.
    oprot writeFieldBegin: (TField new name: 'arg5'; type: TType i64; id: -6).
    iprot writeI64: arg5 asInteger.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestMultiExceptionArg0: arg0 arg1: arg1
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testMultiException'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestMultiException_args').
    oprot writeFieldBegin: (TField new name: 'arg0'; type: TType string; id: -1).
    iprot writeString: arg0.
    oprot writeFieldEnd.
    oprot writeFieldBegin: (TField new name: 'arg1'; type: TType string; id: -2).
    iprot writeString: arg1.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestNestThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testNest'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestNest_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType struct; id: 1).
    [oprot writeStructBegin: (TStruct new name: 'Xtruct2').
      oprot writeFieldBegin: (TField new name: 'byte_thing'; type: TType byte; id: 1).
      iprot writeByte: thing byteThing asInteger.
      oprot writeFieldEnd.
      oprot writeFieldBegin: (TField new name: 'struct_thing'; type: TType struct; id: 2).
      [oprot writeStructBegin: (TStruct new name: 'Xtruct').
        oprot writeFieldBegin: (TField new name: 'string_thing'; type: TType string; id: 1).
        iprot writeString: thing structThing stringThing.
        oprot writeFieldEnd.
        oprot writeFieldBegin: (TField new name: 'byte_thing'; type: TType byte; id: 4).
        iprot writeByte: thing structThing byteThing asInteger.
        oprot writeFieldEnd.
        oprot writeFieldBegin: (TField new name: 'i32_thing'; type: TType i32; id: 9).
        iprot writeI32: thing structThing i32Thing asInteger.
        oprot writeFieldEnd.
        oprot writeFieldBegin: (TField new name: 'i64_thing'; type: TType i64; id: 11).
        iprot writeI64: thing structThing i64Thing asInteger.
        oprot writeFieldEnd.
        oprot writeFieldStop; writeStructEnd] value.
      oprot writeFieldEnd.
      oprot writeFieldBegin: (TField new name: 'i32_thing'; type: TType i32; id: 3).
      iprot writeI32: thing i32Thing asInteger.
      oprot writeFieldEnd.
      oprot writeFieldStop; writeStructEnd] value.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestSetThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testSet'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestSet_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType set; id: 1).
    [oprot writeSetBegin: (TSet new elemType: TType i32; size: thing size).
      thing do: [:temp43|
        iprot writeI32: temp43 asInteger
].
      oprot writeSetEnd] value.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestStringThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testString'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestString_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType string; id: 1).
    iprot writeString: thing.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestStructThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testStruct'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestStruct_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType struct; id: 1).
    [oprot writeStructBegin: (TStruct new name: 'Xtruct').
      oprot writeFieldBegin: (TField new name: 'string_thing'; type: TType string; id: 1).
      iprot writeString: thing stringThing.
      oprot writeFieldEnd.
      oprot writeFieldBegin: (TField new name: 'byte_thing'; type: TType byte; id: 4).
      iprot writeByte: thing byteThing asInteger.
      oprot writeFieldEnd.
      oprot writeFieldBegin: (TField new name: 'i32_thing'; type: TType i32; id: 9).
      iprot writeI32: thing i32Thing asInteger.
      oprot writeFieldEnd.
      oprot writeFieldBegin: (TField new name: 'i64_thing'; type: TType i64; id: 11).
      iprot writeI64: thing i64Thing asInteger.
      oprot writeFieldEnd.
      oprot writeFieldStop; writeStructEnd] value.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestTypedefThing: thing
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testTypedef'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestTypedef_args').
    oprot writeFieldBegin: (TField new name: 'thing'; type: TType i64; id: 1).
    iprot writeI64: thing asInteger.
    oprot writeFieldEnd.
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
sendTestVoid
    oprot writeMessageBegin:
      (TCallMessage new
        name: 'testVoid'; 
        seqid: self nextSeqid).
    oprot writeStructBegin: (TStruct new name: 'TestVoid_args').
    oprot writeFieldStop; writeStructEnd; writeMessageEnd.
    oprot transport flush! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testByteThing: thing
    "thing: byte"
    self sendTestByteThing: thing.
    ^ self recvTestByte success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testDoubleThing: thing
    "thing: double"
    self sendTestDoubleThing: thing.
    ^ self recvTestDouble success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testEnumThing: thing
    "thing: Numberz"
    self sendTestEnumThing: thing.
    ^ self recvTestEnum success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testExceptionArg: arg
    "arg: string"
    self sendTestExceptionArg: arg.
    ^ self recvTestException success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testI16Thing: thing
    "thing: i16"
    self sendTestI16Thing: thing.
    ^ self recvTestI16 success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testI32Thing: thing
    "thing: i32"
    self sendTestI32Thing: thing.
    ^ self recvTestI32 success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testI64Thing: thing
    "thing: i64"
    self sendTestI64Thing: thing.
    ^ self recvTestI64 success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testInsanityArgument: argument
    "argument: Insanity"
    self sendTestInsanityArgument: argument.
    ^ self recvTestInsanity success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testListThing: thing
    "thing: "
    self sendTestListThing: thing.
    ^ self recvTestList success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testMapMapHello: hello
    "hello: i32"
    self sendTestMapMapHello: hello.
    ^ self recvTestMapMap success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testMapThing: thing
    "thing: "
    self sendTestMapThing: thing.
    ^ self recvTestMap success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testMultiArg0: arg0 arg1: arg1 arg2: arg2 arg3: arg3 arg4: arg4 arg5: arg5
    "arg0: byte, arg1: i32, arg2: i64, arg3: , arg4: Numberz, arg5: UserId"
    self sendTestMultiArg0: arg0 arg1: arg1 arg2: arg2 arg3: arg3 arg4: arg4 arg5: arg5.
    ^ self recvTestMulti success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testMultiExceptionArg0: arg0 arg1: arg1
    "arg0: string, arg1: string"
    self sendTestMultiExceptionArg0: arg0 arg1: arg1.
    ^ self recvTestMultiException success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testNestThing: thing
    "thing: Xtruct2"
    self sendTestNestThing: thing.
    ^ self recvTestNest success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testSetThing: thing
    "thing: "
    self sendTestSetThing: thing.
    ^ self recvTestSet success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testStringThing: thing
    "thing: string"
    self sendTestStringThing: thing.
    ^ self recvTestString success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testStructThing: thing
    "thing: Xtruct"
    self sendTestStructThing: thing.
    ^ self recvTestStruct success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testTypedefThing: thing
    "thing: UserId"
    self sendTestTypedefThing: thing.
    ^ self recvTestTypedef success 
! !

!ThriftTestClient methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
testVoid
    ""
    self sendTestVoid.
    ^ self recvTestVoid success 
! !

Object subclass: #TField
	instanceVariableNames: 'name type id'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

!TField methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:05'!
id
	^ id ifNil: [0]! !

!TField methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:44'!
id: anInteger
	id := anInteger! !

!TField methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:04'!
name
	^ name ifNil: ['']! !

!TField methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:44'!
name: anObject
	name := anObject! !

!TField methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:05'!
type
	^ type ifNil: [TType stop]! !

!TField methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:44'!
type: anInteger
	type := anInteger! !

Object subclass: #TMessage
	instanceVariableNames: 'name seqid type'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

TMessage subclass: #TCallMessage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

!TCallMessage methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 22:53'!
type
	^ 1! !

!TMessage methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:05'!
name
	^ name ifNil: ['']! !

!TMessage methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:35'!
name: aString
	name := aString! !

!TMessage methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:05'!
seqid
	^ seqid ifNil: [0]! !

!TMessage methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:35'!
seqid: anInteger
	seqid := anInteger! !

!TMessage methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:06'!
type
	^ type ifNil: [0]! !

!TMessage methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:35'!
type: anInteger
	type := anInteger! !

Object subclass: #TProtocol
	instanceVariableNames: 'transport'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

TProtocol subclass: #TBinaryProtocol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 11/1/2007 04:24'!
intFromByteArray: buf
	| vals |
	vals := Array new: buf size.
	1 to: buf size do: [:n | vals at: n put: ((buf at: n) bitShift: (buf size - n) * 8)].
	^ vals sum! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 18:46'!
readBool
	^ self readByte isZero not! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/25/2007 00:02'!
readByte
	^ (self transport read: 1) first! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/28/2007 16:24'!
readDouble
	| val |
	val := Float new: 2.
	^ val basicAt: 1 put: (self readRawInt: 4);
		basicAt: 2 put: (self readRawInt: 4);
		yourself! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 20:02'!
readFieldBegin
	| field |
	field := TField new type: self readByte.
	
	^ field type = TType stop
		ifTrue: [field]
		ifFalse: [field id: self readI16; yourself]! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:15'!
readI16
	^ self readInt: 2! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:20'!
readI32
	^ self readInt: 4! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:20'!
readI64
	^ self readInt: 8! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 11/1/2007 02:35'!
readInt: size
	| buf val |
	buf := transport read: size.
	val := self intFromByteArray: buf.
	^ buf first > 16r7F
		ifTrue: [self unsignedInt: val size: size]
		ifFalse: [val]! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:57'!
readListBegin
	^ TList new
		elemType: self readByte;
		size: self readI32! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:58'!
readMapBegin
	^ TMap new
		keyType: self readByte;
		valueType: self readByte;
		size: self readI32! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 11/1/2007 04:22'!
readMessageBegin
	| version |
	version := self readI32.
	
	(version bitAnd: self versionMask) = self version1
		ifFalse: [TProtocolError signalWithCode: TProtocolError badVersion].
		
	^ TMessage new
		type: (version bitAnd: 16r000000FF);
		name: self readString;
		seqid: self readI32! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/28/2007 16:24'!
readRawInt: size
	^ self intFromByteArray: (transport read: size)! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 11/1/2007 00:59'!
readSetBegin
	"element type, size"
	^ TSet new
		elemType: self readByte;
		size: self readI32! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 10/26/2007 04:48'!
readString
	^ (transport read: self readI32) asString! !

!TBinaryProtocol methodsFor: 'reading' stamp: 'pc 11/1/2007 04:22'!
unsignedInt: val size: size
	^ 0 - ((val - 1) bitXor: ((2 raisedTo: (size * 8)) - 1))! !

!TBinaryProtocol methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 22:13'!
version1
	^ 16r80010000 ! !

!TBinaryProtocol methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 18:01'!
versionMask
	^ 16rFFFF0000! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 18:35'!
write: aString
	transport write: aString! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:23'!
writeBool: bool
	bool ifTrue: [self writeByte: 1]
		ifFalse: [self writeByte: 0]! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/26/2007 09:31'!
writeByte: aNumber
	aNumber > 16rFF ifTrue: [TError signal: 'writeByte too big'].
	transport write: (Array with: aNumber)! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/28/2007 16:16'!
writeDouble: aDouble
	self writeI32: (aDouble basicAt: 1);
		writeI32: (aDouble basicAt: 2)! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:56'!
writeField: aField
	self writeByte: aField type;
		writeI16: aField id! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/25/2007 00:01'!
writeFieldBegin: aField
	self writeByte: aField type.
	self writeI16: aField id! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 18:04'!
writeFieldStop
	self writeByte: TType stop! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 11/1/2007 02:06'!
writeI16: i16
	self writeInt: i16 size: 2! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 11/1/2007 02:06'!
writeI32: i32
	self writeInt: i32 size: 4! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 11/1/2007 02:06'!
writeI64: i64
	self writeInt: i64 size: 8! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 11/1/2007 04:23'!
writeInt: val size: size
	1 to: size do: [:n | self writeByte: ((val bitShift: (size negated + n) * 8) bitAnd: 16rFF)]! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 11/1/2007 00:48'!
writeListBegin: aList
	self writeByte: aList elemType; writeI32: aList size! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:55'!
writeMapBegin: aMap
	self writeByte: aMap keyType;
		writeByte: aMap valueType;
		writeI32: aMap size! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 20:36'!
writeMessageBegin: msg
	self writeI32: (self version1 bitOr: msg type);
		writeString: msg name;
		writeI32: msg seqid! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 11/1/2007 00:56'!
writeSetBegin: aSet
	self writeByte: aSet elemType; writeI32: aSet size! !

!TBinaryProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 18:35'!
writeString: aString
	self writeI32: aString size;
		write: aString! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readBool! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readByte! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readDouble! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readFieldBegin! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readFieldEnd! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readI16! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readI32! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readI64! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readListBegin! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readListEnd! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readMapBegin! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readMapEnd! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:39'!
readMessageBegin! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:39'!
readMessageEnd! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readSetBegin! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readSetEnd! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/25/2007 16:10'!
readSimpleType: aType
	aType = TType bool ifTrue: [^ self readBool].
	aType = TType byte ifTrue: [^ self readByte].
	aType = TType double ifTrue: [^ self readDouble].
	aType = TType i16 ifTrue: [^ self readI16].
	aType = TType i32 ifTrue: [^ self readI32].
	aType = TType i64 ifTrue: [^ self readI64].
	aType = TType list ifTrue: [^ self readBool].! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readString! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readStructBegin
	! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/24/2007 19:40'!
readStructEnd! !

!TProtocol methodsFor: 'reading' stamp: 'pc 10/26/2007 21:34'!
skip: aType
	aType = TType stop ifTrue: [^ self].
	aType = TType bool ifTrue: [^ self readBool].
	aType = TType byte ifTrue: [^ self readByte].
	aType = TType i16 ifTrue: [^ self readI16].
	aType = TType i32 ifTrue: [^ self readI32].
	aType = TType i64 ifTrue: [^ self readI64].
	aType = TType string ifTrue: [^ self readString].
	aType = TType double ifTrue: [^ self readDouble].
	aType = TType struct ifTrue:
		[| field |
		self readStructBegin.
		[(field := self readFieldBegin) type = TType stop] whileFalse:
			[self skip: field type. self readFieldEnd].
		^ self readStructEnd].
	aType = TType map ifTrue:
		[| map |
		map := self readMapBegin.
		map size timesRepeat: [self skip: map keyType. self skip: map valueType].
		^ self readMapEnd].
	aType = TType list ifTrue:
		[| list |
		list := self readListBegin.
		list size timesRepeat: [self skip: list elemType].
		^ self readListEnd].
	aType = TType set ifTrue:
		[| set |
		set := self readSetBegin.
		set size timesRepeat: [self skip: set elemType].
		^ self readSetEnd].
	
	self error: 'Unknown type'! !

!TProtocol methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 23:02'!
transport
	^ transport! !

!TProtocol methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:28'!
transport: aTransport
	transport := aTransport! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeBool: aBool! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeByte: aByte! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:38'!
writeDouble: aFloat! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:38'!
writeFieldBegin: aField! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeFieldEnd! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeFieldStop! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeI16: i16! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeI32: i32! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeI64: i64! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:39'!
writeListBegin: aList! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeListEnd! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:39'!
writeMapBegin: aMap! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeMapEnd! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:36'!
writeMessageBegin! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:36'!
writeMessageEnd! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:39'!
writeSetBegin: aSet! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeSetEnd! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:38'!
writeString: aString! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:38'!
writeStructBegin: aStruct! !

!TProtocol methodsFor: 'writing' stamp: 'pc 10/24/2007 19:37'!
writeStructEnd! !

Object subclass: #TResult
	instanceVariableNames: 'success oprot iprot exception'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift'!

!TResult methodsFor: 'as yet unclassified' stamp: 'pc 10/26/2007 21:35'!
exception
	^ exception! !

!TResult methodsFor: 'as yet unclassified' stamp: 'pc 10/26/2007 21:35'!
exception: anError
	exception := anError! !

!TResult methodsFor: 'as yet unclassified' stamp: 'pc 10/26/2007 14:43'!
success
	^ success! !

!TResult methodsFor: 'as yet unclassified' stamp: 'pc 10/26/2007 14:43'!
success: anObject
	success := anObject! !

Object subclass: #TSizedObject
	instanceVariableNames: 'size'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

TSizedObject subclass: #TList
	instanceVariableNames: 'elemType'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

!TList methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:04'!
elemType
	^ elemType ifNil: [TType stop]! !

!TList methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:42'!
elemType: anInteger
	elemType := anInteger! !

TList subclass: #TSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

TSizedObject subclass: #TMap
	instanceVariableNames: 'keyType valueType'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

!TMap methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:04'!
keyType
	^ keyType ifNil: [TType stop]! !

!TMap methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:45'!
keyType: anInteger
	keyType := anInteger! !

!TMap methodsFor: 'accessing' stamp: 'pc 10/24/2007 20:04'!
valueType
	^ valueType ifNil: [TType stop]! !

!TMap methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:45'!
valueType: anInteger
	valueType := anInteger! !

!TSizedObject methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 20:03'!
size
	^ size ifNil: [0]! !

!TSizedObject methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 20:06'!
size: anInteger
	size := anInteger! !

Object subclass: #TSocket
	instanceVariableNames: 'host port stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Transport'!

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 22:34'!
close
	self isOpen ifTrue: [stream close]! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 22:23'!
connect
	^ (self socketStream openConnectionToHost:
		(NetNameResolver addressForName: host) port: port)
			timeout: 180;
			binary;
			yourself! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 20:35'!
flush
	stream flush! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:08'!
host: aString
	host := aString! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 20:34'!
isOpen
	^ stream isNil not
		and: [stream socket isConnected]
		and: [stream socket isOtherEndClosed not]! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 22:22'!
open
	stream := self connect! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:09'!
port: anInteger
	port := anInteger! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:17'!
read: size
	| data |
	[data := stream next: size.
	data isEmpty ifTrue: [TTransportError signal: 'Could not read ', size asString, ' bytes'].
	^ data]
		on: ConnectionClosed
		do: [TTransportClosedError signal]! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 22:18'!
socketStream
	^ Smalltalk at: #FastSocketStream ifAbsent: [SocketStream] ! !

!TSocket methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 22:17'!
write: aCollection
	[stream nextPutAll: aCollection]
		on: ConnectionClosed
		do: [TTransportClosedError signal]! !

Object subclass: #TStruct
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Protocol'!

!TStruct methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:47'!
name
	^ name! !

!TStruct methodsFor: 'accessing' stamp: 'pc 10/24/2007 19:47'!
name: aString
	name := aString! !

Object subclass: #TTest
	instanceVariableNames: 'prot'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!TTest methodsFor: 'as yet unclassified' stamp: 'pc 11/1/2007 04:47'!
protocol: aProtocol
	prot := aProtocol! !

!TTest methodsFor: 'as yet unclassified' stamp: 'pc 11/1/2007 04:49'!
runAll
	| c |
	c := ThriftTestClient new inProtocol: prot.
	c testByteThing: 32.
	c testDoubleThing: -1.0.
	c testEnumThing: 1.
	c testExceptionArg: 'foo'.
	c testI16Thing: 16.
	c testI16Thing: -16.
	c testI32Thing: 32.
	c testI32Thing: -32.
	c testI64Thing: 123.
	c testDoubleThing: 1.2.
	c testStructThing: (Xtruct new byteThing: 1; i32Thing: 2; i64Thing: 3; stringThing: 'foo').
	c testSetThing: (Set new).
	c testListThing: (OrderedCollection new).
	c testEnumThing: 1.
	c testInsanityArgument:
		(Insanity new
			userMap: (Dictionary new at: 1 put: 2; yourself);
		xtructs: (OrderedCollection new)).
	c testMultiArg0: 1 arg1: 2 arg2: 3 arg3: (Dictionary new) arg4: ((ThriftTest enums at: 	'Numberz') at: 'FIVE') arg5: 6. 
	c testExceptionArg: 'Xception'.
	c testMultiExceptionArg0: 'Xception' arg1: 'Xception2'! !

Object subclass: #TTransport
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Transport'!

!TTransport methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:18'!
close
	self subclassResponsibility! !

!TTransport methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:22'!
flush
	self subclassResponsibility! !

!TTransport methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:18'!
isOpen
	self subclassResponsibility! !

!TTransport methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:18'!
open
	self subclassResponsibility! !

!TTransport methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:18'!
read: anInteger
	self subclassResponsibility! !

!TTransport methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:22'!
readAll: anInteger
	^ String streamContents: [:str |
		[str size < anInteger] whileTrue:
			[str nextPutAll: (self read: anInteger - str size)]]! !

!TTransport methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:22'!
write: aString
	self subclassResponsibility! !

Object subclass: #TType
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift'!

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:03'!
bool
	^ 2! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:03'!
byte
	^ 3! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/25/2007 15:55'!
codeOf: aTypeName
	self typeMap do: [:each | each first = aTypeName ifTrue: [^ each second]].
	^ nil! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:03'!
double
	^ 4! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:04'!
i16
	^ 6! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:04'!
i32
	^ 8! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:04'!
i64
	^ 10! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:04'!
list
	^ 15! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:04'!
map
	^ 13! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/25/2007 15:56'!
nameOf: aTypeCode
	self typeMap do: [:each | each second = aTypeCode ifTrue: [^ each first]].
	^ nil! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:04'!
set
	^ 14! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:03'!
stop
	^ 0! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:04'!
string
	^ 11! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:04'!
struct
	^ 12! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/25/2007 15:51'!
typeMap
	^ #((bool 2) (byte 3) (double 4) (i16 6) (i32 8) (i64 10) (list 15)
	   (map 13) (set 15) (stop 0) (string 11) (struct 12) (void 1))! !

!TType class methodsFor: 'as yet unclassified' stamp: 'pc 10/24/2007 17:03'!
void
	^ 1! !

Object subclass: #Xtruct
	instanceVariableNames: 'stringThing byteThing i32Thing i64Thing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!Xtruct methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
byteThing
    ^ byteThing! !

!Xtruct methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
byteThing: aByte
    byteThing := aByte! !

!Xtruct methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
i32Thing
    ^ i32Thing! !

!Xtruct methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
i32Thing: anI32
    i32Thing := anI32! !

!Xtruct methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
i64Thing
    ^ i64Thing! !

!Xtruct methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
i64Thing: anI64
    i64Thing := anI64! !

!Xtruct methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
stringThing
    ^ stringThing! !

!Xtruct methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
stringThing: aString
    stringThing := aString! !

Object subclass: #Xtruct2
	instanceVariableNames: 'byteThing structThing i32Thing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Thrift-Test'!

!Xtruct2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
byteThing
    ^ byteThing! !

!Xtruct2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
byteThing: aByte
    byteThing := aByte! !

!Xtruct2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
i32Thing
    ^ i32Thing! !

!Xtruct2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
i32Thing: anI32
    i32Thing := anI32! !

!Xtruct2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
structThing
    ^ structThing! !

!Xtruct2 methodsFor: 'as yet uncategorized' stamp: 'thrift 11/01/2007 04:43'!
structThing: aXtruct
    structThing := aXtruct! !
