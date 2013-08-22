/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#import "TProtocolDecorator.h"
#import "TObjective-C.h"

@implementation TProtocolDecorator

- (id) initWithProtocol: (id <TProtocol>) protocol
{
    self = [super init];
    if (self) {
        mConcreteProtocol = [protocol retain_stub];
    }
    return self;
}

- (id <TTransport>) transport
{
    return [mConcreteProtocol transport];
}

- (void) readMessageBeginReturningName: (NSString **) name
                                  type: (int *) type
                            sequenceID: (int *) sequenceID
{
    [mConcreteProtocol readMessageBeginReturningName:name
                                                type:type
                                          sequenceID:sequenceID];
}

- (void) readMessageEnd
{
    [mConcreteProtocol readMessageEnd];
}

- (void) readStructBeginReturningName: (NSString **) name
{
    [mConcreteProtocol readStructBeginReturningName:name];
}

- (void) readStructEnd
{
    [mConcreteProtocol readStructEnd];
}

- (void) readFieldBeginReturningName: (NSString **) name
                                type: (int *) fieldType
                             fieldID: (int *) fieldID
{
    [mConcreteProtocol readFieldBeginReturningName:name
                                              type:fieldType
                                           fieldID:fieldID];
}
- (void) readFieldEnd
{
    [mConcreteProtocol readFieldEnd];
}

- (NSString *) readString
{
    return [mConcreteProtocol readString];
}

- (BOOL) readBool
{
    return [mConcreteProtocol readBool];
}

- (unsigned char) readByte
{
    return [mConcreteProtocol readByte];
}

- (short) readI16
{
    return [mConcreteProtocol readI16];
}

- (int32_t) readI32
{
    return [mConcreteProtocol readI32];
}

- (int64_t) readI64
{
    return [mConcreteProtocol readI64];
}

- (double) readDouble
{
    return [mConcreteProtocol readDouble];
}

- (NSData *) readBinary
{
    return [mConcreteProtocol readBinary];
}

- (void) readMapBeginReturningKeyType: (int *) keyType
                            valueType: (int *) valueType
                                 size: (int *) size
{
    [mConcreteProtocol readMapBeginReturningKeyType:keyType
                                          valueType:valueType
                                               size:size];
}
- (void) readMapEnd
{
    [mConcreteProtocol readMapEnd];
}


- (void) readSetBeginReturningElementType: (int *) elementType
                                     size: (int *) size
{
    [mConcreteProtocol readSetBeginReturningElementType:elementType
                                                   size:size];
}
- (void) readSetEnd
{
    [mConcreteProtocol readSetEnd];
}

- (void) readListBeginReturningElementType: (int *) elementType
                                      size: (int *) size
{
    [mConcreteProtocol readListBeginReturningElementType:elementType
                                                    size:size];
}
- (void) readListEnd
{
    [mConcreteProtocol readListEnd];
}

- (void) writeMessageBeginWithName: (NSString *) name
                              type: (int) messageType
                        sequenceID: (int) sequenceID
{
    [mConcreteProtocol writeMessageBeginWithName:name
                                            type:messageType
                                      sequenceID:sequenceID];
}
- (void) writeMessageEnd
{
    [mConcreteProtocol writeMessageEnd];
}

- (void) writeStructBeginWithName: (NSString *) name
{
    [mConcreteProtocol writeStructBeginWithName:name];
}
- (void) writeStructEnd
{
    [mConcreteProtocol writeStructEnd];
}

- (void) writeFieldBeginWithName: (NSString *) name
                            type: (int) fieldType
                         fieldID: (int) fieldID
{
    [mConcreteProtocol writeFieldBeginWithName:name
                                          type:fieldType
                                       fieldID:fieldID];
}

- (void) writeI32: (int32_t) value
{
    [mConcreteProtocol writeI32:value];
}

- (void) writeI64: (int64_t) value
{
    [mConcreteProtocol writeI64:value];
}

- (void) writeI16: (short) value
{
    [mConcreteProtocol writeI16:value];
}

- (void) writeByte: (uint8_t) value
{
    [mConcreteProtocol writeByte:value];
}

- (void) writeString: (NSString *) value
{
    [mConcreteProtocol writeString:value];
}

- (void) writeDouble: (double) value
{
    [mConcreteProtocol writeDouble:value];
}

- (void) writeBool: (BOOL) value
{
    [mConcreteProtocol writeBool:value];
}

- (void) writeBinary: (NSData *) data
{
    [mConcreteProtocol writeBinary:data];
}

- (void) writeFieldStop
{
    [mConcreteProtocol writeFieldStop];
}

- (void) writeFieldEnd
{
    [mConcreteProtocol writeFieldEnd];
}

- (void) writeMapBeginWithKeyType: (int) keyType
                        valueType: (int) valueType
                             size: (int) size
{
    [mConcreteProtocol writeMapBeginWithKeyType:keyType
                                      valueType:valueType
                                           size:size];
}
- (void) writeMapEnd
{
    [mConcreteProtocol writeMapEnd];
}

- (void) writeSetBeginWithElementType: (int) elementType
                                 size: (int) size
{
    [mConcreteProtocol writeSetBeginWithElementType:elementType size:size];
}

- (void) writeSetEnd
{
    [mConcreteProtocol writeSetEnd];
}

- (void) writeListBeginWithElementType: (int) elementType
                                  size: (int) size
{
    [mConcreteProtocol writeListBeginWithElementType:elementType size:size];
}

- (void) writeListEnd
{
    [mConcreteProtocol writeListEnd];
}

- (void) dealloc
{
    [mConcreteProtocol release_stub];
    [super dealloc_stub];
}

@end
