//
//  TProtocolDecorator.m
//  TaLiCaiCommunity
//
//  Created by guoyalun on 7/3/14.
//  Copyright (c) 2014 guoyalun. All rights reserved.
//

#import "TProtocolDecorator.h"

@implementation TProtocolDecorator

- (id)initWithProtocol:(id <TProtocol>)protocol
{
    if (self = [super init]) {
        concreteProtocol = protocol;
    }
    return self;
}

- (id <TTransport>) transport
{
    return [concreteProtocol transport];
}

- (void) readMessageBeginReturningName: (NSString **) name
                                  type: (int *) type
                            sequenceID: (int *) sequenceID
{
    [concreteProtocol readMessageBeginReturningName:name type:type sequenceID:sequenceID];
}
- (void) readMessageEnd
{
    [concreteProtocol readMessageEnd];
}

- (void) readStructBeginReturningName: (NSString **) name
{
    [concreteProtocol readStructBeginReturningName:name];
}
- (void) readStructEnd
{
    [concreteProtocol readStructEnd];
}

- (void) readFieldBeginReturningName: (NSString **) name
                                type: (int *) fieldType
                             fieldID: (int *) fieldID
{
    [concreteProtocol readFieldBeginReturningName:name type:fieldType fieldID:fieldID];
}
- (void) readFieldEnd
{
    [concreteProtocol readFieldEnd];
}

- (NSString *) readString
{
    return  [concreteProtocol readString];
}

- (BOOL) readBool
{
    return  [concreteProtocol readBool];
}

- (unsigned char) readByte
{
    return [concreteProtocol readByte];
}

- (short) readI16
{
    return [concreteProtocol readI16];
}

- (int32_t) readI32
{
    return [concreteProtocol readI32];
}

- (int64_t) readI64
{
    return [concreteProtocol readI64];
}

- (double) readDouble
{
    return [concreteProtocol readDouble];
}

- (NSData *) readBinary
{
    return [concreteProtocol readBinary];
}

- (void) readMapBeginReturningKeyType: (int *) keyType
                            valueType: (int *) valueType
                                 size: (int *) size
{
    return [concreteProtocol readMapBeginReturningKeyType:keyType valueType:valueType size:size];
}
- (void) readMapEnd
{
    [concreteProtocol readMapEnd];
}


- (void) readSetBeginReturningElementType: (int *) elementType
                                     size: (int *) size
{
    [concreteProtocol readSetBeginReturningElementType:elementType size:size];
}
- (void) readSetEnd
{
    [concreteProtocol readSetEnd];
}


- (void) readListBeginReturningElementType: (int *) elementType
                                      size: (int *) size
{
    [concreteProtocol readListBeginReturningElementType:elementType size:size];
}
- (void) readListEnd
{
    [concreteProtocol readListEnd];
}


- (void) writeMessageBeginWithName: (NSString *) name
                              type: (int) messageType
                        sequenceID: (int) sequenceID
{
    [concreteProtocol writeMessageBeginWithName:name type:messageType sequenceID:sequenceID];
}
- (void) writeMessageEnd
{
    [concreteProtocol writeMessageEnd];
}

- (void) writeStructBeginWithName: (NSString *) name
{
    [concreteProtocol writeStructBeginWithName:name];
}
- (void) writeStructEnd
{
    [concreteProtocol writeStructEnd];
}

- (void) writeFieldBeginWithName: (NSString *) name
                            type: (int) fieldType
                         fieldID: (int) fieldID
{
    [concreteProtocol writeFieldBeginWithName:name type:fieldType fieldID:fieldID];
}

- (void) writeI32: (int32_t) value
{
    [concreteProtocol writeI32:value];
}

- (void) writeI64: (int64_t) value
{
    [concreteProtocol writeI64:value];
}

- (void) writeI16: (short) value
{
    [concreteProtocol writeI16:value];
}

- (void) writeByte: (uint8_t) value
{
    [concreteProtocol writeByte:value];
}

- (void) writeString: (NSString *) value
{
    [concreteProtocol writeString:value];
}

- (void) writeDouble: (double) value
{
    [concreteProtocol writeDouble:value];
}

- (void) writeBool: (BOOL) value
{
    [concreteProtocol writeBool:value];
}

- (void) writeBinary: (NSData *) data
{
    [concreteProtocol writeBinary:data];
}

- (void) writeFieldStop
{
    [concreteProtocol writeFieldStop];
}

- (void) writeFieldEnd
{
    [concreteProtocol writeFieldEnd];
}

- (void) writeMapBeginWithKeyType: (int) keyType
                        valueType: (int) valueType
                             size: (int) size
{
    [concreteProtocol writeMapBeginWithKeyType:keyType valueType:valueType size:size];
}
- (void) writeMapEnd
{
    [concreteProtocol writeMapEnd];
}


- (void) writeSetBeginWithElementType: (int) elementType
                                 size: (int) size
{
    [concreteProtocol writeSetBeginWithElementType:elementType size:size];
}
- (void) writeSetEnd
{
    [concreteProtocol writeSetEnd];
}


- (void) writeListBeginWithElementType: (int) elementType
                                  size: (int) size
{
    [concreteProtocol writeListBeginWithElementType:elementType size:size];
}

- (void) writeListEnd
{
    [concreteProtocol writeListEnd];
}

@end
