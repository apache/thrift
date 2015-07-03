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

#import <Foundation/Foundation.h>

#import "TTransport.h"


typedef NS_ENUM (int, TMessageType) {
  TMessageTypeCALL = 1,
  TMessageTypeREPLY = 2,
  TMessageTypeEXCEPTION = 3,
  TMessageTypeONEWAY = 4
};

typedef NS_ENUM (int, TType) {
  TTypeSTOP   = 0,
  TTypeVOID   = 1,
  TTypeBOOL   = 2,
  TTypeBYTE   = 3,
  TTypeDOUBLE = 4,
  TTypeI16    = 6,
  TTypeI32    = 8,
  TTypeI64    = 10,
  TTypeSTRING = 11,
  TTypeSTRUCT = 12,
  TTypeMAP    = 13,
  TTypeSET    = 14,
  TTypeLIST   = 15
};


@protocol TProtocol <NSObject>

-(id <TTransport>) transport;

-(BOOL) readMessageBeginReturningName:(NSString **)name
                                 type:(SInt32 *)type
                           sequenceID:(SInt32 *)sequenceID
                                error:(NSError **)error;
-(BOOL) readMessageEnd:(NSError **)error;

-(BOOL) readStructBeginReturningName:(NSString **)name
                               error:(NSError **)error;
-(BOOL) readStructEnd:(NSError **)error;

-(BOOL) readFieldBeginReturningName:(NSString **)name
                               type:(SInt32 *)fieldType
                            fieldID:(SInt32 *)fieldID
                              error:(NSError **)error;
-(BOOL) readFieldEnd:(NSError **)error;

-(BOOL) readString:(NSString **)value error:(NSError **)error;

-(BOOL) readBool:(BOOL *)value error:(NSError **)error;

-(BOOL) readByte:(UInt8 *)value error:(NSError **)error;

-(BOOL) readI16:(SInt16 *)value error:(NSError **)error;

-(BOOL) readI32:(SInt32 *)value error:(NSError **)error;

-(BOOL) readI64:(SInt64 *)value error:(NSError **)error;

-(BOOL) readDouble:(double *)value error:(NSError **)error;

-(BOOL) readBinary:(NSData **)value error:(NSError **)error;

-(BOOL) readMapBeginReturningKeyType:(SInt32 *)keyType
                           valueType:(SInt32 *)valueType
                                size:(SInt32 *)size
                               error:(NSError **)error;
-(BOOL) readMapEnd:(NSError **)error;


-(BOOL) readSetBeginReturningElementType:(SInt32 *)elementType
                                    size:(SInt32 *)size
                                   error:(NSError **)error;
-(BOOL) readSetEnd:(NSError **)error;


-(BOOL) readListBeginReturningElementType:(SInt32 *)elementType
                                     size:(SInt32 *)size
                                    error:(NSError **)error;
-(BOOL) readListEnd:(NSError **)error;


-(BOOL) writeMessageBeginWithName:(NSString *)name
                             type:(SInt32)messageType
                       sequenceID:(SInt32)sequenceID
                            error:(NSError **)error;
-(BOOL) writeMessageEnd:(NSError **)error;

-(BOOL) writeStructBeginWithName:(NSString *)name error:(NSError **)error;
-(BOOL) writeStructEnd:(NSError **)error;

-(BOOL) writeFieldBeginWithName:(NSString *)name
                           type:(SInt32)fieldType
                        fieldID:(SInt32)fieldID
                          error:(NSError **)error;

-(BOOL) writeI32:(SInt32)value error:(NSError **)error;

-(BOOL) writeI64:(SInt64)value error:(NSError **)error;

-(BOOL) writeI16:(short)value error:(NSError **)error;

-(BOOL) writeByte:(UInt8)value error:(NSError **)error;

-(BOOL) writeString:(NSString *)value error:(NSError **)error;

-(BOOL) writeDouble:(double)value error:(NSError **)error;

-(BOOL) writeBool:(BOOL)value error:(NSError **)error;

-(BOOL) writeBinary:(NSData *)data error:(NSError **)error;

-(BOOL) writeFieldStop:(NSError **)error;

-(BOOL) writeFieldEnd:(NSError **)error;

-(BOOL) writeMapBeginWithKeyType:(SInt32)keyType
                       valueType:(SInt32)valueType
                            size:(SInt32)size
                           error:(NSError **)error;
-(BOOL) writeMapEnd:(NSError **)error;


-(BOOL) writeSetBeginWithElementType:(SInt32)elementType
                                size:(SInt32)size
                               error:(NSError **)error;
-(BOOL) writeSetEnd:(NSError **)error;


-(BOOL) writeListBeginWithElementType:(SInt32)elementType
                                 size:(SInt32)size
                                error:(NSError **)error;

-(BOOL) writeListEnd:(NSError **)error;


@end

