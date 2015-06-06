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

#import "TCompactProtocol.h"
#import "TObjective-C.h"
#import "TProtocolException.h"

static const uint8_t COMPACT_PROTOCOL_ID = 0x82;
static const uint8_t COMPACT_VERSION = 1;
static const uint8_t COMPACT_VERSION_MASK = 0x1F; // 0001 1111
static const uint8_t COMPACT_TYPE_MASK = 0xE0; // 1110 0000
static const uint8_t COMPACT_TYPE_BITS = 0x07; // 0000 0111
static const int COMPACT_TYPE_SHIFT_AMOUNT = 5;

enum {
  TCType_STOP = 0x00,
  TCType_BOOLEAN_TRUE = 0x01,
  TCType_BOOLEAN_FALSE = 0x02,
  TCType_BYTE = 0x03,
  TCType_I16 = 0x04,
  TCType_I32 = 0x05,
  TCType_I64 = 0x06,
  TCType_DOUBLE = 0x07,
  TCType_BINARY = 0x08,
  TCType_LIST = 0x09,
  TCType_SET = 0x0A,
  TCType_MAP = 0x0B,
  TCType_STRUCT = 0x0C,
};

@implementation TCompactProtocolFactory

+ (TCompactProtocolFactory *) sharedFactory
{
  static TCompactProtocolFactory * gSharedFactory = nil;
  if (gSharedFactory == nil) {
    gSharedFactory = [[TCompactProtocolFactory alloc] init];
  }
  
  return gSharedFactory;
}

- (TCompactProtocol *) newProtocolOnTransport: (id <TTransport>) transport
{
  return [[TCompactProtocol alloc] initWithTransport: transport];
}

@end

@implementation TCompactProtocol {
  NSMutableArray * lastField;
  short lastFieldId;
  id <TTransport> mTransport;
  
  NSString * boolFieldName;
  NSNumber * boolFieldType;
  NSNumber * boolFieldId;
  NSNumber * booleanValue;
}

- (id) init
{
  self = [super init];
  
  if (self != nil) {
    lastField = [[NSMutableArray alloc] init];
  }
  
  return self;
}

- (id) initWithTransport: (id <TTransport>) transport
{
  self = [self init];
  
  if (self != nil) {
    mTransport = [transport retain_stub];
  }
  
  return self;
}

- (void) dealloc
{
  [lastField release_stub];
  [mTransport release_stub];
  [boolFieldName release_stub];
  [boolFieldType release_stub];
  [boolFieldId release_stub];
  [booleanValue release_stub];
  
  [super dealloc_stub];
}

- (id <TTransport>) transport
{
  return mTransport;
}

- (void) writeByteDirect: (int8_t) n
{
  [mTransport write: (uint8_t *)&n offset: 0 length: 1];
}

- (void)writeVarint32: (uint32_t) n
{
  uint8_t i32buf[5] = {0};
  uint32_t idx = 0;
  
  while (true) {
    if ((n & ~0x7F) == 0) {
      i32buf[idx++] = (uint8_t)n;
      break;
    } else {
      i32buf[idx++] = (uint8_t)((n & 0x7F) | 0x80);
      n >>= 7;
    }
  }
  
  [mTransport write: i32buf offset: 0 length: idx];
}

- (void) writeMessageBeginWithName: (NSString *) name
                              type: (int) messageType
                        sequenceID: (int) sequenceID
{
  [self writeByteDirect: COMPACT_PROTOCOL_ID];
  [self writeByteDirect: (uint8_t)((COMPACT_VERSION & COMPACT_VERSION_MASK) |
                                   ((((uint32_t)messageType) << COMPACT_TYPE_SHIFT_AMOUNT) & COMPACT_TYPE_MASK))];
  [self writeVarint32: (uint32_t)sequenceID];
  [self writeString: name];
}

- (void) writeStructBeginWithName: (NSString *) name
{
  [lastField addObject: [NSNumber numberWithShort: lastFieldId]];
  lastFieldId = 0;
}

- (void) writeStructEnd
{
  lastFieldId = [[lastField lastObject] shortValue];
  [lastField removeLastObject];
}

- (void) writeFieldBeginWithName: (NSString *) name
                            type: (int) fieldType
                         fieldID: (int) fieldID
{
  if (fieldType == TType_BOOL) {
    boolFieldName = [name copy];
    boolFieldType = [[NSNumber numberWithInt: fieldType] retain_stub];
    boolFieldId = [[NSNumber numberWithInt: fieldID] retain_stub];
  } else {
    [self writeFieldBeginInternalWithName: name
                                     type: fieldType
                                  fieldID: fieldID
                             typeOverride: 0xFF];
  }
}

- (void) writeFieldBeginInternalWithName: (NSString *) name
                                    type: (int) fieldType
                                 fieldID: (int) fieldID
                            typeOverride: (uint8_t) typeOverride
{
  uint8_t typeToWrite = typeOverride == 0xFF ? [self compactTypeForTType: fieldType] : typeOverride;
  
  // check if we can use delta encoding for the field id
  if (fieldID > lastFieldId && fieldID - lastFieldId <= 15) {
    // Write them together
    [self writeByteDirect: (fieldID - lastFieldId) << 4 | typeToWrite];
  } else {
    // Write them separate
    [self writeByteDirect: typeToWrite];
    [self writeI16: fieldID];
  }
  
  lastFieldId = fieldID;
}

- (void) writeFieldStop
{
  [self writeByteDirect: TCType_STOP];
}

- (void) writeMapBeginWithKeyType: (int) keyType
                        valueType: (int) valueType
                             size: (int) size
{
  if (size == 0) {
    [self writeByteDirect: 0];
  } else {
    [self writeVarint32: (uint32_t)size];
    [self writeByteDirect: [self compactTypeForTType: keyType] << 4 | [self compactTypeForTType: valueType]];
  }
}

- (void) writeListBeginWithElementType: (int) elementType
                                  size: (int) size
{
  [self writeCollectionBeginWithElementType: elementType size: size];
}

- (void) writeSetBeginWithElementType: (int) elementType
                                 size: (int) size
{
  [self writeCollectionBeginWithElementType: elementType size: size];
}

- (void) writeBool: (BOOL) b
{
  if (boolFieldId != nil && boolFieldName != nil && boolFieldType != nil) {
    // we haven't written the field header yet
    [self writeFieldBeginInternalWithName: boolFieldName
                                     type: [boolFieldType intValue]
                                  fieldID: [boolFieldId intValue]
                             typeOverride: b ? TCType_BOOLEAN_TRUE : TCType_BOOLEAN_FALSE];
    
    [boolFieldId release_stub];
    [boolFieldName release_stub];
    [boolFieldType release_stub];
    
    boolFieldId = nil;
    boolFieldName = nil;
    boolFieldType = nil;
  } else {
    // we're not part of a field, so just Write the value.
    [self writeByteDirect: b ? TCType_BOOLEAN_TRUE : TCType_BOOLEAN_FALSE];
  }
}

- (void) writeByte: (uint8_t) value
{
  [self writeByteDirect: value];
}

- (void) writeI16: (int16_t) value
{
  [self writeVarint32: [self i32ToZigZag: value]];
}

- (void) writeI32: (int32_t) value
{
  [self writeVarint32: [self i32ToZigZag: value]];
}

- (void) writeI64: (int64_t) value
{
  [self writeVarint64: [self i64ToZigZag: value]];
}

- (void) writeDouble: (double) value
{
  //Safe bit-casting double->uint64
  
  uint64_t bits = 0;
  memcpy(&bits, &value, 8);
  
  bits = OSSwapHostToLittleInt64(bits);
  
  [mTransport write: (uint8_t *)&bits offset: 0 length: 8];
}

- (void) writeString: (NSString *) value
{
  [self writeBinary: [value dataUsingEncoding: NSUTF8StringEncoding]];
}

- (void) writeBinary: (NSData *) data
{
  [self writeVarint32: (uint32_t)data.length];
  [mTransport write: data.bytes offset: 0 length: data.length];
}

- (void) writeMessageEnd {}
- (void) writeMapEnd {}
- (void) writeListEnd {}
- (void) writeSetEnd {}
- (void) writeFieldEnd {}

- (void) writeCollectionBeginWithElementType: (int) elementType
                                        size: (int) size
{
  if (size <= 14) {
    [self writeByteDirect: size << 4 | [self compactTypeForTType: elementType]];
  } else {
    [self writeByteDirect: 0xf0 | [self compactTypeForTType: elementType]];
    [self writeVarint32: (uint32_t)size];
  }
}

- (void) writeVarint64: (uint64_t) n
{
  uint8_t varint64out[10] = {0};
  int idx = 0;
  
  while (true) {
    if ((n & ~0x7FL) == 0) {
      varint64out[idx++] = (uint8_t)n;
      break;
    } else {
      varint64out[idx++] = (uint8_t)((n & 0x7F) | 0x80);
      n >>= 7;
    }
  }
  
  [mTransport write: varint64out offset: 0 length: idx];
}

- (uint32_t) i32ToZigZag: (int32_t) n
{
  /*
   ZigZag encoding maps signed integers to unsigned integers so that
   numbers with a small absolute value (for instance, -1) have
   a small varint encoded value too. It does this in a way that
   "zig-zags" back and forth through the positive and negative integers,
   so that -1 is encoded as 1, 1 is encoded as 2, -2 is encoded as 3, and so on
   */
  return (uint32_t)(n << 1) ^ (uint32_t)(n >> 31);
}

- (uint64_t) i64ToZigZag: (int64_t) n
{
  return (uint64_t)(n << 1) ^ (uint64_t)(n >> 63);
}

- (void) readMessageBeginReturningName: (NSString **) pname
                                  type: (int *) ptype
                            sequenceID: (int *) psequenceID
{
  uint8_t protocolId = [self readByte];
  if (protocolId != COMPACT_PROTOCOL_ID) {
    @throw [TProtocolException exceptionWithName: @"TProtocolException"
                                          reason: [NSString stringWithFormat: @"Expected protocol id %X but got %X", COMPACT_PROTOCOL_ID, protocolId]];
  }
  
  uint8_t versionAndType = [self readByte];
  uint8_t version = versionAndType & COMPACT_VERSION_MASK;
  if (version != COMPACT_VERSION) {
    @throw [TProtocolException exceptionWithName: @"TProtocolException"
                                          reason: [NSString stringWithFormat: @"Expected version %d but got %d", COMPACT_VERSION, version]];
  }
  
  int type = (versionAndType >> COMPACT_TYPE_SHIFT_AMOUNT) & COMPACT_TYPE_BITS;
  int sequenceID = (int)[self readVarint32];
  NSString* name = [self readString];
  
  if (ptype != NULL) {
    *ptype = type;
  }
  if (psequenceID != NULL) {
    *psequenceID = sequenceID;
  }
  if (pname != NULL) {
    *pname = name;
  }
}

- (void) readStructBeginReturningName: (NSString **) pname
{
  [lastField addObject: [NSNumber numberWithShort: lastFieldId]];
  lastFieldId = 0;
  
  if (pname != NULL) {
    *pname = @"";
  }
}

- (void) readStructEnd
{
  lastFieldId = [[lastField lastObject] shortValue];
  [lastField removeLastObject];
}

- (void) readFieldBeginReturningName: (NSString **) pname
                                type: (int *) pfieldType
                             fieldID: (int *) pfieldID
{
  uint8_t byte = [self readByte];
  uint8_t type = byte & 0x0f;
  
  // if it's a stop, then we can return immediately, as the struct is over.
  if (type == TCType_STOP) {
    if (pname != NULL) {
      *pname = @"";
    }
    if (pfieldType != NULL) {
      *pfieldType = TType_STOP;
    }
    if (pfieldID != NULL) {
      *pfieldID = 0;
    }
    return;
  }
  
  short fieldId = 0;
  
  // mask off the 4 MSB of the type header. it could contain a field id delta.
  short modifier = (byte & 0xf0) >> 4;
  if (modifier == 0) {
    // not a delta. look ahead for the zigzag varint field id.
    fieldId = [self readI16];
  } else {
    // has a delta. add the delta to the last Read field id.
    fieldId = lastFieldId + modifier;
  }
  
  int fieldType = [self ttypeForCompactType: type];
  
  if (pname != NULL) {
    *pname = @"";
  }
  if (pfieldType != NULL) {
    *pfieldType = fieldType;
  }
  if (pfieldID != NULL) {
    *pfieldID = fieldId;
  }
  
  // if this happens to be a boolean field, the value is encoded in the type
  if (type == TCType_BOOLEAN_TRUE ||
      type == TCType_BOOLEAN_FALSE) {
    // save the boolean value in a special instance variable.
    booleanValue = [[NSNumber numberWithBool: type == TCType_BOOLEAN_TRUE] retain_stub];
  }
  
  // push the new field onto the field stack so we can keep the deltas going.
  lastFieldId = fieldId;
}

- (void) readMapBeginReturningKeyType: (int *) pkeyType
                            valueType: (int *) pvalueType
                                 size: (int *) psize
{
  uint8_t keyAndValueType = 0;
  int size = (int)[self readVarint32];
  if (size != 0) {
    keyAndValueType = [self readByte];
  }
  
  int keyType = [self ttypeForCompactType: keyAndValueType >> 4];
  int valueType = [self ttypeForCompactType: keyAndValueType & 0xf];
  
  if (pkeyType != NULL) {
    *pkeyType = keyType;
  }
  if (pvalueType != NULL) {
    *pvalueType = valueType;
  }
  if (psize != NULL) {
    *psize = size;
  }
}

- (void) readListBeginReturningElementType: (int *) pelementType
                                      size: (int *) psize
{
  uint8_t size_and_type = [self readByte];
  int size = (size_and_type >> 4) & 0x0f;
  if (size == 15) {
    size = (int)[self readVarint32];
  }
  
  int elementType = [self ttypeForCompactType: size_and_type & 0x0f];
  
  if (pelementType != NULL) {
    *pelementType = elementType;
  }
  if (psize != NULL) {
    *psize = size;
  }
}

- (void) readSetBeginReturningElementType: (int *) pelementType
                                     size: (int *) psize
{
  [self readListBeginReturningElementType: pelementType size: psize];
}

- (BOOL) readBool
{
  if (booleanValue != nil) {
    BOOL result = [booleanValue boolValue];
    [booleanValue release_stub];
    booleanValue = nil;
    return result;
  } else {
    return [self readByte] == TCType_BOOLEAN_TRUE;
  }
}

- (uint8_t) readByte
{
  uint8_t buf = 0;
  [mTransport readAll: &buf offset: 0 length: 1];
  return buf;
}

- (int16_t) readI16
{
  return (int16_t)[self zigZagToi32: [self readVarint32]];
}

- (int32_t) readI32
{
  return [self zigZagToi32: [self readVarint32]];
}

- (int64_t) readI64
{
  return [self zigZagToi64: [self readVarint64]];
}

- (double) readDouble
{
  uint64_t bits = 0;
  [mTransport readAll: (uint8_t *)&bits offset: 0 length: 8];
  bits = OSSwapLittleToHostInt64(bits);
  
  double result = 0;
  memcpy(&result, &bits, 8);
  
  return result;
}

- (NSString *) readString
{
  int length = (int)[self readVarint32];
  if (length == 0) {
    return @"";
  }
  
  return [[[NSString alloc] initWithData: [self readBinary: length]
                                encoding: NSUTF8StringEncoding] autorelease_stub];
}

- (NSData *) readBinary
{
  return [self readBinary: (int)[self readVarint32]];
}

- (NSData *) readBinary: (int) length
{
  if (length == 0) {
    return [NSData data];
  }
  
  NSMutableData* buf = [NSMutableData dataWithLength: length];
  [mTransport readAll: buf.mutableBytes offset: 0 length: length];
  return buf;
}

- (void) readMessageEnd {}
- (void) readFieldEnd {}
- (void) readMapEnd {}
- (void) readListEnd {}
- (void) readSetEnd {}

- (uint32_t) readVarint32
{
  uint32_t result = 0;
  int shift = 0;
  
  while (true) {
    uint8_t byte = [self readByte];
    result |= (uint32_t)(byte & 0x7f) << shift;
    if (!(byte & 0x80)) {
      break;
    }
    
    shift += 7;
  }
  return result;
}

- (uint64_t) readVarint64
{
  int shift = 0;
  uint64_t result = 0;
  
  while (true) {
    uint8_t byte = [self readByte];
    result |= (uint64_t)(byte & 0x7f) << shift;
    if (!(byte & 0x80)) {
      break;
    }
    
    shift += 7;
  }
  
  return result;
}

- (int32_t) zigZagToi32: (uint32_t) n
{
  return (int32_t)(n >> 1) ^ (-(int32_t)(n & 1));
}

- (int64_t) zigZagToi64: (uint64_t) n
{
  return (int64_t)(n >> 1) ^ (-(int64_t)(n & 1));
}

- (uint8_t) ttypeForCompactType: (uint8_t) type
{
  switch (type & 0x0f) {
    case TCType_STOP:
      return TType_STOP;
      
    case TCType_BOOLEAN_FALSE:
    case TCType_BOOLEAN_TRUE:
      return TType_BOOL;
      
    case TCType_BYTE:
      return TType_BYTE;
      
    case TCType_I16:
      return TType_I16;
      
    case TCType_I32:
      return TType_I32;
      
    case TCType_I64:
      return TType_I64;
      
    case TCType_DOUBLE:
      return TType_DOUBLE;
      
    case TCType_BINARY:
      return TType_STRING;
      
    case TCType_LIST:
      return TType_LIST;
      
    case TCType_SET:
      return TType_SET;
      
    case TCType_MAP:
      return TType_MAP;
      
    case TCType_STRUCT:
      return TType_STRUCT;
      
    default:
      @throw [TProtocolException exceptionWithName: @"TProtocolException"
                                            reason: [NSString stringWithFormat: @"Don't know what type: %d", (uint8_t)(type & 0x0F)]];
  }
}

- (uint8_t) compactTypeForTType: (uint8_t) ttype
{
  static uint8_t ttypeToCompactType[] = {
    [TType_STOP] = TCType_STOP,
    [TType_BOOL] = TCType_BOOLEAN_FALSE,
    [TType_BYTE] = TCType_BYTE,
    [TType_DOUBLE] = TCType_DOUBLE,
    [TType_I16] = TCType_I16,
    [TType_I32] = TCType_I32,
    [TType_I64] = TCType_I64,
    [TType_STRING] = TCType_BINARY,
    [TType_STRUCT] = TCType_STRUCT,
    [TType_MAP] = TCType_MAP,
    [TType_SET] = TCType_SET,
    [TType_LIST] = TCType_LIST
  };
  
  return ttypeToCompactType[ttype];
}

@end
