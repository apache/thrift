#import <Cocoa/Cocoa.h>

#import "TTransport.h"


enum {
  TMessageType_CALL = 1,
  TMessageType_REPLY = 2,
  TMessageType_EXCEPTION = 3
};

enum {
  TType_STOP   = 0,
  TType_VOID   = 1,
  TType_BOOL   = 2,
  TType_BYTE   = 3,
  TType_DOUBLE = 4,
  TType_I16    = 6,
  TType_I32    = 8,
  TType_I64    = 10,
  TType_STRING = 11,
  TType_STRUCT = 12,
  TType_MAP    = 13,
  TType_SET    = 14,
  TType_LIST   = 15
};


@protocol TProtocol <NSObject>

- (id <TTransport>) transport;

- (void) readMessageBeginReturningName: (NSString **) name
                                  type: (int *) type
                            sequenceID: (int *) sequenceID;
- (void) readMessageEnd;

- (void) readStructBeginReturningName: (NSString **) name;
- (void) readStructEnd;

- (void) readFieldBeginReturningName: (NSString **) name
                                type: (int *) fieldType
                             fieldID: (int *) fieldID;
- (void) readFieldEnd;

- (NSString *) readString;

- (BOOL) readBool;

- (unsigned char) readByte;

- (short) readI16;

- (int32_t) readI32;

- (int64_t) readI64;

- (double) readDouble;

- (NSData *) readBinary;

- (void) readMapBeginReturningKeyType: (int *) keyType
                            valueType: (int *) valueType
                                 size: (int *) size;
- (void) readMapEnd;


- (void) readSetBeginReturningElementType: (int *) elementType
                                     size: (int *) size;
- (void) readSetEnd;


- (void) readListBeginReturningElementType: (int *) elementType
                                      size: (int *) size;
- (void) readListEnd;


- (void) writeMessageBeginWithName: (NSString *) name
                              type: (int) messageType
                        sequenceID: (int) sequenceID;
- (void) writeMessageEnd;

- (void) writeStructBeginWithName: (NSString *) name;
- (void) writeStructEnd;

- (void) writeFieldBeginWithName: (NSString *) name
                            type: (int) fieldType
                         fieldID: (int) fieldID;

- (void) writeI32: (int32_t) value;

- (void) writeI64: (int64_t) value;

- (void) writeI16: (short) value;

- (void) writeByte: (uint8_t) value;

- (void) writeString: (NSString *) value;

- (void) writeDouble: (double) value;

- (void) writeBool: (BOOL) value;

- (void) writeBinary: (NSData *) data;

- (void) writeFieldStop;

- (void) writeFieldEnd;

- (void) writeMapBeginWithKeyType: (int) keyType
                        valueType: (int) valueType
                             size: (int) size;
- (void) writeMapEnd;


- (void) writeSetBeginWithElementType: (int) elementType
                                 size: (int) size;
- (void) writeSetEnd;


- (void) writeListBeginWithElementType: (int) elementType
                                  size: (int) size;

- (void) writeListEnd;


@end

