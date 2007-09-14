#import "TProtocolUtil.h"

@implementation TProtocolUtil

+ (void) skipType: (int) type onProtocol: (id <TProtocol>) protocol
{
  switch (type) {
  case TType_BOOL:
    [protocol readBool];
    break;
  case TType_BYTE:
    [protocol readByte];
    break;
  case TType_I16:
    [protocol readI16];
    break;
  case TType_I32:
    [protocol readI32];
    break;
  case TType_I64:
    [protocol readI64];
    break;
  case TType_DOUBLE:
    [protocol readDouble];
    break;
  case TType_STRING:
    [protocol readString];
    break;
  case TType_STRUCT:
    [protocol readStructBeginReturningName: NULL];
    while (true) {
      int fieldType;
      [protocol readFieldBeginReturningName: nil type: &fieldType fieldID: nil];
      if (fieldType == TType_STOP) {
        break;
      }
      [TProtocolUtil skipType: fieldType onProtocol: protocol];
      [protocol readFieldEnd];
    }
    [protocol readStructEnd];
    break;
  case TType_MAP:
  {
    int keyType;
    int valueType;
    int size;
    [protocol readMapBeginReturningKeyType: &keyType valueType: &valueType size: &size];
    int i;
    for (i = 0; i < size; i++) {
      [TProtocolUtil skipType: keyType onProtocol: protocol];
      [TProtocolUtil skipType: valueType onProtocol: protocol];
    }
    [protocol readMapEnd];
  }
    break;
    case TType_SET:
    {
      int elemType;
      int size;
      [protocol readSetBeginReturningElementType: &elemType size: &size];
      int i;
      for (i = 0; i < size; i++) {
        [TProtocolUtil skipType: elemType onProtocol: protocol];
      }
      [protocol readSetEnd];
    }
      break;
    case TType_LIST:
    {
      int elemType;
      int size;
      [protocol readListBeginReturningElementType: &elemType size: &size];
      int i;
      for (i = 0; i < size; i++) {
        [TProtocolUtil skipType: elemType onProtocol: protocol];
      }
      [protocol readListEnd];
    }
      break;
    default:
      return;
  }
}

@end
