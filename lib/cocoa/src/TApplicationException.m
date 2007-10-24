#import "TApplicationException.h"
#import "TProtocolUtil.h"

@implementation TApplicationException

- (id) initWithType: (int) type
             reason: (NSString *) reason
{
  NSString * name;
  switch (type) {
  case TApplicationException_UNKNOWN_METHOD:
    name = @"Unknown method";
    break;
  case TApplicationException_INVALID_MESSAGE_TYPE:
    name = @"Invalid message type";
    break;
  case TApplicationException_WRONG_METHOD_NAME:
    name = @"Wrong method name";
    break;
  case TApplicationException_BAD_SEQUENCE_ID:
    name = @"Bad sequence ID";
    break;
  case TApplicationException_MISSING_RESULT:
    name = @"Missing result";
    break;
  default:
    name = @"Unknown";
    break;
  }

  self = [super initWithName: name reason: reason userInfo: nil];
  return self;
}


+ (TApplicationException *) read: (id <TProtocol>) protocol
{
  NSString * reason = nil;
  int type = TApplicationException_UNKNOWN;
  int fieldType;
  int fieldID;

  while (true) {
    [protocol readFieldBeginReturningName: NULL
              type: &fieldType
              fieldID: &fieldID];
    if (fieldType == TType_STOP) { 
      break;
    }
    switch (fieldID) {
    case 1:
      if (fieldType == TType_STRING) {
        reason = [protocol readString];
      } else { 
        [TProtocolUtil skipType: fieldType onProtocol: protocol];
      }
      break;
    case 2:
      if (fieldType == TType_I32) {
        type = [protocol readI32];
      } else { 
        [TProtocolUtil skipType: fieldType onProtocol: protocol];
      }
      break;
    default:
      [TProtocolUtil skipType: fieldType onProtocol: protocol];
      break;
    }
    [protocol readFieldEnd];
  }
  [protocol readStructEnd];

  return [TApplicationException exceptionWithType: type reason: reason];
}


+ (TApplicationException *) exceptionWithType: (int) type
                                      reason: (NSString *) reason
{
  return [[[TApplicationException alloc] initWithType: type
                                         reason: reason] autorelease];
}

@end
