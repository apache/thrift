#import "TException.h"
#import "TProtocol.h"

enum {
  TApplicationException_UNKNOWN = 0,
  TApplicationException_UNKNOWN_METHOD = 1,
  TApplicationException_INVALID_MESSAGE_TYPE = 2,
  TApplicationException_WRONG_METHOD_NAME = 3,
  TApplicationException_BAD_SEQUENCE_ID = 4,
  TApplicationException_MISSING_RESULT = 5
};

// FIXME
@interface TApplicationException : TException {
  int mType;
}

+ (TApplicationException *) read: (id <TProtocol>) protocol;

- (void) write: (id <TProtocol>) protocol;

+ (TApplicationException *) exceptionWithType: (int) type
                                       reason: (NSString *) message;

@end
