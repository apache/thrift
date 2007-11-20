#import "TTransportException.h"

@implementation TTransportException

+ (id) exceptionWithReason: (NSString *) reason
                     error: (NSError *) error
{
  NSDictionary * userInfo = nil;
  if (error != nil) {
    userInfo = [NSDictionary dictionaryWithObject: error forKey: @"error"];
  }

  return [super exceptionWithName: @"TTransportException"
                           reason: reason
                         userInfo: userInfo];
}


+ (id) exceptionWithReason: (NSString *) reason
{
  return [self exceptionWithReason: reason error: nil];
}

@end
