#import "TException.h"

@implementation TException

+ (id) exceptionWithName: (NSString *) name
{
  return [self exceptionWithName: name reason: @"unknown" error: nil];
}


+ (id) exceptionWithName: (NSString *) name
                  reason: (NSString *) reason
{
  return [self exceptionWithName: name reason: reason error: nil];
}


+ (id) exceptionWithName: (NSString *) name
                  reason: (NSString *) reason
                   error: (NSError *) error
{
  NSDictionary * userInfo = nil;
  if (error != nil) {
    userInfo = [NSDictionary dictionaryWithObject: error forKey: @"error"];
  }

  return [super exceptionWithName: name
                reason: reason
                userInfo: userInfo];
}


- (NSString *) description
{
  NSMutableString * result = [NSMutableString stringWithString: [self name]];
  [result appendFormat: @": %@", [self reason]];
  if ([self userInfo] != nil) {
    [result appendFormat: @"\n  userInfo = %@", [self userInfo]];
  }

  return result;
}


@end
