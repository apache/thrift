#import <Cocoa/Cocoa.h>

@interface TException : NSException {
}

+ (id) exceptionWithName: (NSString *) name;

+ (id) exceptionWithName: (NSString *) name
                  reason: (NSString *) reason;

+ (id) exceptionWithName: (NSString *) name
                  reason: (NSString *) reason
                   error: (NSError *) error;

@end
