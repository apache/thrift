#import <Cocoa/Cocoa.h>

@interface TException : NSException {
}

- (id) initWithType: (int) type
            message: (NSString *) message;

@end
