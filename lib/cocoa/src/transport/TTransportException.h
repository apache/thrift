#import "TException.h"

@interface TTransportException : TException {
}

+ (id) exceptionWithReason: (NSString *) reason
                     error: (NSError *) error;

+ (id) exceptionWithReason: (NSString *) reason;

@end
