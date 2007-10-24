#import <Cocoa/Cocoa.h>
#import "TNSStreamTransport.h"

@interface TSocketClient : TNSStreamTransport {
}

- (id) initWithHostname: (NSString *) hostname
                   port: (int) port;

@end



