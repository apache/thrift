#import <Cocoa/Cocoa.h>
#import "TProtocol.h"
#import "TTransport.h"


@protocol TProtocolFactory <NSObject>

- (id <TProtocol>) newProtocolOnTransport: (id <TTransport>) transport;

@end
