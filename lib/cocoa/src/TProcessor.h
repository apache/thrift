#import <Cocoa/Cocoa.h>


@protocol TProcessor <NSObject>

- (BOOL) processOnInputProtocol: (id <TProtocol>) inProtocol
                 outputProtocol: (id <TProtocol>) outProtocol;

@end
