#import "TProtocol.h"
#import "TTransport.h"

@interface TProtocolUtil : NSObject {

}

+ (void) skipType: (int) type onProtocol: (id <TProtocol>) protocol;

@end;
