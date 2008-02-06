#import "TProtocol.h"
#import "TTransport.h"
#import "TProtocolFactory.h"


@interface TBinaryProtocol : NSObject <TProtocol> {
  id <TTransport> mTransport;
  BOOL mStrictRead;
  BOOL mStrictWrite;
  int32_t mMessageSizeLimit;
}

- (id) initWithTransport: (id <TTransport>) transport;

- (id) initWithTransport: (id <TTransport>) transport
              strictRead: (BOOL) strictRead
             strictWrite: (BOOL) strictWrite;

- (int32_t) messageSizeLimit;
- (void) setMessageSizeLimit: (int32_t) sizeLimit;

@end;


@interface TBinaryProtocolFactory : NSObject <TProtocolFactory> {
}

+ (TBinaryProtocolFactory *) sharedFactory;

- (TBinaryProtocol *) newProtocolOnTransport: (id <TTransport>) transport;

@end
