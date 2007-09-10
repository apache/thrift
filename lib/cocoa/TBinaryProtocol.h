#import "TProtocol.h"
#import "TTransport.h"

@interface TBinaryProtocol : NSObject <TProtocol> {
  id <TTransport> mTransport;
  BOOL mStrictRead;
  BOOL mStrictWrite;
}

- (id) initWithTransport: (id <TTransport>) transport;

- (id) initWithTransport: (id <TTransport>) transport 
              strictRead: (BOOL) strictRead
             strictWrite: (BOOL) strictWrite;

@end;
