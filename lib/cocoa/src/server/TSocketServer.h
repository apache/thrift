#import <Cocoa/Cocoa.h>
#import "TProtocolFactory.h"
#import "TProcessor.h"


@interface TSocketServer : NSObject {
  NSSocketPort * mServerSocket;
  NSFileHandle * mSocketFileHandle;
  id <TProtocolFactory> mInputProtocolFactory;
  id <TProtocolFactory> mOutputProtocolFactory;
  id <TProcessor> mProcessor;
}

- (id) initWithPort: (int) port
    protocolFactory: (id <TProtocolFactory>) protocolFactory
          processor: (id <TProcessor>) processor;

@end



