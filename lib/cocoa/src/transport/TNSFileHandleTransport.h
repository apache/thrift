
#import <Cocoa/Cocoa.h>
#import "TTransport.h"

@interface TNSFileHandleTransport : NSObject <TTransport> {
  NSFileHandle * mInputFileHandle;
  NSFileHandle * mOutputFileHandle;
}

- (id) initWithFileHandle: (NSFileHandle *) fileHandle;

- (id) initWithInputFileHandle: (NSFileHandle *) inputFileHandle
              outputFileHandle: (NSFileHandle *) outputFileHandle;


@end
