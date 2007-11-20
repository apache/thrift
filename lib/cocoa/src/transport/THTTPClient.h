#import <Cocoa/Cocoa.h>
#import "TTransport.h"

@interface THTTPClient : NSObject <TTransport> {
  NSURL * mURL;
  NSMutableURLRequest * mRequest;
  NSMutableData * mRequestData;
  NSData * mResponseData;
  int mResponseDataOffset;
}

- (id) initWithURL: (NSURL *) aURL;

- (id) initWithURL: (NSURL *) aURL 
           timeout: (int) timeout;

- (void) setURL: (NSURL *) aURL;

@end

