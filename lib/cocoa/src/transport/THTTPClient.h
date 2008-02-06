#import <Cocoa/Cocoa.h>
#import "TTransport.h"

@interface THTTPClient : NSObject <TTransport> {
  NSURL * mURL;
  NSMutableURLRequest * mRequest;
  NSMutableData * mRequestData;
  NSData * mResponseData;
  int mResponseDataOffset;
  NSString * mUserAgent;
  int mTimeout;
}

- (id) initWithURL: (NSURL *) aURL;

- (id) initWithURL: (NSURL *) aURL
         userAgent: (NSString *) userAgent
           timeout: (int) timeout;

- (void) setURL: (NSURL *) aURL;

@end

