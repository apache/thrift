#import <Cocoa/Cocoa.h>
#import "TTransport.h"

@interface TNSStreamTransport : NSObject <TTransport> {
  NSInputStream * mInput;
  NSOutputStream * mOutput;
}

- (id) initWithInputStream: (NSInputStream *) input
              outputStream: (NSOutputStream *) output;

- (id) initWithInputStream: (NSInputStream *) input;

- (id) initWithOutputStream: (NSOutputStream *) output;

@end



