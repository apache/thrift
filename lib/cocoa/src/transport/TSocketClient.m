#import <Cocoa/Cocoa.h>
#import "TSocketClient.h"

@implementation TSocketClient

- (id) initWithHostname: (NSString *) hostname
                   port: (int) port
{
  NSInputStream * input = nil;
  NSOutputStream * output = nil;
  
  [NSStream getStreamsToHost: [NSHost hostWithName: hostname]
            port: port
            inputStream: &input 
            outputStream: &output];

  return [super initWithInputStream: input outputStream: output];
}


@end



