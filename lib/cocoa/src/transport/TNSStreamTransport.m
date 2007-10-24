#import "TNSStreamTransport.h"

@implementation TNSStreamTransport

- (id) initWithInputStream: (NSInputStream *) input
              outputStream: (NSOutputStream *) output
{
  [super init];
  mInput = [input retain];
  mOutput = [output retain];
  return self;
}

- (id) initWithInputStream: (NSInputStream *) input
{
  return [self initWithInputStream: input outputStream: nil];
}

- (id) initWithOutputStream: (NSOutputStream *) output
{
  return [self initWithInputStream: nil outputStream: output];
}


- (int) readAll: (uint8_t *) buf offset: (int) off length: (int) len
{
  int got = 0;
  int ret = 0;
  while (got < len) {
    ret = [mInput read: buf+off+got maxLength: len-got];
    if (ret <= 0) {
      @throw [NSException exceptionWithName: @"TTransportException"
                          reason: @"Cannot read. Remote side has closed."
                          userInfo: nil];
    }
    got += ret;
  }
  return got;
}


- (void) write: (uint8_t *) data offset: (unsigned int) offset length: (unsigned int) length
{
  int result = [mOutput write: data+offset maxLength: length];
  if (result == -1) {
    NSDictionary * errorInfo = [NSDictionary dictionaryWithObject: [mOutput streamError]
                                             forKey: @"error"];
    @throw [NSException exceptionWithName: @"TTransportException"
                        reason: [NSString stringWithFormat: @"Error writing to transport output stream (%@).", [mOutput streamError]]
                                 userInfo: errorInfo];
  } else if (result == 0) {
    @throw [NSException exceptionWithName: @"TTransportException"
                        reason: @"End of output stream."
                        userInfo: nil];
  } else if (result != length) {
    @throw [NSException exceptionWithName: @"TTransportException"
                        reason: @"Output stream did not write all of our data."
                        userInfo: nil];
  }
} 

- (void) flush
{
  // no flush for you!
}

@end
