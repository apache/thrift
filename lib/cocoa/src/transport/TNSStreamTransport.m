#import "TNSStreamTransport.h"
#import "TTransportException.h"


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
      @throw [TTransportException exceptionWithReason: @"Cannot read. Remote side has closed."];
    }
    got += ret;
  }
  return got;
}


// FIXME:geech:20071019 - make this write all
- (void) write: (uint8_t *) data offset: (unsigned int) offset length: (unsigned int) length
{
  int result = [mOutput write: data+offset maxLength: length];
  if (result == -1) {
    @throw [TTransportException exceptionWithReason: @"Error writing to transport output stream."
                                              error: [mOutput streamError]];
  } else if (result == 0) {
    @throw [TTransportException exceptionWithReason: @"End of output stream."];
  } else if (result != length) {
    @throw [TTransportException exceptionWithReason: @"Output stream did not write all of our data."];
  }
}

- (void) flush
{
  // no flush for you!
}

@end
