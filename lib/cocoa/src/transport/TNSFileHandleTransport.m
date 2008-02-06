
#import "TNSFileHandleTransport.h"
#import "TTransportException.h"


@implementation TNSFileHandleTransport

- (id) initWithFileHandle: (NSFileHandle *) fileHandle
{
  return [self initWithInputFileHandle: fileHandle
                      outputFileHandle: fileHandle];
}


- (id) initWithInputFileHandle: (NSFileHandle *) inputFileHandle
              outputFileHandle: (NSFileHandle *) outputFileHandle
{
  self = [super init];

  mInputFileHandle = [inputFileHandle retain];
  mOutputFileHandle = [outputFileHandle retain];

  return self;
}


- (void) dealloc {
  [mInputFileHandle release];
  [mOutputFileHandle release];
  [super dealloc];
}


- (int) readAll: (uint8_t *) buf offset: (int) off length: (int) len
{
  int got = 0;
  while (got < len) {
    NSData * d = [mInputFileHandle readDataOfLength: len-got];
    if ([d length] == 0) {
      @throw [TTransportException exceptionWithName: @"TTransportException"
                                  reason: @"Cannot read. No more data."];
    }
    [d getBytes: buf+got];
    got += [d length];
  }
  return got;
}


- (void) write: (uint8_t *) data offset: (unsigned int) offset length: (unsigned int) length
{
  NSData * dataObject = [[NSData alloc] initWithBytesNoCopy: data+offset
                                                     length: length
                                               freeWhenDone: NO];

  [mOutputFileHandle writeData: dataObject];


  [dataObject release];
}


- (void) flush
{

}

@end
