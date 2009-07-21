/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */


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

  @try {
    [mOutputFileHandle writeData: dataObject];
  } @catch (NSException * e) {
    @throw [TTransportException exceptionWithName: @"TTransportException"
                                           reason: [NSString stringWithFormat: @"%s: Unable to write data: %@", __PRETTY_FUNCTION__, e]];
  }

  [dataObject release];
}


- (void) flush
{

}

@end
