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
#import "TObjective-C.h"


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

  mInputFileHandle = [inputFileHandle retain_stub];
  mOutputFileHandle = [outputFileHandle retain_stub];

  return self;
}


- (void) dealloc {
  [mInputFileHandle release_stub];
  [mOutputFileHandle release_stub];
  [super dealloc_stub];
}


- (size_t) readAll: (uint8_t *) buf offset: (size_t) offset length: (size_t) length
{
  size_t totalBytesRead = 0;
  while (totalBytesRead < length) {
    NSData * data = [mInputFileHandle readDataOfLength: length-totalBytesRead];
    if ([data length] == 0) {
      @throw [TTransportException exceptionWithName: @"TTransportException"
                                  reason: @"Cannot read. No more data."];
    }
    [data getBytes: buf+totalBytesRead];
    totalBytesRead += [data length];
  }
  return totalBytesRead;
}


- (void) write: (const uint8_t *) data offset: (size_t) offset length: (size_t) length
{
  const void *pos = data + offset;
  NSData * dataObject = [[NSData alloc] initWithBytesNoCopy: (void *)pos
                                                     length: length
                                               freeWhenDone: NO];

  @try {
    [mOutputFileHandle writeData: dataObject];
  } @catch (NSException * e) {
    @throw [TTransportException exceptionWithName: @"TTransportException"
                                           reason: [NSString stringWithFormat: @"%s: Unable to write data: %@", __PRETTY_FUNCTION__, e]];
  }

  [dataObject release_stub];
}


- (void) flush
{

}

@end
