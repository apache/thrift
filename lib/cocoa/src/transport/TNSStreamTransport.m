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

#import "TNSStreamTransport.h"
#import "TTransportException.h"
#import "TObjective-C.h"


@implementation TNSStreamTransport

- (id) initWithInputStream: (NSInputStream *) input
              outputStream: (NSOutputStream *) output
{
  self = [super init];
  self.mInput = [input retain_stub];
  self.mOutput = [output retain_stub];
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

- (void) dealloc
{
  [self.mInput release_stub];
  [self.mOutput release_stub];
  [super dealloc_stub];
}


- (size_t) readAll: (uint8_t *) buf offset: (size_t) offset length: (size_t) length
{
  size_t totalBytesRead = 0;
  ssize_t bytesRead = 0;
  while (totalBytesRead < length) {
    bytesRead = [self.mInput read: buf+offset+totalBytesRead maxLength: length-totalBytesRead];

    BOOL encounteredErrorOrEOF = (bytesRead <= 0);
    if (encounteredErrorOrEOF) {
      @throw [TTransportException exceptionWithReason: @"Cannot read. Remote side has closed."];
    } else {
        /* bytesRead is guaranteed to be positive and within the range representable by size_t. */
        totalBytesRead += (size_t)bytesRead;
    }
  }
  return totalBytesRead;
}


- (void) write: (const uint8_t *) data offset: (size_t) offset length: (size_t) length
{
  size_t totalBytesWritten = 0;
  ssize_t bytesWritten = 0;
  while (totalBytesWritten < length) {
    bytesWritten = [self.mOutput write: data+offset+totalBytesWritten maxLength: length-totalBytesWritten];
    if (bytesWritten < 0) {
      @throw [TTransportException exceptionWithReason: @"Error writing to transport output stream."
                                                error: [self.mOutput streamError]];
    } else if (bytesWritten == 0) {
      @throw [TTransportException exceptionWithReason: @"End of output stream."];
    } else {
        /* bytesWritten is guaranteed to be positive and within the range representable by size_t. */
        totalBytesWritten += (size_t)bytesWritten;
    }
  }
}

- (void) flush
{
  // no flush for you!
}

@end
