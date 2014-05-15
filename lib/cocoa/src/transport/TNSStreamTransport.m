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


- (int) readAll: (uint8_t *) buf offset: (int) off length: (int) len
{
  int got = 0;
  int ret = 0;
  while (got < len) {
    ret = [self.mInput read: buf+off+got maxLength: len-got];
    if (ret <= 0) {
      @throw [TTransportException exceptionWithReason: @"Cannot read. Remote side has closed."];
    }
    got += ret;
  }
  return got;
}


- (void) write: (const uint8_t *) data offset: (unsigned int) offset length: (unsigned int) length
{
  int got = 0;
  int result = 0;
  while (got < length) {
    result = [self.mOutput write: data+offset+got maxLength: length-got];
    if (result == -1) {
      @throw [TTransportException exceptionWithReason: @"Error writing to transport output stream."
                                                error: [self.mOutput streamError]];
    } else if (result == 0) {
      @throw [TTransportException exceptionWithReason: @"End of output stream."];
    }
    got += result;
  }
}

- (void) flush
{
  // no flush for you!
}

@end
