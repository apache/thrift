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

#import "TFramedTransport.h"
#import "TTransportError.h"

#define HEADER_SIZE 4
#define INIT_FRAME_SIZE 1024


@interface TFramedTransport () {
  id<TTransport> transport;
  NSMutableData *writeBuffer;
  NSMutableData *readBuffer;
  NSUInteger readOffset;
}

@end


@implementation TFramedTransport

-(id) initWithTransport:(id <TTransport>)aTransport
{
  if ((self = [self init])) {
    transport = aTransport;
    readBuffer = nil;
    readOffset = 0;
    writeBuffer = [NSMutableData dataWithLength:HEADER_SIZE];
  }
  return self;
}

-(BOOL) flush:(NSError **)error
{
  int len = (int)[writeBuffer length];
  int data_len = len - HEADER_SIZE;
  if (data_len < 0) {
    if (error) {
      *error = [NSError errorWithDomain:TTransportErrorDomain
                                   code:TTransportErrorNoFrameHeader
                               userInfo:@{}];
    }
    return NO;
  }

  UInt8 i32rd[HEADER_SIZE];
  i32rd[0] = (UInt8)(0xff & (data_len >> 24));
  i32rd[1] = (UInt8)(0xff & (data_len >> 16));
  i32rd[2] = (UInt8)(0xff & (data_len >> 8));
  i32rd[3] = (UInt8)(0xff & (data_len));

  // should we make a copy of the writeBuffer instead? Better for threaded
  //  operations!
  [writeBuffer replaceBytesInRange:NSMakeRange(0, HEADER_SIZE)
                         withBytes:i32rd length:HEADER_SIZE];

  if (![transport write:writeBuffer.mutableBytes offset:0 length:len error:error]) {
    return NO;
  }

  if (![transport flush:error]) {
    return NO;
  }

  writeBuffer.length = HEADER_SIZE;

  return YES;
}

-(BOOL) write:(const UInt8 *)data offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  [writeBuffer appendBytes:data+offset length:length];

  return YES;
}

-(BOOL) readAll:(UInt8 *)buf offset:(UInt32)off length:(UInt32)len error:(NSError *__autoreleasing *)error
{
  if (readBuffer == nil) {

    if (![self readFrame:error]) {
      return NO;
    }

  }

  if (readBuffer != nil) {

    int buffer_len = (int)[readBuffer length];
    if (buffer_len-readOffset >= len) {

      [readBuffer getBytes:buf range:NSMakeRange(readOffset, len)];      // copy
                                                                         //  data
      readOffset += len;
    }
    else {

      // void the previous readBuffer data and request a new frame
      if (![self readFrame:error]) {
        return NO;
      }

      [readBuffer getBytes:buf range:NSMakeRange(0, len)];      // copy data

      readOffset = len;
    }

  }

  return YES;
}

-(BOOL) readFrame:(NSError **)error
{
  UInt8 i32rd[HEADER_SIZE];
  if (![transport readAll:i32rd offset:0 length:HEADER_SIZE error:error]) {
    return NO;
  }

  SInt32 size =
    ((i32rd[0] & 0xff) << 24) |
    ((i32rd[1] & 0xff) << 16) |
    ((i32rd[2] & 0xff) <<  8) |
    ((i32rd[3] & 0xff));

  if (readBuffer == nil) {

    readBuffer = [NSMutableData dataWithLength:size];

  }
  else {

    SInt32 len = (SInt32)readBuffer.length;
    if (len >= size) {

      readBuffer.length = size;

    }
    else {

      // increase length of data buffer
      [readBuffer increaseLengthBy:size-len];

    }

  }

  // copy into internal memory buffer
  if (![transport readAll:readBuffer.mutableBytes offset:0 length:size error:error]) {
    return NO;
  }

  return YES;
}

@end
