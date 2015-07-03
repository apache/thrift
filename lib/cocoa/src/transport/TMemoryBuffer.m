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

#import "TMemoryBuffer.h"
#import "TTransportError.h"


#define GARBAGE_BUFFER_SIZE 4096 // 4KiB


@interface TMemoryBuffer () {
  NSMutableData *mBuffer;
  UInt32 mOffset;
}

@end


@implementation TMemoryBuffer

-(id) init
{
  if ((self = [super init])) {
    mBuffer = [NSMutableData new];
    mOffset = 0;
  }
  return self;
}

-(id) initWithData:(NSData *)data
{
  if (self = [super init]) {
    mBuffer = [data mutableCopy];
    mOffset = 0;
  }
  return self;
}

-(BOOL) readAll:(UInt8 *)buf offset:(UInt32)off length:(UInt32)len error:(NSError *__autoreleasing *)error
{
  if ((mBuffer.length - mOffset) < len) {
    if (error) {
      *error = [NSError errorWithDomain:TTransportErrorDomain
                                   code:TTransportErrorNoFrameHeader
                               userInfo:@{}];
    }
    return NO;
  }

  [mBuffer getBytes:buf range:NSMakeRange(mOffset, len)];
  mOffset += len;

  if (mOffset >= GARBAGE_BUFFER_SIZE) {
    [mBuffer replaceBytesInRange:NSMakeRange(0, mOffset) withBytes:NULL length:0];
    mOffset = 0;
  }

  return YES;
}

-(BOOL) write:(const UInt8 *)data offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  [mBuffer appendBytes:data+offset length:length];

  return YES;
}

-(BOOL)flush:(NSError *__autoreleasing *)error
{
  return YES;
}

-(NSData *) buffer
{
  return [mBuffer copy];
}

@end
