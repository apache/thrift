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
#import "TTransportError.h"


@interface TNSStreamTransport ()
@end


@implementation TNSStreamTransport

-(id) initWithInputStream:(NSInputStream *)input
             outputStream:(NSOutputStream *)output
{
  self = [super init];
  if (self) {
    _input = input;
    _output = output;
  }
  return self;
}

-(id) initWithInputStream:(NSInputStream *)input
{
  return [self initWithInputStream:input outputStream:nil];
}

-(id) initWithOutputStream:(NSOutputStream *)output
{
  return [self initWithInputStream:nil outputStream:output];
}

-(BOOL) readAll:(UInt8 *)buf offset:(UInt32)off length:(UInt32)len error:(NSError *__autoreleasing *)error
{
  int got = 0;
  NSInteger total = 0;
  while (got < len) {

    total = [_input read:buf+off+got maxLength:len-got];
    if (total <= 0) {

      if (error) {
        *error = [NSError errorWithDomain:TTransportErrorDomain
                                     code:TTransportErrorStreamClosed
                                 userInfo:@{}];
      }
      return NO;

    }

    got += total;
  }

  return YES;
}


-(BOOL) write:(const UInt8 *)data offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  int got = 0;
  NSInteger total = 0;
  while (got < length) {

    total = [_output write:data+offset+got maxLength:length-got];
    if (total == -1) {
      if (error) {
        *error = [NSError errorWithDomain:TTransportErrorDomain
                                     code:TTransportErrorFailedWrite
                                 userInfo:@{}];
      }
      return NO;
    }
    else if (total == 0) {
      if (error) {
        *error = [NSError errorWithDomain:TTransportErrorDomain
                                     code:TTransportErrorEOF
                                 userInfo:@{}];
      }
      return NO;
    }

    got += total;
  }

  return YES;
}

-(BOOL) flush:(NSError *__autoreleasing *)error
{
  return YES;
}

@end
