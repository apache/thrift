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

#import "THTTPClient.h"
#import "TTransportError.h"


@interface THTTPClient () {
  NSURL *mURL;
  NSMutableURLRequest *mRequest;
  NSMutableData *mRequestData;
  NSData *mResponseData;
  size_t mResponseDataOffset;
  NSString *mUserAgent;
  int mTimeout;
}

@end


@implementation THTTPClient

-(void) setupRequest
{
  // set up our request object that we'll use for each request
  mRequest = [[NSMutableURLRequest alloc] initWithURL:mURL];
  [mRequest setHTTPMethod:@"POST"];
  [mRequest setValue:@"application/x-thrift" forHTTPHeaderField:@"Content-Type"];
  [mRequest setValue:@"application/x-thrift" forHTTPHeaderField:@"Accept"];

  NSString *userAgent = mUserAgent;
  if (!userAgent) {
    userAgent = @"Cocoa/THTTPClient";
  }
  [mRequest setValue:userAgent forHTTPHeaderField:@"User-Agent"];

  [mRequest setCachePolicy:NSURLRequestReloadIgnoringCacheData];
  if (mTimeout) {
    [mRequest setTimeoutInterval:mTimeout];
  }
}


-(id) initWithURL:(NSURL *)aURL
{
  return [self initWithURL:aURL
                 userAgent:nil
                   timeout:0];
}


-(id) initWithURL:(NSURL *)aURL
        userAgent:(NSString *)userAgent
          timeout:(int)timeout
{
  self = [super init];
  if (!self) {
    return nil;
  }

  mTimeout = timeout;
  mUserAgent = userAgent;
  mURL = aURL;

  [self setupRequest];

  // create our request data buffer
  mRequestData = [[NSMutableData alloc] initWithCapacity:1024];

  return self;
}

-(void) setURL:(NSURL *)aURL
{
  mURL = aURL;

  [self setupRequest];
}

-(BOOL) readAll:(UInt8 *)buf offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  NSRange r;
  r.location = mResponseDataOffset;
  r.length = length;

  [mResponseData getBytes:buf+offset range:r];
  mResponseDataOffset += length;

  return length;
}

-(BOOL) write:(const UInt8 *)data offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  [mRequestData appendBytes:data+offset length:length];

  return YES;
}

-(BOOL) flush:(NSError *__autoreleasing *)error
{
  [mRequest setHTTPBody:mRequestData];  // not sure if it copies the data

  // make the HTTP request
  NSURLResponse *response;
  NSData *responseData =
    [NSURLConnection sendSynchronousRequest:mRequest returningResponse:&response error:error];
  if (!responseData) {
    return NO;
  }

  [mRequestData setLength:0];

  if (![response isKindOfClass:NSHTTPURLResponse.class]) {
    if (error) {
      *error = [NSError errorWithDomain:TTransportErrorDomain
                                   code:TTransportErrorInvalidHttpResponse
                               userInfo:@{}];
    }
    return NO;
  }

  NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
  if ([httpResponse statusCode] != 200) {
    if (error) {
      *error = [NSError errorWithDomain:TTransportErrorDomain
                                   code:TTransportErrorInvalidHttpStatus
                               userInfo:@{@"statusCode":@(httpResponse.statusCode)}];
    }
    return NO;
  }

  // phew!
  mResponseData = responseData;
  mResponseDataOffset = 0;

  return YES;
}

@end
