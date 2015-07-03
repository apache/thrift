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

#import "THTTPTransport.h"
#import "TTransportError.h"


@interface THTTPTransport () {
  NSURL *url;
  NSMutableURLRequest *request;
  NSMutableData *requestData;
  NSData *responseData;
  NSUInteger responseDataOffset;
  NSString *userAgent;
  int timeout;
}

@end


@implementation THTTPTransport

-(void) setupRequest
{
  // set up our request object that we'll use for each request
  request = [[NSMutableURLRequest alloc] initWithURL:url];
  [request setHTTPMethod:@"POST"];
  [request setValue:@"application/x-thrift" forHTTPHeaderField:@"Content-Type"];
  [request setValue:@"application/x-thrift" forHTTPHeaderField:@"Accept"];

  NSString *validUserAgent = userAgent;
  if (!validUserAgent) {
    validUserAgent = @"Cocoa/THTTPTransport";
  }
  [request setValue:validUserAgent forHTTPHeaderField:@"User-Agent"];

  [request setCachePolicy:NSURLRequestReloadIgnoringCacheData];
  if (timeout) {
    [request setTimeoutInterval:timeout];
  }
}


-(id) initWithURL:(NSURL *)aURL
{
  return [self initWithURL:aURL
                 userAgent:nil
                   timeout:0];
}


-(id) initWithURL:(NSURL *)aURL
        userAgent:(NSString *)aUserAgent
          timeout:(int)aTimeout
{
  self = [super init];
  if (!self) {
    return nil;
  }

  timeout = aTimeout;
  userAgent = aUserAgent;
  url = aURL;

  [self setupRequest];

  // create our request data buffer
  requestData = [[NSMutableData alloc] initWithCapacity:1024];

  return self;
}

-(void) setURL:(NSURL *)aURL
{
  url = aURL;

  [self setupRequest];
}

-(BOOL) readAll:(UInt8 *)buf offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  NSRange r;
  r.location = responseDataOffset;
  r.length = length;

  [responseData getBytes:buf+offset range:r];
  responseDataOffset += length;

  return length;
}

-(BOOL) write:(const UInt8 *)data offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  [requestData appendBytes:data+offset length:length];

  return YES;
}

-(BOOL) flush:(NSError *__autoreleasing *)error
{
  [request setHTTPBody:requestData];  // not sure if it copies the data

  responseDataOffset = 0;

  // make the HTTP request
  NSURLResponse *response;
  responseData = [NSURLConnection sendSynchronousRequest:request returningResponse:&response error:error];
  if (!responseData) {
    return NO;
  }

  [requestData setLength:0];

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

  return YES;
}

@end
