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
#import "TTransportException.h"
#import "TObjective-C.h"

@implementation THTTPClient


- (void) setupRequest
{
  if (mRequest != nil) {
    [mRequest release_stub];
  }

  // set up our request object that we'll use for each request
  mRequest = [[NSMutableURLRequest alloc] initWithURL: mURL];
  [mRequest setHTTPMethod: @"POST"];
  [mRequest setValue: @"application/x-thrift" forHTTPHeaderField: @"Content-Type"];
  [mRequest setValue: @"application/x-thrift" forHTTPHeaderField: @"Accept"];

  NSString * userAgent = mUserAgent;
  if (!userAgent) {
    userAgent = @"Cocoa/THTTPClient";
  }
  [mRequest setValue: userAgent forHTTPHeaderField: @"User-Agent"];

  [mRequest setCachePolicy: NSURLRequestReloadIgnoringCacheData];
  if (mTimeout) {
    [mRequest setTimeoutInterval: mTimeout];
  }
}


- (id) initWithURL: (NSURL *) aURL
{
  return [self initWithURL: aURL
                 userAgent: nil
                   timeout: 0];
}


- (id) initWithURL: (NSURL *) aURL
         userAgent: (NSString *) userAgent
           timeout: (int) timeout
{
  self = [super init];
  if (!self) {
    return nil;
  }

  mTimeout = timeout;
  if (userAgent) {
    mUserAgent = [userAgent retain_stub];
  }
  mURL = [aURL retain_stub];

  [self setupRequest];

  // create our request data buffer
  mRequestData = [[NSMutableData alloc] initWithCapacity: 1024];

  return self;
}


- (void) setURL: (NSURL *) aURL
{
  [aURL retain_stub];
  [mURL release_stub];
  mURL = aURL;

  [self setupRequest];
}


- (void) dealloc
{
  [mURL release_stub];
  [mUserAgent release_stub];
  [mRequest release_stub];
  [mRequestData release_stub];
  [mResponseData release_stub];
  [super dealloc_stub];
}


- (size_t) readAll: (uint8_t *) buf offset: (size_t) offset length: (size_t) length
{
  NSRange r;
  r.location = mResponseDataOffset;
  r.length = length;

  [mResponseData getBytes: buf+offset range: r];
  mResponseDataOffset += length;

  return length;
}


- (void) write: (const uint8_t *) data offset: (size_t) offset length: (size_t) length
{
  [mRequestData appendBytes: data+offset length: length];
}


- (void) flush
{
  [mRequest setHTTPBody: mRequestData]; // not sure if it copies the data

  // make the HTTP request
  NSURLResponse * response;
  NSError * error;
  NSData * responseData =
    [NSURLConnection sendSynchronousRequest: mRequest returningResponse: &response error: &error];

  [mRequestData setLength: 0];

  if (responseData == nil) {
    @throw [TTransportException exceptionWithName: @"TTransportException"
                                reason: @"Could not make HTTP request"
                                error: error];
  }
  if (![response isKindOfClass: [NSHTTPURLResponse class]]) {
    @throw [TTransportException exceptionWithName: @"TTransportException"
                                           reason: [NSString stringWithFormat: @"Unexpected NSURLResponse type: %@",
                                                    NSStringFromClass([response class])]];
  }

  NSHTTPURLResponse * httpResponse = (NSHTTPURLResponse *) response;
  if ([httpResponse statusCode] != 200) {
    @throw [TTransportException exceptionWithName: @"TTransportException"
                                           reason: [NSString stringWithFormat: @"Bad response from HTTP server: %ld",
                                                    (long)[httpResponse statusCode]]];
  }

  // phew!
  [mResponseData release_stub];
  mResponseData = [responseData retain_stub];
  mResponseDataOffset = 0;
}


@end
