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

#import "THTTPSessionTransport.h"
#import "TTransportError.h"


@interface THTTPSessionTransportFactory () <NSURLSessionDelegate>

@property (strong, nonatomic) NSURLSession *session;
@property (strong, nonatomic) NSURLRequest *request;

@end


@interface THTTPSessionTransport ()

@property (strong, nonatomic) THTTPSessionTransportFactory *factory;
@property (strong, nonatomic) NSMutableData *requestData;
@property (strong, nonatomic) NSData *responseData;
@property (assign, nonatomic) NSUInteger responseDataOffset;

-(instancetype) initWithFactory:(THTTPSessionTransportFactory *)factory;

@end


@implementation THTTPSessionTransportFactory

+(NSURLRequest *) newRequestWithURL:(NSURL *)url userAgent:(NSString *)userAgent timeout:(NSTimeInterval)timeout
{
  // set up our request object that we'll use for each request
  NSMutableURLRequest *request = [[NSMutableURLRequest alloc] initWithURL:url];
  [request setHTTPMethod:@"POST"];
  [request setValue:@"application/x-thrift" forHTTPHeaderField:@"Content-Type"];
  [request setValue:@"application/x-thrift" forHTTPHeaderField:@"Accept"];

  NSString *validUserAgent = userAgent;
  if (!validUserAgent) {
    validUserAgent = @"Cocoa/THTTPSessionTransport";
  }
  [request setValue:validUserAgent forHTTPHeaderField:@"User-Agent"];

  [request setCachePolicy:NSURLRequestReloadIgnoringCacheData];

  if (timeout) {
    [request setTimeoutInterval:timeout];
  }

  return [request copy];
}


-(id) initWithSession:(NSURLSession *)session URL:(NSURL *)url
{
  return [self initWithSession:session
                           URL:url
                     userAgent:nil
                       timeout:0];
}


-(id) initWithSession:(NSURLSession *)session
                  URL:(NSURL *)url
            userAgent:(NSString *)userAgent
              timeout:(int)timeout
{
  self = [super init];
  if (self) {
    _session = session;
    _request = [THTTPSessionTransportFactory newRequestWithURL:url userAgent:userAgent timeout:timeout];
  }

  return self;
}

-(id<TAsyncTransport>) newTransport
{
  return [[THTTPSessionTransport alloc] initWithFactory:self];
}

-(NSURLRequest *) requestWithData:(NSData *)data error:(NSError *__autoreleasing *)error
{
  NSMutableURLRequest *newRequest = [_request mutableCopy];

  // Use stream to avoid unnecessary copy of data
  newRequest.HTTPBodyStream = [NSInputStream inputStreamWithData:data];

  if (_requestConfig) {
    if (!_requestConfig(newRequest, error)) {
      return nil;
    }
  }

  return newRequest;
}

-(NSURLSessionDataTask *) taskWithRequest:(NSURLRequest *)request
                        completionHandler:(void (^)(NSData *data, NSURLResponse *response, NSError *error))completionHandler
                                    error:(NSError *__autoreleasing *)error
{
  NSURLSessionDataTask *newTask = [_session dataTaskWithRequest:request completionHandler:completionHandler];
  if (!newTask) {
    if (error) {
      *error = [NSError errorWithDomain:TTransportErrorDomain
                                   code:TTransportErrorUnknown
                               userInfo:@{NSLocalizedDescriptionKey:@"Failed to create session data task"}];
    }
    return nil;
  }

  if (_taskConfig) {
    if (!_taskConfig(newTask, error)) {
      return nil;
    }
  }

  return newTask;
}

-(NSError *) validateResponse:(NSHTTPURLResponse *)response data:(NSData *)data
{
  if (_responseValidate) {
    return _responseValidate(response, data);
  }
  return nil;
}

@end



@implementation THTTPSessionTransport

-(instancetype) initWithFactory:(THTTPSessionTransportFactory *)factory
{
  self = [super init];
  if (self) {
    _factory = factory;
    _requestData = [NSMutableData dataWithCapacity:1024];
  }
  return self;
}

-(BOOL) readAll:(UInt8 *)buf offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  if (_responseData.length - _responseDataOffset < length) {
    if (error) {
      *error = [NSError errorWithDomain:TTransportErrorDomain
                                   code:TTransportErrorUnderflow
                               userInfo:nil];
    }
    return NO;
  }

  NSRange r;
  r.location = _responseDataOffset;
  r.length = length;

  [_responseData getBytes:buf+offset range:r];
  _responseDataOffset += length;

  return length;
}

-(BOOL) write:(const UInt8 *)data offset:(UInt32)offset length:(UInt32)length error:(NSError *__autoreleasing *)error
{
  if (!_requestData) {
    if (error) {
      *error = [NSError errorWithDomain:TTransportErrorDomain
                                   code:TTransportErrorFailedWrite
                               userInfo:@{NSLocalizedDescriptionKey:@"Write called out of order"}];
    }
    return NO;
  }

  [_requestData appendBytes:data+offset length:length];

  return YES;
}

-(void) flushWithCompletion:(TAsyncCompletionBlock)completed failure:(TAsyncFailureBlock)failure
{
  NSError *error;

  NSURLRequest *request = [_factory requestWithData:_requestData error:&error];
  if (!request) {
    failure(error);
    return;
  }

  _requestData = nil;

  NSURLSessionDataTask *task = [_factory taskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {

    if (!error) {
      // Check response type
      if (![response isKindOfClass:NSHTTPURLResponse.class]) {
        error = [NSError errorWithDomain:TTransportErrorDomain
                                    code:TTransportErrorInvalidHttpResponse
                                userInfo:@{}];
      }
      // Check status code
      NSHTTPURLResponse *httpResponse = (id)response;
      if (httpResponse.statusCode != 200) {
        error = [NSError errorWithDomain:TTransportErrorDomain
                                    code:TTransportErrorInvalidHttpStatus
                                userInfo:@{@"statusCode":@(httpResponse.statusCode)}];
      }
      // Allow factory to check
      error = [_factory validateResponse:httpResponse data:data];
    }

    _responseDataOffset = 0;

    if (error) {

      _responseData = nil;

      failure(error);

    }
    else {

      _responseData = data;

      completed(self);
    }

  } error:&error];

  if (!task) {
    failure(error);
    return;
  }

  [task resume];
}

-(BOOL) flush:(NSError *__autoreleasing *)error
{
  dispatch_semaphore_t completed = dispatch_semaphore_create(0);

  __block BOOL result;

  [self flushWithCompletion:^(id < TAsyncTransport > transport) {

    result = YES;

    dispatch_semaphore_signal(completed);

  } failure:^(NSError *asyncError) {

    if (error) {
      *error = asyncError;
    }

    result = NO;

    dispatch_semaphore_signal(completed);

  }];

  dispatch_semaphore_wait(completed, DISPATCH_TIME_FOREVER);

  return result;
}

@end
