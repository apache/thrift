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

#import <Foundation/Foundation.h>
#import "TAsyncTransport.h"



typedef BOOL (^THTTPSessionTransportRequestConfigBlock) (NSURLRequest *request, NSError *__autoreleasing *);
typedef BOOL (^THTTPSessionTransportTaskConfigBlock) (NSURLSessionDataTask *task, NSError *__autoreleasing *);
typedef NSError *(^THTTPSessionTransportResponseValidateBlock) (NSHTTPURLResponse *response, NSData *responseData);


@interface THTTPSessionTransportFactory : NSObject<TAsyncTransportFactory>

@property (strong, nonatomic) THTTPSessionTransportRequestConfigBlock requestConfig;
@property (strong, nonatomic) THTTPSessionTransportTaskConfigBlock taskConfig;
@property (strong, nonatomic) THTTPSessionTransportResponseValidateBlock responseValidate;

-(id) initWithSession:(NSURLSession *)session
                  URL:(NSURL *)aURL;

-(id) initWithSession:(NSURLSession *)session
                  URL:(NSURL *)aURL
            userAgent:(NSString *)userAgent
              timeout:(int)timeout;

@end


@interface THTTPSessionTransport : NSObject <TAsyncTransport>

@end
