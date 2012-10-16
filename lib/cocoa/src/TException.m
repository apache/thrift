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

#import "TException.h"
#import "TObjective-C.h"

@implementation TException

+ (id) exceptionWithName: (NSString *) name
{
  return [self exceptionWithName: name reason: @"unknown" error: nil];
}


+ (id) exceptionWithName: (NSString *) name
                  reason: (NSString *) reason
{
  return [self exceptionWithName: name reason: reason error: nil];
}


+ (id) exceptionWithName: (NSString *) name
                  reason: (NSString *) reason
                   error: (NSError *) error
{
  NSDictionary * userInfo = nil;
  if (error != nil) {
    userInfo = [NSDictionary dictionaryWithObject: error forKey: @"error"];
  }

  return [super exceptionWithName: name
                reason: reason
                userInfo: userInfo];
}


- (NSString *) description
{
  NSMutableString * result = [NSMutableString stringWithString: [self name]];
  [result appendFormat: @": %@", [self reason]];
  if ([self userInfo] != nil) {
    [result appendFormat: @"\n  userInfo = %@", [self userInfo]];
  }

  return result;
}


@end
