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
#import "TProtocol.h"

enum {
  TApplicationException_UNKNOWN = 0,
  TApplicationException_UNKNOWN_METHOD = 1,
  TApplicationException_INVALID_MESSAGE_TYPE = 2,
  TApplicationException_WRONG_METHOD_NAME = 3,
  TApplicationException_BAD_SEQUENCE_ID = 4,
  TApplicationException_MISSING_RESULT = 5
};

// FIXME
@interface TApplicationException : TException {
  int mType;
}

+ (TApplicationException *) read: (id <TProtocol>) protocol;

- (void) write: (id <TProtocol>) protocol;

+ (TApplicationException *) exceptionWithType: (int) type
                                       reason: (NSString *) message;

@end
