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

#import <tutorial.h>

@interface CalculatorHandler : NSObject <Calculator, SharedService>

- (void) ping;  // throws TException
- (int32_t) add: (int32_t) num1 num2: (int32_t) num2;  // throws TException
- (int32_t) calculate: (int32_t) logid w: (Work *) w;  // throws InvalidOperation *, TException
- (void) zip;  // throws TException

- (SharedStruct *) getStruct: (int32_t) key;  // throws TException

@end
