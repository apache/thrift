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

#import "CalculatorHandler.h"

@interface CalculatorHandler ()

@property (nonatomic, strong) NSMutableDictionary *logDictionary;

@end

@implementation CalculatorHandler

@synthesize logDictionary = _logDictionary;

- (id)init
{
    self = [super init];
    if (self) {
        _logDictionary = [[NSMutableDictionary alloc] init];
    }
    return self;
}

#pragma mark - Calculator Service Functions

- (void) ping
{
    NSLog(@"ping()");
}

- (int32_t) add: (int32_t) num1 num2: (int32_t) num2
{
    NSLog(@"add(%d, %d)", num1, num2);
    return num1 + num2;
}

- (int32_t) calculate: (int32_t) logid w: (Work *) work
{
    int32_t value = 0;
    NSLog(@"calculate(%d, %@)", logid, work);
    switch (work.op) {
        case Operation_ADD:
            value = work.num1 + work.num2;
            break;
        case Operation_SUBTRACT:
            value = work.num1 - work.num2;
            break;
        case Operation_MULTIPLY:
            value = work.num1 * work.num2;
            break;
        case Operation_DIVIDE:
            if (work.num2 == 0) {
                [[[InvalidOperation alloc] initWithWhat:work.op
                                                    why:@"Cannot divide by 0"] raise];
            } else {
                value = work.num1 / work.num2;
            }
            break;
        default:
            [[[InvalidOperation alloc] initWithWhat:work.op
                                                why:@"Invalid operation"] raise];
            break;
    }

    NSNumber *result = [NSNumber numberWithInt:value];
    SharedStruct *sharedStruct = [[SharedStruct alloc] initWithKey:logid
                                                             value:[result stringValue]];
    [self.logDictionary setObject:sharedStruct
                           forKey:result];

    return value;
}

- (void) zip
{
    NSLog(@"zip()");
}

#pragma mark - Shared Service Functions

- (SharedStruct *) getStruct: (int32_t) key
{
    NSLog(@"getStruct(%d)", key);
    return [self.logDictionary objectForKey:[NSNumber numberWithInt:key]];
}

@end
