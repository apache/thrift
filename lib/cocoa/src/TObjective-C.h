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

/*
 * TObjective-C.h is for supporting coexistence of both the ARC (Automatic 
 * Reference Counting) mode and the Non-ARC mode of Objective-C 
 * in the same source code.
 *
 *                  2011/11/14  HIRANO Satoshi (AIST, Japan)
 *
 * Before:
 *
 *    var = [aObject retain];
 *    [aObject release];
 *    [aObject autorelease];
 *    [super dealloc];
 *    CFFunction(obj);
 *
 * ARC and Non-ARC compatible:
 *
 *    #import "TObjective-C.h"
 *    var = [aObject retain_stub];
 *    [aObject release_stub];
 *    [aObject autorelease_stub];
 *    [super dealloc_stub];
 *    CFFunction(bridge_stub obj);
 *
 *    Don't use retain_stub for @property(retain).
 *    Use NSAutoreleasePool like this:
 *        #if __has_feature(objc_arc)
 *          @autoreleasepool {
 *              // code 
 *          }
 *        #else
 *          NSAutoReleasePool *pool = [[NSAutoReleasePool alloc] init...
 *          // code
 *          [pool release];
 *        #endif
 */


#if !defined(retain_stub)
#if __has_feature(objc_arc)
#define retain_stub self
#define autorelease_stub self
#define release_stub self
#define dealloc_stub self
#define bridge_stub __bridge
#else
#define retain_stub retain
#define autorelease_stub autorelease
#define release_stub release
#define dealloc_stub dealloc
#define bridge_stub
#endif
#endif
