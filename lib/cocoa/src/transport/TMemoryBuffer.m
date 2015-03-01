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

#import "TMemoryBuffer.h"
#import "TTransportException.h"
#import "TObjective-C.h"

#define GARBAGE_BUFFER_SIZE 4096 // 4KiB

@implementation TMemoryBuffer
- (id)init {
	if ((self = [super init])) {
		mBuffer = [[NSMutableData alloc] init];
		mOffset = 0;
	}
	return self;
}

- (id)initWithData:(NSData *)data {
	if ((self = [super init])) {
		mBuffer = [data mutableCopy];
		mOffset = 0;
	}
	return self;
}

- (size_t) readAll: (uint8_t *) buf offset: (size_t) offset length: (size_t) length
{
	if ([mBuffer length] - mOffset < length) {
		@throw [TTransportException exceptionWithReason:@"Not enough bytes remain in buffer"];
	}
	[mBuffer getBytes:buf range:NSMakeRange(mOffset, length)];
	mOffset += length;
	if (mOffset >= GARBAGE_BUFFER_SIZE) {
		[mBuffer replaceBytesInRange:NSMakeRange(0, mOffset) withBytes:NULL length:0];
		mOffset = 0;
	}
	return length;
}

- (void) write: (const uint8_t *) data offset: (size_t) offset length: (size_t) length
{
	[mBuffer appendBytes:data+offset length:length];
}

- (void)flush {
	// noop
}

- (NSData *)getBuffer {
	return [[mBuffer copy] autorelease_stub];
}

- (void)dealloc {
	[mBuffer release_stub];
	[super dealloc_stub];
}
@end
