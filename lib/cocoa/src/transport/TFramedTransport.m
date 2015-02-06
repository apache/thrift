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

#import "TFramedTransport.h"
#import "TTransportException.h"
#import "TObjective-C.h"

#define HEADER_SIZE 4
#define INIT_FRAME_SIZE 1024

@implementation TFramedTransport {
    NSMutableData* writeBuffer;
    NSMutableData* readBuffer;
    NSUInteger readOffset;
    uint8_t dummy_header[HEADER_SIZE];
}

- (id) initWithTransport:(id <TTransport>)transport
{
    mTransport = [transport retain_stub];
    readBuffer = nil;
    readOffset = 0;
    writeBuffer = [[NSMutableData alloc] initWithCapacity:INIT_FRAME_SIZE];
    [writeBuffer appendBytes:dummy_header length:HEADER_SIZE];
    return self;
}

- (void) dealloc
{
    [mTransport release_stub];
    [writeBuffer release_stub];
    if (readBuffer != nil)
        [readBuffer release_stub];
    [super dealloc_stub];
}

- (void)flush
{
    size_t headerAndDataLength = [writeBuffer length];
    if (headerAndDataLength < HEADER_SIZE) {
        @throw [TTransportException exceptionWithReason:@"Framed transport buffer has no header"];
    }

    size_t dataLength = headerAndDataLength - HEADER_SIZE;
    uint8_t i32rd[HEADER_SIZE];
    i32rd[0] = (uint8_t)(0xff & (dataLength >> 24));
    i32rd[1] = (uint8_t)(0xff & (dataLength >> 16));
    i32rd[2] = (uint8_t)(0xff & (dataLength >> 8));
    i32rd[3] = (uint8_t)(0xff & (dataLength));

    // should we make a copy of the writeBuffer instead? Better for threaded operations!
    [writeBuffer replaceBytesInRange:NSMakeRange(0, HEADER_SIZE) withBytes:i32rd length:HEADER_SIZE];
    [mTransport write:[writeBuffer mutableBytes] offset:0 length:headerAndDataLength];
    [mTransport flush];

    // reuse old memory buffer
    [writeBuffer setLength:0];
    [writeBuffer appendBytes:dummy_header length:HEADER_SIZE];
}

- (void) write: (const uint8_t *) data offset: (size_t) offset length: (size_t) length
{
    [writeBuffer appendBytes:data+offset length:length];
}

- (size_t) readAll: (uint8_t *) buf offset: (size_t) offset length: (size_t) length
{
    if (readBuffer == nil) {
        [self readFrame];
    }
    
    if (readBuffer != nil) {
        size_t bufferLength = [readBuffer length];
        if (bufferLength - readOffset >= length) {
            [readBuffer getBytes:buf range:NSMakeRange(readOffset,length)]; // copy data
            readOffset += length;
        } else {
            // void the previous readBuffer data and request a new frame
            [self readFrame];
            [readBuffer getBytes:buf range:NSMakeRange(0,length)]; // copy data
            readOffset = length;
        }
    }
    return length;
}

- (void)readFrame
{
    uint8_t i32rd[HEADER_SIZE];
    [mTransport readAll: i32rd offset: 0 length: HEADER_SIZE];
    int32_t headerValue =
        ((i32rd[0] & 0xff) << 24) |
        ((i32rd[1] & 0xff) << 16) |
        ((i32rd[2] & 0xff) <<  8) |
        ((i32rd[3] & 0xff));
    if (headerValue < 0) {
        NSString *reason = [NSString stringWithFormat:
                            @"Frame header reports negative frame size: %"PRId32,
                            headerValue];
        @throw [TTransportException exceptionWithReason:reason];
    }

    /* Cast should be safe:
     * Have verified headerValue non-negative and of lesser or equal bitwidth to size_t. */
    size_t frameSize = (size_t)headerValue;
    [self ensureReadBufferHasLength:frameSize];

    [mTransport readAll:[readBuffer mutableBytes] offset:0 length:frameSize];
}

- (void)ensureReadBufferHasLength:(size_t)length
{
    if (readBuffer == nil) {
        readBuffer = [[NSMutableData alloc] initWithLength:length];
    } else {
        size_t currentLength = [readBuffer length];
        BOOL isTooLong = (currentLength >= length);
        if (isTooLong) {
            [readBuffer setLength:length];
        } else {
            size_t lengthToAdd = length - currentLength;
            [readBuffer increaseLengthBy:lengthToAdd];
        }
    }
}

@end
