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
    int len = [writeBuffer length];
    int data_len = len - HEADER_SIZE;
    if (data_len < 0)
        @throw [TTransportException exceptionWithReason:@"Framed transport buffer has no header"];

    uint8_t i32rd[HEADER_SIZE];
    i32rd[0] = (uint8_t)(0xff & (data_len >> 24));
    i32rd[1] = (uint8_t)(0xff & (data_len >> 16));
    i32rd[2] = (uint8_t)(0xff & (data_len >> 8));
    i32rd[3] = (uint8_t)(0xff & (data_len));

    // should we make a copy of the writeBuffer instead? Better for threaded operations!
    [writeBuffer replaceBytesInRange:NSMakeRange(0, HEADER_SIZE) withBytes:i32rd length:HEADER_SIZE];
    [mTransport write:[writeBuffer mutableBytes] offset:0 length:len];
    [mTransport flush];

    // reuse old memory buffer
    [writeBuffer setLength:0];
    [writeBuffer appendBytes:dummy_header length:HEADER_SIZE];
}

- (void)write:(const uint8_t *)data offset:(unsigned int)offset length:(unsigned int)length
{
    [writeBuffer appendBytes:data+offset length:length];
}

- (int)readAll:(uint8_t *)buf offset:(int)off length:(int)len {
    if (readBuffer == nil) {
        [self readFrame];
    }
    
    if (readBuffer != nil) {
        int buffer_len = [readBuffer length];
        if (buffer_len-readOffset >= len) {
            [readBuffer getBytes:buf range:NSMakeRange(readOffset,len)]; // copy data
            readOffset += len;
        } else {
            // void the previous readBuffer data and request a new frame
            [self readFrame];
            [readBuffer getBytes:buf range:NSMakeRange(0,len)]; // copy data
            readOffset = len;
        }
    }
    return len;
}

- (void)readFrame
{
    uint8_t i32rd[HEADER_SIZE];
    [mTransport readAll: i32rd offset: 0 length: HEADER_SIZE];
    int size =
        ((i32rd[0] & 0xff) << 24) |
        ((i32rd[1] & 0xff) << 16) |
        ((i32rd[2] & 0xff) <<  8) |
        ((i32rd[3] & 0xff));

    if (readBuffer == nil) {
        readBuffer = [[NSMutableData alloc] initWithLength:size];
    } else {
        int len = [readBuffer length];
        if (len >= size) {
            [readBuffer setLength:size];
        } else {
            // increase length of data buffer
            [readBuffer increaseLengthBy:size-len];
        }
    }
    // copy into internal memory buffer
    [mTransport readAll:[readBuffer mutableBytes] offset:0 length:size];
}

@end
