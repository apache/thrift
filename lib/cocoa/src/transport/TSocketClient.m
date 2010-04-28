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
#import "TSocketClient.h"

#if !TARGET_OS_IPHONE
#import <CoreServices/CoreServices.h>
#else
#import <CFNetwork/CFNetwork.h>
#endif

@implementation TSocketClient

- (id) initWithHostname: (NSString *) hostname
                   port: (int) port
{
	NSInputStream * inputStream = NULL;
	NSOutputStream * outputStream = NULL;
	CFReadStreamRef readStream = NULL;
	CFWriteStreamRef writeStream = NULL;
	CFStreamCreatePairWithSocketToHost(kCFAllocatorDefault, (CFStringRef)hostname, port, &readStream, &writeStream);
	if (readStream && writeStream) {
		CFReadStreamSetProperty(readStream, kCFStreamPropertyShouldCloseNativeSocket, kCFBooleanTrue);
		CFWriteStreamSetProperty(writeStream, kCFStreamPropertyShouldCloseNativeSocket, kCFBooleanTrue);
		
		inputStream = (NSInputStream *)readStream;
		[inputStream retain];
		[inputStream setDelegate:self];
		[inputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
		[inputStream open];
		
		outputStream = (NSOutputStream *)writeStream;
		[outputStream retain];
		[outputStream setDelegate:self];
		[outputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
		[outputStream open];
	}
	
	self = [super initWithInputStream: inputStream outputStream: outputStream];
	
	return self;
}


@end



