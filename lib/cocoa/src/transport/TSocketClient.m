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
#import "TObjective-C.h"

#if !TARGET_OS_IPHONE
#import <CoreServices/CoreServices.h>
#else
#import <CFNetwork/CFNetwork.h>
#endif

@interface TSocketClient ()
{
    NSInputStream * inputStream;
	NSOutputStream * outputStream;
}
@end

@implementation TSocketClient

- (id) initWithHostname: (NSString *) hostname
                   port: (int) port
{
    return [self initWithHostname: hostname
                             port: port
                           useSSL: NO];
}

- (id) initWithHostname: (NSString *) hostname
                   port: (int) port
                 useSSL: (bool) useSSL
{
    return [self initWithHostname: hostname
                             port: port
                           useSSL: useSSL
                      sslSettings: nil];
}

- (id) initWithHostname: (NSString *) hostname
                   port: (int) port
                 useSSL: (bool) useSSL
            sslSettings: (NSDictionary *) sslSettings
{
	inputStream = NULL;
	outputStream = NULL;
	CFReadStreamRef readStream = NULL;
	CFWriteStreamRef writeStream = NULL;
	CFStreamCreatePairWithSocketToHost(kCFAllocatorDefault, (bridge_stub CFStringRef)hostname, port, &readStream, &writeStream);
	if (readStream && writeStream) {
		CFReadStreamSetProperty(readStream, kCFStreamPropertyShouldCloseNativeSocket, kCFBooleanTrue);
		CFWriteStreamSetProperty(writeStream, kCFStreamPropertyShouldCloseNativeSocket, kCFBooleanTrue);
		
		inputStream = (bridge_stub NSInputStream *)readStream;
		[inputStream retain_stub];
		[inputStream setDelegate:self];
		[inputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
		
		outputStream = (bridge_stub NSOutputStream *)writeStream;
		[outputStream retain_stub];
		[outputStream setDelegate:self];
		[outputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];

        if (useSSL) {
            // configure with default settings if the sslSettings are not set
            sslSettings = (sslSettings) ? sslSettings : [NSDictionary dictionaryWithObjectsAndKeys:
                                                         (NSString *)kCFStreamSocketSecurityLevelNegotiatedSSL, kCFStreamSSLLevel,
                                                         kCFBooleanFalse, kCFStreamSSLAllowsExpiredRoots,
                                                         kCFBooleanFalse, kCFStreamSSLAllowsAnyRoot,
                                                         kCFBooleanTrue, kCFStreamSSLValidatesCertificateChain,
                                                         nil];
            [inputStream setProperty:sslSettings
                              forKey:(NSString *)kCFStreamPropertySSLSettings];
            [outputStream setProperty:sslSettings
                               forKey:(NSString *)kCFStreamPropertySSLSettings];
        }

        [inputStream open];
        [outputStream open];

        CFRelease(readStream);
        CFRelease(writeStream);
	}
	
	self = [super initWithInputStream: inputStream outputStream: outputStream];
	
	return self;
}

-(void)dealloc
{
    [inputStream close];
    [inputStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [inputStream setDelegate:nil];
    [inputStream release_stub];
    
    [outputStream close];
    [outputStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [outputStream setDelegate:nil];
    [outputStream release_stub];
    [super dealloc_stub];
}


@end



