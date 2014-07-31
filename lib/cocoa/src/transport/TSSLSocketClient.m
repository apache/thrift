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
#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>
#import "TSSLSocketClient.h"
#import "TSSLSocketException.h"
#import "TObjective-C.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#if !TARGET_OS_IPHONE
#import <CoreServices/CoreServices.h>
#else
#import <CFNetwork/CFNetwork.h>
#endif

@implementation TSSLSocketClient

- (id) initWithHostname: (NSString *) hostname
                   port: (int) port
{
    sslHostname = hostname;
	CFReadStreamRef readStream = NULL;
	CFWriteStreamRef writeStream = NULL;
    
    
    /* create a socket structure */
    struct sockaddr_in pin;
    struct hostent *hp = NULL;
    for(int i = 0; i < 10; i++) {
        

        
        if ((hp = gethostbyname([hostname UTF8String])) == NULL) { 
            NSLog(@"failed to resolve hostname %@", hostname);
            herror("resolv");
            if(i == 9) {
                @throw [TSSLSocketException exceptionWithReason: @"failed to resolve hostname"];
            }
            [NSThread sleepForTimeInterval:0.2];
        } else {
            break;
        }
    }
    
    memset (&pin, 0, sizeof(pin));
    pin.sin_family = AF_INET;
    pin.sin_addr.s_addr = ((struct in_addr *) (hp->h_addr))->s_addr;
    pin.sin_port = htons (port);
    
    /* create the socket */
    if ((sd = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1)
    {
        NSLog(@"failed to create socket for host %@:%d", hostname, port);
        @throw [TSSLSocketException exceptionWithReason: @"failed to create socket"];
    }
    
    /* open a connection */
    if (connect (sd, (struct sockaddr *) &pin, sizeof(pin)) == -1)
    {
        NSLog(@"failed to create conenct to host %@:%d", hostname, port);
        @throw [TSSLSocketException exceptionWithReason: @"failed to connect"];
    }
    CFStreamCreatePairWithSocket(kCFAllocatorDefault, sd, &readStream, &writeStream);
    
    CFReadStreamSetProperty(readStream, kCFStreamPropertyShouldCloseNativeSocket, kCFBooleanTrue);
    CFWriteStreamSetProperty(writeStream, kCFStreamPropertyShouldCloseNativeSocket, kCFBooleanTrue);
    
	if (readStream && writeStream) {
        CFReadStreamSetProperty(readStream,
                                kCFStreamPropertySocketSecurityLevel,
                                kCFStreamSocketSecurityLevelTLSv1);
        
        NSDictionary *settings =
        [NSDictionary dictionaryWithObjectsAndKeys:
         (id)kCFBooleanTrue, (id)kCFStreamSSLValidatesCertificateChain,
         nil];
        
        CFReadStreamSetProperty((CFReadStreamRef)readStream,
                                kCFStreamPropertySSLSettings,
                                (CFTypeRef)settings);
        CFWriteStreamSetProperty((CFWriteStreamRef)writeStream,
                                 kCFStreamPropertySSLSettings,
                                 (CFTypeRef)settings);
        
		inputStream = (bridge_stub NSInputStream *)readStream;
		[inputStream retain_stub];
		[inputStream setDelegate:self];
   		[inputStream scheduleInRunLoop:[NSRunLoop mainRunLoop] forMode:NSDefaultRunLoopMode];
		[inputStream open];
		
		outputStream = (bridge_stub NSOutputStream *)writeStream;
		[outputStream retain_stub];
		[outputStream setDelegate:self];
        [outputStream scheduleInRunLoop:[NSRunLoop mainRunLoop] forMode:NSDefaultRunLoopMode];
		[outputStream open];
        
        
        CFRelease(readStream);
        CFRelease(writeStream);
	}
    
    
	
	self = [super initWithInputStream: inputStream outputStream: outputStream];
    
	return self;
}

#pragma mark -
#pragma mark NSStreamDelegate
- (void)stream:(NSStream *)aStream
    handleEvent:(NSStreamEvent)eventCode {
    switch (eventCode) {
        case NSStreamEventNone:
            break;
        case NSStreamEventHasBytesAvailable:
            break;
        case NSStreamEventOpenCompleted:
            break;
        case NSStreamEventHasSpaceAvailable:
        {
            SecPolicyRef policy = SecPolicyCreateSSL(NO, (__bridge CFStringRef)(sslHostname));
            SecTrustRef trust = NULL;
            CFArrayRef streamCertificatesRef =
            CFBridgingRetain((__bridge id)((__bridge CFArrayRef)([aStream propertyForKey:(NSString *) kCFStreamPropertySSLPeerCertificates])));
            SecTrustCreateWithCertificates(CFBridgingRetain((__bridge id)(streamCertificatesRef)),
                                           policy,
                                           &trust);
            
            SecTrustResultType trustResultType = kSecTrustResultInvalid;
            SecTrustEvaluate(trust, &trustResultType);
            
            BOOL proceed = NO;
            switch (trustResultType) {
                case kSecTrustResultProceed:
                    proceed = YES;
                    break;
                case kSecTrustResultUnspecified:
                    NSLog(@"Trusted by OS");
                    proceed = YES;
                    break;
                case kSecTrustResultRecoverableTrustFailure:
                    proceed = recoverFromTrustFailure(trust);
                    break;
                case kSecTrustResultDeny:
                    NSLog(@"Deny");
                    break;
                case kSecTrustResultFatalTrustFailure:
                    NSLog(@"FatalTrustFailure");
                    break;
                case kSecTrustResultOtherError:
                    NSLog(@"OtherError");
                    break;
                case kSecTrustResultInvalid:
                    NSLog(@"Invalid");
                    break;
                default:
                    NSLog(@"Default");
                    break;
            }
            
            if (trust) {
                CFRelease(trust);
            }
            if (policy) {
                CFRelease(policy);
            }
            if (!proceed) {
                NSLog(@"Cannot trust certificate. TrustResultType: %u", trustResultType);
                [aStream close];
                @throw [TSSLSocketException exceptionWithReason: @"Cannot trust certificate"];
            }
        }
            break;
        case NSStreamEventErrorOccurred:
        {
            NSError *theError = [aStream streamError];
            NSLog(@"Error occured opening stream: %@", theError);
//            @throw [TSSLSocketException exceptionWithReason: @"Error occured opening stream" error: theError];
            break;
        }
        case NSStreamEventEndEncountered:
            break;
        default:
            break;
    }
}

bool recoverFromTrustFailure(SecTrustRef myTrust)
{
    
    SecTrustResultType trustResult;
    OSStatus status = SecTrustEvaluate(myTrust, &trustResult);
    
    CFAbsoluteTime trustTime,currentTime,timeIncrement,newTime;
    CFDateRef newDate;
    if (trustResult == kSecTrustResultRecoverableTrustFailure) {
        trustTime = SecTrustGetVerifyTime(myTrust);
        timeIncrement = 31536000;
        currentTime = CFAbsoluteTimeGetCurrent();
        newTime = currentTime - timeIncrement;
        if (trustTime - newTime){
            newDate = CFDateCreate(NULL, newTime);
            SecTrustSetVerifyDate(myTrust, newDate);
            status = SecTrustEvaluate(myTrust, &trustResult);
        }
    }
    if (trustResult != kSecTrustResultProceed) {
        NSLog(@"Certificate trust failure");
        return false;
    }
    return true;
}

- (void)close
{
    if(self.mInput) {
        //Close and reset inputstream
        CFReadStreamSetProperty((__bridge CFReadStreamRef)(self.mInput), kCFStreamPropertyShouldCloseNativeSocket, kCFBooleanTrue);
        [self.mInput setDelegate:nil];
        [self.mInput close];
        [self.mInput removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
        self.mInput = nil;
    }
    
    if(self.mOutput) {
        //Close and reset outputstream
        CFReadStreamSetProperty((__bridge CFReadStreamRef)(self.mOutput), kCFStreamPropertyShouldCloseNativeSocket, kCFBooleanTrue);
        [self.mOutput setDelegate:nil];
        [self.mOutput close];
        [self.mOutput removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
        self.mOutput = nil;
    }
}

- (BOOL) isOpen
{
    if(sd > 0)
        return TRUE;
    else
        return FALSE;
}

@end

