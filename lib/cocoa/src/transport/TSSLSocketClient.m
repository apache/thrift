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
#import "TSSLSocketClientError.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#if !TARGET_OS_IPHONE
#import <CoreServices/CoreServices.h>
#else
#import <CFNetwork/CFNetwork.h>
#endif

@interface TSSLSocketClient () {
  NSInputStream *inputStream;
  NSOutputStream *outputStream;
  NSString *sslHostname;
  int sd;
}

@end


@implementation TSSLSocketClient

-(id) initWithHostname:(NSString *)hostname
                  port:(int)port
                 error:(NSError **)error
{
  sslHostname = hostname;
  CFReadStreamRef readStream = NULL;
  CFWriteStreamRef writeStream = NULL;


  /* create a socket structure */
  struct sockaddr_in pin;
  struct hostent *hp = NULL;
  for (int i = 0; i < 10; i++) {



    if ((hp = gethostbyname([hostname UTF8String])) == NULL) {
      NSLog(@"failed to resolve hostname %@", hostname);
      herror("resolv");
      if (i == 9) {
        if (error) {
          *error = [NSError errorWithDomain:TSSLSocketClientErrorDomain
                                       code:TSSLSocketClientErrorHostanameResolution
                                   userInfo:nil];
        }
        return nil;
      }
      [NSThread sleepForTimeInterval:0.2];
    }
    else {
      break;
    }
  }

  memset(&pin, 0, sizeof(pin));
  pin.sin_family = AF_INET;
  memcpy(&pin.sin_addr, hp->h_addr, sizeof(struct in_addr));
  pin.sin_port = htons(port);

  /* create the socket */
  if ((sd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1) {
    NSLog(@"failed to create socket for host %@:%d", hostname, port);
    if (error) {
      *error = [NSError errorWithDomain:TSSLSocketClientErrorDomain
                                   code:TSSLSocketClientErrorSocketCreate
                               userInfo:nil];
    }
    return nil;
  }

  /* open a connection */
  if (connect(sd, (struct sockaddr *)&pin, sizeof(pin)) == -1) {
    NSLog(@"failed to create conenct to host %@:%d", hostname, port);
    if (error) {
      *error = [NSError errorWithDomain:TSSLSocketClientErrorDomain
                                   code:TSSLSocketClientErrorConnect
                               userInfo:nil];
    }
    return nil;
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

    inputStream = (__bridge NSInputStream *)readStream;
    [inputStream setDelegate:self];
    [inputStream scheduleInRunLoop:[NSRunLoop mainRunLoop] forMode:NSDefaultRunLoopMode];
    [inputStream open];

    outputStream = (__bridge NSOutputStream *)writeStream;
    [outputStream setDelegate:self];
    [outputStream scheduleInRunLoop:[NSRunLoop mainRunLoop] forMode:NSDefaultRunLoopMode];
    [outputStream open];


    CFRelease(readStream);
    CFRelease(writeStream);
  }

  self = [super initWithInputStream:inputStream outputStream:outputStream];

  return self;
}

#pragma mark -
#pragma mark NSStreamDelegate

-(void) stream:(NSStream *)aStream
   handleEvent:(NSStreamEvent)eventCode
{
  switch (eventCode) {
  case NSStreamEventNone:
    break;

  case NSStreamEventHasBytesAvailable:
    break;

  case NSStreamEventOpenCompleted:
    break;

  case NSStreamEventHasSpaceAvailable: {
    SecPolicyRef policy = SecPolicyCreateSSL(NO, (__bridge CFStringRef)(sslHostname));
    SecTrustRef trust = NULL;
    CFArrayRef streamCertificatesRef = (__bridge CFArrayRef)[aStream propertyForKey:(NSString *)kCFStreamPropertySSLPeerCertificates];
    SecTrustCreateWithCertificates(streamCertificatesRef, policy, &trust);

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
    }
  }
  break;

  case NSStreamEventErrorOccurred: {
    NSError *theError = [aStream streamError];
    NSLog(@"Error occurred opening stream: %@", theError);
//            @throw [TSSLSocketException exceptionWithReason: @"Error occurred
//  opening stream" error: theError];
    break;
  }

  case NSStreamEventEndEncountered:
    break;
  }
}

BOOL recoverFromTrustFailure(SecTrustRef myTrust)
{

  SecTrustResultType trustResult;
  OSStatus status = SecTrustEvaluate(myTrust, &trustResult);
  if (status != errSecSuccess) {
    return NO;
  }

  CFAbsoluteTime trustTime, currentTime, timeIncrement, newTime;
  if (trustResult == kSecTrustResultRecoverableTrustFailure) {

    trustTime = SecTrustGetVerifyTime(myTrust);
    timeIncrement = 31536000;
    currentTime = CFAbsoluteTimeGetCurrent();
    newTime = currentTime - timeIncrement;

    if (trustTime - newTime) {

      CFDateRef newDate = CFDateCreate(NULL, newTime);
      SecTrustSetVerifyDate(myTrust, newDate);
      CFRelease(newDate);

      status = SecTrustEvaluate(myTrust, &trustResult);
      if (status != errSecSuccess) {
        return NO;
      }

    }
  }
  if (trustResult != kSecTrustResultProceed || trustResult != kSecTrustResultUnspecified) {
    NSLog(@"Certificate trust failure");
    return NO;
  }
  return YES;
}

-(BOOL) isOpen
{
  if (sd > 0) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}

@end

