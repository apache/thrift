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

#import <TBinaryProtocol.h>
#import <TSocketClient.h>
#import <TTransportException.h>

#import <tutorial.h>

BOOL isOptionSetWrapperHelper(const char * option, int argc, const char * argv[], BOOL optionFound)
{
    return (argc <= 0 || optionFound) ? optionFound
                                      : isOptionSetWrapperHelper(option, argc - 1, argv, strcasecmp(option, argv[argc]) == 0);
}

BOOL isOptionSet(const char * option, int argc, const char * argv[])
{
    return isOptionSetWrapperHelper(option, argc, argv, NO);
}

int main(int argc, const char * argv[])
{

    @autoreleasepool {

        NSString *host = @"localhost";
        int port = 9090;

        // Make socket
        TSocketClient *transport = [[TSocketClient alloc] initWithHostname:host
                                                                      port:port
                                                                    useSSL:isOptionSet("-ssl", argc - 1, argv)
                                                               sslSettings:[NSDictionary dictionaryWithObjectsAndKeys:
                                                                            (NSString *)kCFStreamSocketSecurityLevelNegotiatedSSL, kCFStreamSSLLevel,
                                                                            kCFBooleanTrue, kCFStreamSSLAllowsExpiredCertificates,
                                                                            kCFBooleanTrue, kCFStreamSSLAllowsExpiredRoots,
                                                                            kCFBooleanTrue, kCFStreamSSLAllowsAnyRoot,
                                                                            kCFBooleanFalse, kCFStreamSSLValidatesCertificateChain,
                                                                            [NSNull null], kCFStreamSSLPeerName,
                                                                            nil]];

        // Wrap in a protocol
        TBinaryProtocol *protocol = [[TBinaryProtocol alloc] initWithTransport:transport];

        // Create a calculator client to use the protocol encoder
        CalculatorClient *calculatorClient = [[CalculatorClient alloc] initWithProtocol:protocol];

        @try {
            [calculatorClient ping];
            NSLog(@"ping()");

            int sum = [calculatorClient add:1
                                       num2:1];
            NSLog(@"1+1=%d", sum);

            Work *work = [[Work alloc] initWithNum1:1
                                               num2:0
                                                 op:Operation_DIVIDE
                                            comment:@"divide by zero"];

            @try {
                [calculatorClient calculate:1
                                          w:work];
                NSLog(@"Whoa? You know how to divide by zero?");
            }
            @catch (InvalidOperation *ioexception) {
                NSLog(@"InvalidOperation: %@", ioexception);
            }

            work = [[Work alloc] initWithNum1:15
                                         num2:10
                                           op:Operation_SUBTRACT
                                      comment:@"Subtract 15 - 10"];

            int diff = [calculatorClient calculate:1
                                                 w:work];
            NSLog(@"15-10=%d", diff);

            [calculatorClient zip];
            NSLog(@"zip()");

            @try {
                SharedStruct *sharedStruct = [calculatorClient getStruct:1];
                NSLog(@"Check log: %@", sharedStruct.value);
            }
            @catch (TApplicationException *applicationException) {
                NSLog(@"TApplicationException: %@", applicationException);
            }

        }
        @catch (TTransportException *transportException) {
            NSLog(@"Error while connecting to %@:%d - Reason: %@", host, port, transportException.reason);
        }
        
    }
    return 0;
}
