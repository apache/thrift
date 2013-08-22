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
#import <TMultiplexedProtocol.h>

#import <Foundation/Foundation.h>

#import <multiplexing.h>

int main(int argc, const char * argv[])
{

    @autoreleasepool {
        NSString *host = @"localhost";
        int port = 9090;

        // Make socket
        TSocketClient *transport = [[TSocketClient alloc] initWithHostname:host port:port];
        
        // Wrap in a protocol
        TBinaryProtocol *protocol = [[TBinaryProtocol alloc] initWithTransport:transport];
        
        // Embed the created protocol in multiplexed protocols
        TMultiplexedProtocol *calculatorMultiplexedProtocol = [[TMultiplexedProtocol alloc] initWithProtocol:protocol
                                                                                                 serviceName:@"Multiplexing_Calculator"];
        TMultiplexedProtocol *weatherReportMultiplexedProtocol = [[TMultiplexedProtocol alloc] initWithProtocol:protocol
                                                                                                    serviceName:@"Multiplexing_WeatherReport"];

        // Create a calculator and weahter report client to use the multiplexed protocol encoder
        Multiplexing_CalculatorClient *calculatorClient = [[Multiplexing_CalculatorClient alloc] initWithProtocol:calculatorMultiplexedProtocol];
        Multiplexing_WeatherReportClient *weatherReportClient = [[Multiplexing_WeatherReportClient alloc] initWithProtocol:weatherReportMultiplexedProtocol];

        @try {

            int sum = [calculatorClient add:1
                                          y:1];
            NSLog(@"1+1=%d", sum);

            double temperature = [weatherReportClient getTemperature];
            NSLog(@"Temperature=%f", temperature);

        }
        @catch (TTransportException *transportException) {
            NSLog(@"Error while connecting to %@:%d - Reason: %@", host, port, transportException.reason);
        }

    }
    return 0;
}
