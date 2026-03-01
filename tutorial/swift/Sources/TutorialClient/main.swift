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

import Foundation
import Thrift
import Common

let transport = try TSocketTransport(hostname: "localhost", port: 9090)
let inOutProto = TBinaryProtocol(on: transport)
let client = CalculatorClient(inoutProtocol: inOutProto)

try client.ping()
print("1+1= \(try client.add(num1: 1, num2: 1))")

let work = Work(num1: 1, num2: 0, op: .divide)
do {
    _ = try client.calculate(logid: 1, w: work)
    assertionFailure("Hm... shouldn't be able to divide by zero")
} catch let error as InvalidOperation {
    print("Invalid operation: \(error.why)")
}

work.op = .subtract
work.num1 = 15
work.num2 = 10

print("15-10= \(try client.calculate(logid: 1, w: work))")

print("Done!")
