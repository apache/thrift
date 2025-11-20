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
import Common

struct CalculatorError: Error {
    
}
class CalculatorService: Calculator {
    var resultMap = [Int32:SharedStruct]()
    
    func ping() throws {
        print("ping()")
    }
    
    func add(num1: Int32, num2: Int32) throws -> Int32 {
        print("add(\(num1), \(num2))")
        return num1 + num2
    }
    
    func calculate(logid: Int32, w: Work) throws -> Int32 {
        print("calculate(\(logid), \(w))")
        let result: Int32 = try {
        switch w.op {
        case .add:
            return w.num1 + w.num2
        case .subtract:
            return w.num1 - w.num2
        case .multiply:
            return w.num1 * w.num2
        case .divide:
            guard w.num2 != 0 else {
                throw InvalidOperation(whatOp: w.op.rawValue, why: "Cannot divide by 0")
            }
            return w.num1 / w.num2
        }
        }()
        
        let resultEntry = SharedStruct(key: logid, value: "\(result)")
        resultMap[logid] = resultEntry
        
        return result
    }
    
    func zip() throws {
        print("zip()")
    }
    
    func getStruct(key: Int32) throws -> SharedStruct {
        print("getStruct(\(key))")
        guard let entry = resultMap[key] else {
            throw CalculatorError()
        }
        return entry
    }
}
