// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

import Foundation
import Thrift
import Fuzz

/// Generic parser that returns a parsed object from binary data - for use as a converter
public func parseObjectWithProtocol<P: TProtocol>(
    start: UnsafeRawPointer,
    count: Int,
    protocolType: P.Type) -> Fuzz.FuzzTest? {
    let data = Data(bytes: start, count: count)
    let transport = TMemoryBufferTransport(readBuffer: data)
    let proto = P(on: transport)
    
    do {
        return try Fuzz.FuzzTest.read(from: proto)
    } catch {
        return nil
    }
}

/// Test roundtrip serialization/deserialization with the specified protocol and conversion function
public func roundtripWithProtocol<P: TProtocol>(
    start: UnsafeRawPointer,
    count: Int,
    protocolType: P.Type
) -> Int32 {    
    // Try to convert data to a test object
    guard let testObj = parseObjectWithProtocol(start: start, count: count, protocolType: protocolType) else {
        return 0
    }
    
    // Now do a roundtrip test with the converted object
    do {
        // Serialize
        let writeTransport = TMemoryBufferTransport()
        let writeProto = P(on: writeTransport)
        
        try testObj.write(to: writeProto)
        try writeTransport.flush()
        
        // Deserialize
        let readTransport = TMemoryBufferTransport(readBuffer: writeTransport.writeBuffer)
        let readProto = P(on: readTransport)
        
        let deserialized = try Fuzz.FuzzTest.read(from: readProto)
        
        // This should always be true, but we check just to be sure
        guard deserialized == testObj else {
            fatalError("Roundtrip test failed: objects not equal after serialization/deserialization")
        }
        
    } catch {
        // Catch expected exceptions
    }
    
    return 0
}

/// Typedef for the fuzzer function signature required by libFuzzer
public typealias FuzzTarget = @convention(c) (UnsafeRawPointer, Int) -> Int32

// Import the libFuzzer driver function
@_silgen_name("LLVMFuzzerRunDriver")
public func LLVMFuzzerRunDriver(
    _ argc: UnsafeMutablePointer<Int32>, 
    _ argv: UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<CChar>>>,
    _ userCb: @escaping @convention(c) (UnsafeRawPointer, Int) -> Int32) -> Int32

// Run the libFuzzer driver with the given test function
// We use this to get around swift compilation issues, which create main functions that otherwise
// conflict with the libfuzzer main.
// See more documentation here: https://llvm.org/docs/LibFuzzer.html#using-libfuzzer-as-a-library 
public func runLibFuzzerDriver(testOneInput: @escaping FuzzTarget) -> Never {
    // Create C-style arguments to pass to LLVMFuzzerRunDriver
    var args = CommandLine.arguments.map { strdup($0) }
    var argc = Int32(args.count)
    var argv = args.map { UnsafeMutablePointer<CChar>($0!) }
    let argvPtr = UnsafeMutablePointer<UnsafeMutablePointer<CChar>>.allocate(capacity: args.count)
    argvPtr.initialize(from: &argv, count: args.count)
    let argvPtrPtr = UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<CChar>>>.allocate(capacity: 1)
    argvPtrPtr.pointee = argvPtr
    
    // Start the fuzzer engine with our test function
    let result = LLVMFuzzerRunDriver(&argc, argvPtrPtr, testOneInput)
    
    // Clean up
    argvPtrPtr.deallocate()
    argvPtr.deallocate()
    
    exit(Int32(result))
} 