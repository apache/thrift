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
import Common

class ThriftTestImpl : ThriftTest {

  /// Prints "testVoid()" and returns nothing.
  ///
  /// - Throws: 
  func testVoid() throws {
    print("testVoid()")
  }

  /// Prints 'testString("%s")' with thing as '%s'
  /// @param string thing - the string to print
  /// @return string - returns the string 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: String
  /// - Throws: 
  func testString(thing: String) throws -> String {
    print("testString(\"\(thing)\")")
    return thing
  }

  /// Prints 'testBool("%s")' where '%s' with thing as 'true' or 'false'
  /// @param bool  thing - the bool data to print
  /// @return bool  - returns the bool 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Bool
  /// - Throws: 
  func testBool(thing: Bool) throws -> Bool {
    print("testBool\"(\(thing ? "true" : "false")\")")
    return thing
  }

  /// Prints 'testByte("%d")' with thing as '%d'
  /// The types i8 and byte are synonyms, use of i8 is encouraged, byte still exists for the sake of compatibility.
  /// @param byte thing - the i8/byte to print
  /// @return i8 - returns the i8/byte 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Int8
  /// - Throws: 
  func testByte(thing: Int8) throws -> Int8 {
    print("testByte(\"\(thing)\")")
    return thing
  }


  /// Prints 'testI32("%d")' with thing as '%d'
  /// @param i32 thing - the i32 to print
  /// @return i32 - returns the i32 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Int32
  /// - Throws: 
  func testI32(thing: Int32) throws -> Int32 {
    print("testI32(\"\(thing)\")")
    return thing
  }


  /// Prints 'testI64("%d")' with thing as '%d'
  /// @param i64 thing - the i64 to print
  /// @return i64 - returns the i64 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Int64
  /// - Throws: 
  func testI64(thing: Int64) throws -> Int64 {
    print("testI64(\"\(thing)\")")
    return thing
  }


  /// Prints 'testDouble("%f")' with thing as '%f'
  /// @param double thing - the double to print
  /// @return double - returns the double 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Double
  /// - Throws: 
  func testDouble(thing: Double) throws -> Double {
    print("testDouble(\"\(thing)\")")
    return thing
  }


  /// Prints 'testBinary("%s")' where '%s' is a hex-formatted string of thing's data
  /// @param binary  thing - the binary data to print
  /// @return binary  - returns the binary 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Data
  /// - Throws: 
  func testBinary(thing: Data) throws -> Data {
    print("testBinary(\"\(thing)\")")
    return thing
  }


  /// Prints 'testStruct("{%s}")' where thing has been formatted into a string of comma separated values
  /// @param Xtruct thing - the Xtruct to print
  /// @return Xtruct - returns the Xtruct 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Xtruct
  /// - Throws: 
  func testStruct(thing: Xtruct) throws -> Xtruct {
    print("testStruct({\([thing.string_thing, "\(thing.byte_thing)", "\(thing.i32_thing)", "\(thing.i64_thing)"].joined(separator: ", "))})")
    return thing
  }


  /// Prints 'testNest("{%s}")' where thing has been formatted into a string of the nested struct
  /// @param Xtruct2 thing - the Xtruct2 to print
  /// @return Xtruct2 - returns the Xtruct2 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Xtruct2
  /// - Throws: 
  func testNest(thing: Xtruct2) throws -> Xtruct2 {
    print("testNest(\(thing)")
    return thing
  }


  /// Prints 'testMap("{%s")' where thing has been formatted into a string of 'key => value' pairs
  ///  separated by commas and new lines
  /// @param map<i32,i32> thing - the map<i32,i32> to print
  /// @return map<i32,i32> - returns the map<i32,i32> 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: TMap<Int32, Int32>
  /// - Throws: 
  func testMap(thing: TMap<Int32, Int32>) throws -> TMap<Int32, Int32> {
    print("testMap(\(thing)")
    return thing
  }


  /// Prints 'testStringMap("{%s}")' where thing has been formatted into a string of 'key => value' pairs
  ///  separated by commas and new lines
  /// @param map<string,string> thing - the map<string,string> to print
  /// @return map<string,string> - returns the map<string,string> 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: TMap<String, String>
  /// - Throws: 
  func testStringMap(thing: TMap<String, String>) throws -> TMap<String, String> {
    print("testStringMap(\(thing)")
    return thing
  }


  /// Prints 'testSet("{%s}")' where thing has been formatted into a string of values
  ///  separated by commas and new lines
  /// @param set<i32> thing - the set<i32> to print
  /// @return set<i32> - returns the set<i32> 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: TSet<Int32>
  /// - Throws: 
  func testSet(thing: TSet<Int32>) throws -> TSet<Int32> {
    print("testSet\(thing)")
    return thing
  }


  /// Prints 'testList("{%s}")' where thing has been formatted into a string of values
  ///  separated by commas and new lines
  /// @param list<i32> thing - the list<i32> to print
  /// @return list<i32> - returns the list<i32> 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: TList<Int32>
  /// - Throws: 
  func testList(thing: TList<Int32>) throws -> TList<Int32> {
    print("testList\(thing)")
    return thing
  }


  /// Prints 'testEnum("%d")' where thing has been formatted into its numeric value
  /// @param Numberz thing - the Numberz to print
  /// @return Numberz - returns the Numberz 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: Numberz
  /// - Throws: 
  func testEnum(thing: Numberz) throws -> Numberz {
    print("testEnum\(thing.rawValue)")
    return thing
  }


  /// Prints 'testTypedef("%d")' with thing as '%d'
  /// @param UserId thing - the UserId to print
  /// @return UserId - returns the UserId 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: UserId
  /// - Throws: 
  func testTypedef(thing: UserId) throws -> UserId {
    print("testTypedef(\(thing)")
    return thing
  }


  /// Prints 'testMapMap("%d")' with hello as '%d'
  /// @param i32 hello - the i32 to print
  /// @return map<i32,map<i32,i32>> - returns a dictionary with these values:
  ///   {-4 => {-4 => -4, -3 => -3, -2 => -2, -1 => -1, }, 4 => {1 => 1, 2 => 2, 3 => 3, 4 => 4, }, }
  ///
  /// - Parameters:
  ///   - hello: 
  /// - Returns: TMap<Int32, TMap<Int32, Int32>>
  /// - Throws: 
  func testMapMap(hello: Int32) throws -> TMap<Int32, TMap<Int32, Int32>> {
    print("testMapMap(\(hello)")
    return TMap<Int32, TMap<Int32, Int32>>([
      -4: [-4: -4, -3: -3, -2: -2, -1: -1],
       4: [4: 4, 3: 3, 2: 2, 1: 1]
    ])
  }


  /// So you think you've got this all worked out, eh?
  /// Creates a map with these values and prints it out:
  ///   { 1 => { 2 => argument,
  ///            3 => argument,
  ///          },
  ///     2 => { 6 => <empty Insanity struct>, },
  ///   }
  /// @return map<UserId, map<Numberz,Insanity>> - a map with the above values
  ///
  /// - Parameters:
  ///   - argument: 
  /// - Returns: TMap<UserId, TMap<Numberz, Insanity>>
  /// - Throws: 
  func testInsanity(argument: Insanity) throws -> TMap<UserId, TMap<Numberz, Insanity>> {
    return TMap<UserId, TMap<Numberz, Insanity>>([
      1: [
        .two: argument,
        .three: argument
      ],
      2: [
        .six: Insanity(userMap: [:], xtructs: [])
      ]
    ])
  }


  /// Prints 'testMulti()'
  /// @param i8 arg0 -
  /// @param i32 arg1 -
  /// @param i64 arg2 -
  /// @param map<i16, string> arg3 -
  /// @param Numberz arg4 -
  /// @param UserId arg5 -
  /// @return Xtruct - returns an Xtruct with string_thing = "Hello2, byte_thing = arg0, i32_thing = arg1
  ///    and i64_thing = arg2
  ///
  /// - Parameters:
  ///   - arg0: 
  ///   - arg1: 
  ///   - arg2: 
  ///   - arg3: 
  ///   - arg4: 
  ///   - arg5: 
  /// - Returns: Xtruct
  /// - Throws: 
  func testMulti(arg0: Int8, arg1: Int32, arg2: Int64, arg3: TMap<Int16, String>, arg4: Numberz, arg5: UserId) throws -> Xtruct {
    print("testMulti()")
    return Xtruct(string_thing: "Hello2", byte_thing: arg0, i32_thing: arg1, i64_thing: arg2)
  }


  /// Print 'testException(%s)' with arg as '%s'
  /// @param string arg - a string indication what type of exception to throw
  /// if arg == "Xception" throw Xception with errorCode = 1001 and message = arg
  /// else if arg == "TException" throw TException
  /// else do not throw anything
  ///
  /// - Parameters:
  ///   - arg: 
  /// - Throws: Xception
  func testException(arg: String) throws {
    print("testException(\(arg)")
    if arg == "Xception" {
      throw Xception(errorCode: 1001, message: arg)
    } else if arg == "TException" {
      throw TApplicationError() // is type TError (TException Swift equiv)
    }
  }


  /// Print 'testMultiException(%s, %s)' with arg0 as '%s' and arg1 as '%s'
  /// @param string arg - a string indicating what type of exception to throw
  /// if arg0 == "Xception" throw Xception with errorCode = 1001 and message = "This is an Xception"
  /// else if arg0 == "Xception2" throw Xception2 with errorCode = 2002 and struct_thing.string_thing = "This is an Xception2"
  /// else do not throw anything
  /// @return Xtruct - an Xtruct with string_thing = arg1
  ///
  /// - Parameters:
  ///   - arg0: 
  ///   - arg1: 
  /// - Returns: Xtruct
  /// - Throws: Xception, Xception2
  func testMultiException(arg0: String, arg1: String) throws -> Xtruct {
    print("testMultiException(\(arg0), \(arg1)")
    if arg0 == "Xception" {
      throw Xception(errorCode: 1001, message: "This is an Xception")
    } else if arg0 == "Xception2" {
      throw Xception2(errorCode: 2002, struct_thing: Xtruct(string_thing: "This is an Xception2", byte_thing: 0, i32_thing: 0, i64_thing: 0))
    }
    return Xtruct(string_thing: arg1, byte_thing: 0, i32_thing: 0, i64_thing: 0)
  }


  /// Print 'testOneway(%d): Sleeping...' with secondsToSleep as '%d'
  /// sleep 'secondsToSleep'
  /// Print 'testOneway(%d): done sleeping!' with secondsToSleep as '%d'
  /// @param i32 secondsToSleep - the number of seconds to sleep
  ///
  /// - Parameters:
  ///   - secondsToSleep: 
  /// - Throws: 
  func testOneway(secondsToSleep: Int32) throws {
    print("testOneway(\(secondsToSleep): Sleeping...")
    Thread.sleep(forTimeInterval: TimeInterval(secondsToSleep))
  }
  
  func testUuid(thing: UUID) throws -> UUID {
    print("testUuid(\(thing))")
    return thing
  }
}

class SecondServiceImpl : SecondService {

  /// Prints 'testString("%s")' with thing as '%s'
  /// @param string thing - the string to print
  /// @return string - returns the string 'thing'
  ///
  /// - Parameters:
  ///   - thing: 
  /// - Returns: String
  /// - Throws: 
  func secondtestString(thing: String) throws -> String {
    print("testString(\"\(thing)\")")
    return thing
  }
}

