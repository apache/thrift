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
import XCTest

enum Error: Int32 {
  case baseTypes = 1
  case structs = 2
  case containers = 4
  case exceptions = 8
  case unknown = 64
  case timeout = 128
}

class TestClient {
  var client: ThriftTestClient
  var resultCode: Int32 = 0

  public init(parameters: TestClientParameters) throws {
    let transport = try TestClient.getTransport(parameters: parameters)  
    let proto = try TestClient.getProtocol(parameters: parameters, transport: transport)
    client = ThriftTestClient(inoutProtocol: proto)
  }

  static func getTransport(parameters: TestClientParameters) throws -> TTransport {
      let socketTransport: TTransport = try { () throws -> TTransport in
          if let domainSocket = parameters.domainSocket {
            return try TSocketTransport(path: domainSocket)
          }
          return try TSocketTransport(hostname: parameters.host!, port: parameters.port!)
      }()
    if parameters.transport == .framed {
      return TFramedTransport(transport: socketTransport)
    } 

    if parameters.transport == .buffered {
      return socketTransport
    }

    throw ParserError.unsupportedOption
  }

  static func getProtocol(parameters: TestClientParameters, transport: TTransport) throws -> TProtocol {
    if parameters.proto == .binary {
      return TBinaryProtocol(on: transport)
    } 

    if parameters.proto == .compact {
      return TCompactProtocol(on: transport)
    }

    throw ParserError.unsupportedOption
  }

  func run() throws {
    do {
      try testVoid()
      try testString()
      try testBool()
      try testByte()
      try testI32()
      try testI64()
      try testDouble()
      try testBinary()
      try testStruct()
      try testNest()
      try testMap()
      try testSet()
      try testList()
      try testEnum()
      try testTypedef()
      try testMapMap()
      try testInsanity()
      try testMulti()
      try testException()
      try testMultiException()
      // Swift generator doesn't yet support one way functions (THRIFT-5468)
      /*try testOneway()
       try testOnewayThenNormal()*/
      try testUuid()
      
    } catch let error {
      print("\(error)")
      resultCode |= Error.unknown.rawValue
    }
    exit(resultCode)
  }

  func testVoid() throws {
    print("testVoid")
    try client.testVoid()
  }


  func testString1(_ s1: String) throws {
    print("testString(\(s1))")
    let r1 = try client.testString(thing: s1)
    print(r1)
    if s1 != r1 {
      resultCode |= Error.baseTypes.rawValue
    }
  }

  func testString() throws {
    try testString1(String(repeating: "Python", count: 20))
    try testString1("")
    try testString1("\t\n/\\\\\r{}:パイソン")
    try testString1("""
Afrikaans, Alemannisch, Aragonés, العربية, مصرى,
Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška,
Беларуская, Беларуская (тарашкевіца), Български, Bamanankan,
বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн,
Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg,
Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English,
Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt,
Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego,
Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski,
Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia,
Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa,
ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар,
Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino,
Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa
Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, مازِرونی, Bahasa
Melayu, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪
Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad,
Occitan, Иронау, Papiamentu, Deitsch, Polski, پنجابی, پښتو,
Norfuk / Pitkern, Português, Runa Simi, Rumantsch, Romani, Română,
Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple
English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk,
Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog,
Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük,
Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文,
Bân-lâm-gú, 粵語
""")
  }
  
  func testBool1(_ s1: Bool) throws {
    print("testBool(\(s1))")
    let r1 = try client.testBool(thing: s1)
    print(r1)
    if s1 != r1 {
      resultCode |= Error.baseTypes.rawValue
    }
  }
  
  func testBool() throws {
    try testBool1(true)
    try testBool1(false)
  }
  
  func testByte() throws {
    print("testByte")
    if try client.testByte(thing: 63) != 63 {
      resultCode |= Error.baseTypes.rawValue
    }
    if try client.testByte(thing: -127) != -127 {
      resultCode |= Error.baseTypes.rawValue
    }
  }
  
  func testI32() throws {
    print("testI32")
    if try client.testI32(thing: -1) != -1 {
      resultCode |= Error.baseTypes.rawValue
    }
    if try client.testI32(thing: 0) != 0 {
      resultCode |= Error.baseTypes.rawValue
    }
  }
  
  func testI64() throws {
    print("testI64")
    if try client.testI64(thing: 1) != 1 ||
        client.testI64(thing: -34359738368) != -34359738368 {
      resultCode |= Error.baseTypes.rawValue
    }
  }
  
  func testDouble() throws {
    print("testDouble")
    for testValue in [-5.235098235, 0, -1, -0.000341012439638598279] {
      if try client.testDouble(thing: testValue) != testValue {
        resultCode |= Error.baseTypes.rawValue
      }
    }
  }
  
  func testBinary() throws {
    print("testBinary")
    let val = Data(Array(0...255))
    if try client.testBinary(thing: val) != val {
      resultCode |= Error.baseTypes.rawValue
    }
  }
  
  func testStruct() throws {
    print("testStruct")
    let x = Xtruct(string_thing: "Zero", byte_thing: 1, i32_thing: -3, i64_thing: -5)
    if try client.testStruct(thing: x) != x {
      resultCode |= Error.structs.rawValue
    }
  }
  
  func testNest() throws {
    print("testNest")
    let inner = Xtruct(string_thing: "Zero", byte_thing: 1, i32_thing: -3, i64_thing: -5)
    let x = Xtruct2(byte_thing: 0, struct_thing: inner, i32_thing: 0)
    if try client.testNest(thing: x) != x {
      resultCode |= Error.structs.rawValue
    }
  }
  
  func testMap() throws {
    print("testMap")
    let x = TMap<Int32, Int32>([0: 1, 1: 2, 2: 3, 3: 4, -1: -2])
    if try client.testMap(thing: x) != x {
      resultCode |= Error.containers.rawValue
    }
  }
  
  func testSet() throws {
    print("testSet")
    let x = TSet<Int32>([8, 1, 42])
    if try client.testSet(thing: x) != x {
      resultCode |= Error.containers.rawValue
    }
  }
  
  func testList() throws {
    print("testList")
    let x = TList<Int32>([1, 4, 9, -42])
    if try client.testList(thing: x) != x {
      resultCode |= Error.containers.rawValue
    }
  }
  
  func testEnum() throws {
    print("testEnum")
    let x = Numberz.five
    if try client.testEnum(thing: x) != x {
      resultCode |= Error.containers.rawValue
    }
  }
  
  func testTypedef() throws {
    print("testTypedef")
    let x = UserId(bitPattern: 0xffffffffffffff)
    if try client.testTypedef(thing: x) != x {
      resultCode |= Error.containers.rawValue
    }
  }
  
  func testMapMap() throws {
    print("testMapMap")
    let x = TMap<Int32, TMap<Int32, Int32>>([
      -4: [-4: -4, -3: -3, -2: -2, -1: -1],
       4: [4: 4, 3: 3, 2: 2, 1: 1]
    ])
    if try client.testMapMap(hello: 42) != x {
      resultCode |= Error.containers.rawValue
    }
  }
  
  func testInsanity() throws {
    print("testInsanity()")
    let argument = Insanity(userMap: [.eight: 8], xtructs: [])
    let expected = TMap<UserId, TMap<Numberz, Insanity>>([
      1: [
        .two: argument,
        .three: argument
      ],
      2: [
        .six: Insanity(userMap: [:], xtructs: [])
      ]
    ])
    if try client.testInsanity(argument: argument) != expected {
      resultCode |= Error.containers.rawValue
    }
  }
  
  func testMulti() throws {
    print("testMulti")
    let x = Xtruct(string_thing: "Hello2", byte_thing: 74, i32_thing: 0xff00ff, i64_thing: 0xffffffffd0d0)
    if try client.testMulti(arg0: x.byte_thing, arg1: x.i32_thing, arg2: x.i64_thing, arg3: .init([0: "abc"]), arg4: Numberz.five, arg5: 0xf0f0f0) != x {
      resultCode |= Error.containers.rawValue
    }
  }
  
  func testException() throws {
    print("testException")
    try client.testException(arg: "Safe")
    do {
      try client.testException(arg: "Xception")
      resultCode |= Error.exceptions.rawValue
    } catch let error as Xception {
      guard error.errorCode == 1001, error.message == "Xception" else {
        resultCode |= Error.exceptions.rawValue
        return
      }
    } catch {
      resultCode |= Error.exceptions.rawValue
    }
    
    do {
      try client.testException(arg: "TException")
      resultCode |= Error.exceptions.rawValue
    } catch is TError {
      
    } catch {
      resultCode |= Error.exceptions.rawValue
    }
    
    try client.testException(arg: "success")
  }
  
  func testMultiException() throws {
    print("testMultiException")
    do {
      _ = try client.testMultiException(arg0: "Xception", arg1: "ignore")
    } catch let error as Xception {
      guard error.errorCode == 1001, error.message == "This is an Xception" else {
        resultCode |= Error.exceptions.rawValue
        return
      }
    } catch {
      resultCode |= Error.exceptions.rawValue
    }
    
    do {
      _ = try client.testMultiException(arg0: "Xception2", arg1: "ignore")
    } catch let error as Xception2 {
      guard error.errorCode == 2002, error.struct_thing.string_thing == "This is an Xception2" else {
        resultCode |= Error.exceptions.rawValue
        return
      }
    }
    
    let y = try client.testMultiException(arg0: "success", arg1: "foobar")
    if y.string_thing != "foobar" {
      resultCode |= Error.exceptions.rawValue
    }
  }
  
  // Swift generator doesn't yet support one way functions (THRIFT-5468)
  /*func testOneway() throws {
   print("testOneway")
   let start = CACurrentMediaTime()
   try client.testOneway(secondsToSleep: 1)
   let end = CACurrentMediaTime()
   let duration = end - start
   let delta = abs(1 - duration)
   print("oneway sleep took \(end - start) sec")
   
   guard delta < 0.5 else {
   print("oneway sleep took \(end - start) sec")
   resultCode |= Error.unknown.rawValue
   return
   }
   }
   
   func testOnewayThenNormal() throws {
   print("testOnewayThenNormal")
   try client.testOneway(secondsToSleep: 1)
   if try client.testString(thing: "Swift") != "Swift" {
   resultCode |= Error.baseTypes.rawValue
   }
   }*/
  
  func testUuid() throws {
    let uuid = UUID()
    guard try client.testUuid(thing: uuid) == uuid else {
      resultCode |= Error.baseTypes.rawValue
      return
    }
  }
}


let parameters = try TestClientParameters(arguments: CommandLine.arguments)
    
if parameters.showHelp {
  parameters.printHelp()
  exit(0)
}

Thread.sleep(forTimeInterval: 1)

try TestClient(parameters: parameters).run()
