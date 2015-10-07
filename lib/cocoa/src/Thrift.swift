//
//  Messages.swift
//  ReTxt
//
//  Created by Kevin Wooten on 10/2/15.
//  Copyright © 2015 reTXT Labs, LLC. All rights reserved.
//

import Foundation


public protocol TSerializable : Hashable {
  
  static var thriftType : TType { get }

  init()
  
  static func readValueFromProtocol(proto: TProtocol) throws -> Self
  
  static func writeValue(value: Self, toProtocol proto: TProtocol) throws

}

extension Bool : TSerializable {

  public static let thriftType = TType.BOOL
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Bool {
    var value : ObjCBool = false
    try proto.readBool(&value)
    return value.boolValue
  }
  
  public static func writeValue(value: Bool, toProtocol proto: TProtocol) throws {
    try proto.writeBool(value)
  }

}

extension UInt8 : TSerializable {
  
  public static let thriftType = TType.BYTE
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> UInt8 {
    var value = UInt8()
    try proto.readByte(&value)
    return value
  }
  
  public static func writeValue(value: UInt8, toProtocol proto: TProtocol) throws {
    try proto.writeByte(value)
  }

}

extension Int16 : TSerializable {
  
  public static let thriftType = TType.I16
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Int16 {
    var value = Int16()
    try proto.readI16(&value)
    return value
  }
  
  public static func writeValue(value: Int16, toProtocol proto: TProtocol) throws {
    try proto.writeI16(value)
  }

}

extension Int : TSerializable {

  public static let thriftType = TType.I32
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Int {
    var value = Int32()
    try proto.readI32(&value)
    return Int(value)
  }
  
  public static func writeValue(value: Int, toProtocol proto: TProtocol) throws {
    try proto.writeI32(Int32(value))
  }
  
}

extension Int32 : TSerializable {
  
  public static let thriftType = TType.I32
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Int32 {
    var value = Int32()
    try proto.readI32(&value)
    return value
  }
  
  public static func writeValue(value: Int32, toProtocol proto: TProtocol) throws {
    try proto.writeI32(value)
  }

}

extension Int64 : TSerializable {
  
  public static let thriftType = TType.I64
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Int64 {
    var value = Int64()
    try proto.readI64(&value)
    return value
  }
  
  public static func writeValue(value: Int64, toProtocol proto: TProtocol) throws {
    try proto.writeI64(value)
  }

}

extension Double : TSerializable {
  
  public static let thriftType = TType.DOUBLE
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Double {
    var value = Double()
    try proto.readDouble(&value)
    return value
  }
  
  public static func writeValue(value: Double, toProtocol proto: TProtocol) throws {
    try proto.writeDouble(value)
  }
  
}

extension String : TSerializable {

  public static let thriftType = TType.STRING
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> String {
    var value : NSString?
    try proto.readString(&value)
    return value as! String
  }

  public static func writeValue(value: String, toProtocol proto: TProtocol) throws {
    try proto.writeString(value)
  }

}

public protocol TEnum : TSerializable {
}

public extension TEnum {
  
  public static var thriftType : TType { return TType.I32 }

  public init() {
    self.init()
  }
  
}

public protocol TStruct : TSerializable {
}


public extension TStruct {
  
  public static var thriftType : TType { return TType.STRUCT }
  
}


public extension TProtocol {
  
  public func readMessageBegin() throws -> (String, TMessageType, Int) {
    
    var name : NSString?
    var type : Int32 = -1
    var sequenceID : Int32 = -1
    
    try readMessageBeginReturningName(&name, type: &type, sequenceID: &sequenceID)
    
    return (name as String!, TMessageType(rawValue: type)!, Int(sequenceID))
  }
  
  public func writeMessageBeginWithName(name: String, type: TMessageType, sequenceID: Int) throws {
    try writeMessageBeginWithName(name, type: type.rawValue, sequenceID: Int32(sequenceID))
  }
  
  public func readStructBegin() throws -> (String?) {

    var name : NSString? = nil
    
    try readStructBeginReturningName(&name)

    return (name as String?)
  }
  
  public func readFieldBegin() throws -> (String?, TType, Int) {
    
    var name : NSString? = nil
    var type : Int32 = -1
    var fieldID : Int32 = -1
    
    try readFieldBeginReturningName(&name, type: &type, fieldID: &fieldID)
    
    return (name as String?, TType(rawValue: type)!, Int(fieldID))
  }
  
  public func writeFieldBeginWithName(name: String, type: TType, fieldID: Int) throws {
    try writeFieldBeginWithName(name, type: type.rawValue, fieldID: Int32(fieldID))
  }
  
  public func readMapBegin() throws -> (TType, TType, Int32) {
    
    var keyType : Int32 = -1
    var valueType : Int32 = -1
    var size : Int32 = 0
    
    try readMapBeginReturningKeyType(&keyType, valueType: &valueType, size: &size)
    
    return (TType(rawValue: keyType)!, TType(rawValue: valueType)!, size)
  }
  
  public func writeMapBeginWithKeyType(keyType: TType, valueType: TType, size: Int) throws {
    try writeMapBeginWithKeyType(keyType.rawValue, valueType: valueType.rawValue, size: Int32(size))
  }
  
  public func readSetBegin() throws -> (TType, Int32) {
    
    var elementType : Int32 = -1
    var size : Int32 = 0
    
    try readSetBeginReturningElementType(&elementType, size: &size)
    
    return (TType(rawValue: elementType)!, size)
  }
  
  public func writeSetBeginWithElementType(elementType: TType, size: Int) throws {
    try writeSetBeginWithElementType(elementType.rawValue, size: Int32(size))
  }
  
  public func readListBegin() throws -> (TType, Int32) {
    
    var elementType : Int32 = -1
    var size : Int32 = 0
    
    try readListBeginReturningElementType(&elementType, size: &size)
    
    return (TType(rawValue: elementType)!, size)
  }

  public func writeListBeginWithElementType(elementType: TType, size: Int) throws {
    try writeListBeginWithElementType(elementType.rawValue, size: Int32(size))
  }
  
  public func writeFieldValue<T: TSerializable>(value: T, name: String, type: TType, id: Int32) throws {
    try writeFieldBeginWithName(name, type: type.rawValue, fieldID: id)
    try writeValue(value)
    try writeFieldEnd()
  }

  public func readValue<T: TSerializable>() throws -> T {
    return try T.readValueFromProtocol(self)
  }
  
  public func writeValue<T: TSerializable>(value: T) throws {
    try T.writeValue(value, toProtocol: self)
  }
  
  public func readResultMessageBegin() throws {
    
    let (_, type, _) = try readMessageBegin();
    
    if type == .EXCEPTION {

      // TODO: read exception
      throw NSError(domain: TApplicationErrorDomain, code: 0, userInfo: nil)
      
    }
    
    return
  }
  
  public func validateValue(value: Any?, named name: String) throws {
    
    if value == nil {
      throw NSError(
        domain: TProtocolErrorDomain,
        code: Int(TProtocolError.MissingRequiredField.rawValue),
        userInfo: [TProtocolErrorFieldNameKey: name])
    }
    
  }
  
  public func skipType(type: TType) throws {
    try TProtocolUtil.skipType(type.rawValue, onProtocol: self)
  }
  
}


infix operator ?== {}

public func ?==<T: TSerializable>(lhs: T?, rhs: T?) -> Bool {
  if let l = lhs, r = rhs {
    return l == r
  }
  return lhs == rhs
}

public func ?==<T: TSerializable>(lhs: T, rhs: T) -> Bool {
  return lhs == rhs
}
