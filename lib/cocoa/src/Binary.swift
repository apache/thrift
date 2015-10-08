//
//  Binary.swift
//  Pods
//
//  Created by Kevin Wooten on 10/6/15.
//
//

import Foundation


public struct Binary : TSerializable {
  
  public static var thriftType : TType { return .STRING }
  
  private var storage : NSData
  
  public init() {
    self.storage = NSData()
  }
  
  public init(data: NSData) {
    self.storage = data
  }
  
  public var length : Int {
    return storage.length
  }
  
  public var hashValue : Int {
    return storage.hashValue
  }
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Binary {
    var data : NSData?
    try proto.readBinary(&data)
    return Binary(data: data!)
  }
  
  public static func writeValue(value: Binary, toProtocol proto: TProtocol) throws {
    try proto.writeBinary(value.storage)
  }
  
}

extension Binary : CustomStringConvertible, CustomDebugStringConvertible {
  
  public var description : String {
    return storage.description
  }
  
  public var debugDescription : String {
    return storage.debugDescription
  }
  
}

public func ==(lhs: Binary, rhs: Binary) -> Bool {
  return lhs.storage == rhs.storage
}
