//
//  Map.swift
//  Pods
//
//  Created by Kevin Wooten on 10/6/15.
//
//

import Foundation


public struct Map<Key : TSerializable, Value : TSerializable> : CollectionType, DictionaryLiteralConvertible, TSerializable {
  
  public static var thriftType : TType { return .MAP }

  typealias Storage = Dictionary<Key, Value>

  public typealias Index = Storage.Index

  public typealias Element = Storage.Element
  
  private var storage : Storage

  public var startIndex : Index {
    return storage.startIndex
  }
  
  public var endIndex: Index {
    return storage.endIndex
  }

  public var keys: LazyMapCollection<[Key : Value], Key> {
    return storage.keys
  }
  
  public var values: LazyMapCollection<[Key : Value], Value> {
    return storage.values
  }
  
  public init() {
    storage = Storage()
  }
  
  public init(dictionaryLiteral elements: (Key, Value)...) {
    storage = Storage()
    for (key, value) in elements {
      storage[key] = value
    }
  }
  
  public init(minimumCapacity: Int) {
    storage = Storage(minimumCapacity: minimumCapacity)
  }
  
  public subscript (position: Index) -> Element {
    get {
      return storage[position]
    }
  }
  
  public func indexForKey(key: Key) -> Index? {
    return storage.indexForKey(key)
  }
  
  public subscript (key: Key) -> Value? {
    get {
      return storage[key]
    }
    set {
      storage[key] = newValue
    }
  }

  public mutating func updateValue(value: Value, forKey key: Key) -> Value? {
    return updateValue(value, forKey: key)
  }
  
  public mutating func removeAtIndex(index: DictionaryIndex<Key, Value>) -> (Key, Value) {
    return removeAtIndex(index)
  }
  
  public mutating func removeValueForKey(key: Key) -> Value? {
    return storage.removeValueForKey(key)
  }
  
  public mutating func removeAll(keepCapacity keepCapacity: Bool = false) {
    storage.removeAll(keepCapacity: keepCapacity)
  }

  public var hashValue : Int {
    let prime = 31
    var result = 1
    for (key, value) in storage {
      result = prime * result + key.hashValue
      result = prime * result + value.hashValue
    }
    return result
  }
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Map {
    let (keyType, valueType, size) = try proto.readMapBegin()
    if keyType != Key.thriftType && valueType != Value.thriftType {
      throw NSError(domain: TProtocolErrorDomain, code: Int(TProtocolError.UnexpectedType.rawValue), userInfo: nil)
    }
    var map = Map()
    for _ in 0..<size {
      let key = try Key.readValueFromProtocol(proto)
      let value = try Value.readValueFromProtocol(proto)
      map.storage[key] = value
    }
    try proto.readMapEnd()
    return map
  }
  
  public static func writeValue(value: Map, toProtocol proto: TProtocol) throws {
    try proto.writeMapBeginWithKeyType(Key.thriftType, valueType: Value.thriftType, size: value.count)
    for (key, value) in value.storage {
      try Key.writeValue(key, toProtocol: proto)
      try Value.writeValue(value, toProtocol: proto)
    }
    try proto.writeMapEnd()
  }
  
}


extension Map : CustomStringConvertible, CustomDebugStringConvertible {
  
  public var description : String {
    return storage.description
  }
  
  public var debugDescription : String {
    return storage.debugDescription
  }
  
}

public func ==<Key, Value>(lhs: Map<Key,Value>, rhs: Map<Key, Value>) -> Bool {
  if lhs.count != rhs.count {
    return false
  }
  return lhs.storage == rhs.storage
}
