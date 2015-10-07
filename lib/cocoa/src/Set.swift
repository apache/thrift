//
//  Set.swift
//  Pods
//
//  Created by Kevin Wooten on 10/6/15.
//
//

import Foundation


public struct Set<Element : TSerializable> : CollectionType, ArrayLiteralConvertible, TSerializable {
  
  public static var thriftType : TType { return .SET }
  
  public typealias Index = Storage.Index
  
  typealias Storage = Swift.Set<Element>
  
  private var storage : Storage
  
  public init() {
    storage = Storage()
  }
  
  public init(arrayLiteral elements: Element...) {
    storage = Storage(elements)
  }
  
  public init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S) {
    storage = Storage(sequence)    
  }
  
  public var startIndex : Index { return storage.startIndex }
  
  public var endIndex : Index { return storage.endIndex }
  
  public mutating func insert(member: Element) {
    return storage.insert(member)
  }
  
  public mutating func remove(element: Element) -> Element? {
    return storage.remove(element)
  }
  
  public mutating func removeAll(keepCapacity keepCapacity: Bool = false) {
    return storage.removeAll(keepCapacity: keepCapacity)
  }
  
  public mutating func removeAtIndex(index: SetIndex<Element>) -> Element {
    return storage.removeAtIndex(index)
  }
  
  public subscript (position: SetIndex<Element>) -> Element {
    return storage[position]
  }
  
  public func union(other: Set) -> Set {
    return Set(storage.union(other))
  }
  
  public func intersect(other: Set) -> Set {
    return Set(storage.intersect(other))
  }
  
  public func exclusiveOr(other: Set) -> Set {
    return Set(storage.exclusiveOr(other))
  }
  
  public func subtract(other: Set) -> Set {
    return Set(storage.subtract(other))
  }
  
  public mutating func intersectInPlace(other: Set) {
    storage.intersectInPlace(other)
  }

  public mutating func exclusiveOrInPlace(other: Set) {
    storage.exclusiveOrInPlace(other)
  }

  public mutating func subtractInPlace(other: Set) {
    storage.subtractInPlace(other)
  }  

  public func isSubsetOf(other: Set) -> Bool {
    return storage.isSubsetOf(other)
  }

  public func isDisjointWith(other: Set) -> Bool {
    return storage.isDisjointWith(other)
  }
  
  public func isSupersetOf(other: Set) -> Bool {
    return storage.isSupersetOf(other)
  }

  public var isEmpty: Bool { return storage.isEmpty }

  public var hashValue : Int {
    let prime = 31
    var result = 1
    for element in storage {
      result = prime * result + element.hashValue
    }
    return result
  }
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> Set {
    let (elementType, size) = try proto.readSetBegin()
    if elementType != Element.thriftType {
      throw NSError(domain: TProtocolErrorDomain, code: Int(TProtocolError.UnexpectedType.rawValue), userInfo: nil)
    }
    var set = Set()
    for _ in 0..<size {
      let element = try Element.readValueFromProtocol(proto)
      set.storage.insert(element)
    }
    try proto.readSetEnd()
    return set
  }
  
  public static func writeValue(value: Set, toProtocol proto: TProtocol) throws {
    try proto.writeSetBeginWithElementType(Element.thriftType, size: value.count)
    for element in value.storage {
      try Element.writeValue(element, toProtocol: proto)
    }
    try proto.writeSetEnd()
  }
  
}

public func ==<Element>(lhs: Set<Element>, rhs: Set<Element>) -> Bool {
  return lhs.storage == rhs.storage
}
