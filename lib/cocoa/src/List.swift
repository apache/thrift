//
//  List.swift
//  Pods
//
//  Created by Kevin Wooten on 10/6/15.
//
//

import Foundation



public struct List<Element : TSerializable> : MutableCollectionType, Hashable, ArrayLiteralConvertible, TSerializable {
  
  public static var thriftType : TType { return .LIST }
  
  typealias Storage = Array<Element>

  public typealias Index = Storage.Index
  
  private var storage = Storage()
  
  public var startIndex : Index {
    return storage.startIndex
  }
  
  public var endIndex : Index {
    return storage.endIndex
  }

  public subscript (position: Index) -> Element {
    get {
      return storage[position]
    }
    set {
      storage[position] = newValue
    }
  }
  
  public var hashValue : Int {
    let prime = 31
    var result = 1
    for element in storage {
      result = prime * result + element.hashValue
    }
    return result
  }
  
  public init(arrayLiteral elements: Element...) {
    self.storage = Storage(storage)
  }
  
  public init() {
    self.storage = Storage()
  }
  
  public mutating func append(newElement: Element) {
    self.storage.append(newElement)
  }
  
  public mutating func appendContentsOf<C : CollectionType where C.Generator.Element == Element>(newstorage: C) {
    self.storage.appendContentsOf(newstorage)
  }
  
  public mutating func insert(newElement: Element, atIndex index: Int) {
    self.storage.insert(newElement, atIndex: index)
  }
  
  public mutating func insertContentsOf<C : CollectionType where C.Generator.Element == Element>(newElements: C, at index: Int) {
    self.storage.insertContentsOf(newElements, at: index)
  }
  
  public mutating func removeAll(keepCapacity keepCapacity: Bool = true) {
    self.storage.removeAll(keepCapacity: keepCapacity)
  }
  
  public mutating func removeAtIndex(index: Index) {
    self.storage.removeAtIndex(index)
  }
  
  public mutating func removeFirst(n: Int = 0) {
    self.storage.removeFirst(n)
  }
  
  public mutating func removeLast() -> Element {
    return self.storage.removeLast()
  }
  
  public mutating func removeRange(subRange: Range<Index>) {
    self.storage.removeRange(subRange)
  }
  
  public mutating func reserveCapacity(minimumCapacity: Int) {
    self.storage.reserveCapacity(minimumCapacity)
  }
  
  public static func readValueFromProtocol(proto: TProtocol) throws -> List {
    let (elementType, size) = try proto.readListBegin()
    if elementType != Element.thriftType {
      throw NSError(domain: TProtocolErrorDomain, code: Int(TProtocolError.UnexpectedType.rawValue), userInfo: nil)
    }
    var list = List()
    for _ in 0..<size {
      let element = try Element.readValueFromProtocol(proto)
      list.storage.append(element)
    }
    try proto.readListEnd()
    return list
  }
  
  public static func writeValue(value: List, toProtocol proto: TProtocol) throws {
    try proto.writeListBeginWithElementType(Element.thriftType, size: value.count)
    for element in value.storage {
      try Element.writeValue(element, toProtocol: proto)
    }
    try proto.writeListEnd()
  }
}

extension List : CustomStringConvertible, CustomDebugStringConvertible {
  
  public var description : String {
    return storage.description
  }
  
  public var debugDescription : String {
    return storage.debugDescription
  }
  
}

public func ==<Element>(lhs: List<Element>, rhs: List<Element>) -> Bool {
  return lhs.storage == rhs.storage
}
