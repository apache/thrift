//
//  TEnum.swift
//  Pods
//
//  Created by Kevin Wooten on 10/7/15.
//
//

import Foundation


public protocol TEnum : TSerializable {
  
}

public extension TEnum {
  
  public static var thriftType : TType { return TType.I32 }
  
}
