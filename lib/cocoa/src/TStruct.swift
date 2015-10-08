//
//  TStruct.swift
//  Pods
//
//  Created by Kevin Wooten on 10/7/15.
//
//

import Foundation


public protocol TStruct : TSerializable {
}


public extension TStruct {
  
  public static var thriftType : TType { return TType.STRUCT }
  
}
