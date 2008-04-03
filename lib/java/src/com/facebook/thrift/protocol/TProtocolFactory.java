// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.protocol;

import com.facebook.thrift.transport.TTransport;

/**
 * Factory interface for constructing protocol instances.
 *
 * @author Mark Slee <mcslee@facebook.com>
 * @author Aditya Agarwal <aditya@facebook.com>
 */
public interface TProtocolFactory {
  public TProtocol getProtocol(TTransport trans);
}
