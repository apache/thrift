// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.protocol;

import org.apache.thrift.transport.TTransport;

/**
 * Factory interface for constructing protocol instances.
 *
 */
public interface TProtocolFactory {
  public TProtocol getProtocol(TTransport trans);
}
