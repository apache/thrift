// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift;

import org.apache.thrift.protocol.TProtocol;

/**
 * Generic base interface for generated Thrift objects.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public interface TBase  {

  /**
   * Reads the TObject from the given input protocol.
   *
   * @param iprot Input protocol
   */
  public void read(TProtocol iprot) throws TException;

  /**
   * Writes the objects out to the protocol
   *
   * @param oprot Output protocol
   */
  public void write(TProtocol oprot) throws TException;
}
