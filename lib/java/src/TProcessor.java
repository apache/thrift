// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift;

import com.facebook.thrift.protocol.TProtocol;

/**
 * A processor is a generic object which operates upon an input stream and
 * writes to some output stream.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public interface TProcessor {
  public boolean process(TProtocol in, TProtocol out)
    throws TException;
}
