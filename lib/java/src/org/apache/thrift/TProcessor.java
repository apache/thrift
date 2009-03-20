// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift;

import org.apache.thrift.protocol.TProtocol;

/**
 * A processor is a generic object which operates upon an input stream and
 * writes to some output stream.
 *
 */
public interface TProcessor {
  public boolean process(TProtocol in, TProtocol out)
    throws TException;
}
