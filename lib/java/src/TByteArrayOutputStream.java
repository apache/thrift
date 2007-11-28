// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift;

import java.io.ByteArrayOutputStream;

/**
 * Class that allows access to the underlying buf without doing deep
 * copies on it.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TByteArrayOutputStream extends ByteArrayOutputStream {
  public TByteArrayOutputStream(int size) {
    super(size);
  }

  public byte[] get() {
    return buf;
  }

  public int len() {
    return count;
  }
}
