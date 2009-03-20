// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift;

import java.io.ByteArrayOutputStream;

/**
 * Class that allows access to the underlying buf without doing deep
 * copies on it.
 *
 */
public class TByteArrayOutputStream extends ByteArrayOutputStream {
  public TByteArrayOutputStream(int size) {
    super(size);
  }

  public TByteArrayOutputStream() {
    super();
  }


  public byte[] get() {
    return buf;
  }

  public int len() {
    return count;
  }
}
