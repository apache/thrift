// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift;

import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;

import com.facebook.thrift.protocol.TBinaryProtocol;
import com.facebook.thrift.protocol.TProtocol;
import com.facebook.thrift.protocol.TProtocolFactory;
import com.facebook.thrift.transport.TIOStreamTransport;
import com.facebook.thrift.transport.TTransport;

/**
 * Generic utility for easily serializing objects into a byte array.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TSerializer {

  private static class TByteArrayTransport extends TIOStreamTransport {

    private final ByteArrayOutputStream baos_ = new ByteArrayOutputStream();

    public TByteArrayTransport() {
      outputStream_ = baos_;
    }

    public byte[] get() {
      return baos_.toByteArray();
    }

    public void reset() {
      baos_.reset();
    }
  }

  private TProtocol protocol_;

  private final TByteArrayTransport transport_ = new TByteArrayTransport();

  public TSerializer() {
    this(new TBinaryProtocol.Factory());
  }

  public TSerializer(TProtocolFactory protocolFactory) {
    protocol_ = protocolFactory.getProtocol(transport_);
  }

  public byte[] serialize(TBase base) throws TException {
    transport_.reset();
    base.write(protocol_);
    byte[] data = transport_.get();
    return data;
  }

  public String toString(TBase base, String charset) throws TException {
    try {
      return new String(serialize(base), charset);
    } catch (UnsupportedEncodingException uex) {
      throw new TException("JVM DOES NOT SUPPORT ENCODING");
    }
  }

  public String toString(TBase base) throws TException {
    return new String(serialize(base));
  }
}

