// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TIOStreamTransport;

/**
 * Generic utility for easily deserializing objects from a byte array or Java
 * String.
 *
 */
public class TDeserializer {
  private final TProtocolFactory protocolFactory_;

  /**
   * Create a new TDeserializer that uses the TBinaryProtocol by default.
   */
  public TDeserializer() {
    this(new TBinaryProtocol.Factory());
  }

  /**
   * Create a new TDeserializer. It will use the TProtocol specified by the
   * factory that is passed in.
   *
   * @param protocolFactory Factory to create a protocol
   */
  public TDeserializer(TProtocolFactory protocolFactory) {
    protocolFactory_ = protocolFactory;
  }

  /**
   * Deserialize the Thrift object from a byte array.
   *
   * @param base The object to read into
   * @param bytes The array to read from
   */
  public void deserialize(TBase base, byte[] bytes) throws TException {
    base.read(
        protocolFactory_.getProtocol(
          new TIOStreamTransport(
            new ByteArrayInputStream(bytes))));
  }

  /**
   * Deserialize the Thrift object from a Java string, using a specified
   * character set for decoding.
   *
   * @param base The object to read into
   * @param data The string to read from
   * @param charset Valid JVM charset
   */
  public void deserialize(TBase base, String data, String charset) throws TException {
    try {
      deserialize(base, data.getBytes(charset));
    } catch (UnsupportedEncodingException uex) {
      throw new TException("JVM DOES NOT SUPPORT ENCODING: " + charset);
    }
  }

  /**
   * Deserialize the Thrift object from a Java string, using the default JVM
   * charset encoding.
   *
   * @param base The object to read into
   * @param data The string to read from
   */
  public void toString(TBase base, String data) throws TException {
    deserialize(base, data.getBytes());
  }
}

