/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.thrift;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.protocol.TProtocolUtil;
import org.apache.thrift.protocol.TType;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.TFieldIdEnum;

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
   * Deserialize only a single Thrift object (addressed by recursively using field id)
   * from a byte record.
   * @param record The object to read from
   * @param tb The object to read into
   * @param fieldIdPath The FieldId's that define a path tb
   * @throws TException 
   */
  public void partialDeserialize(TBase tb, byte[] bytes, TFieldIdEnum ... fieldIdPath) throws TException {
    // if there are no elements in the path, then the user is looking for the 
    // regular deserialize method
    // TODO: it might be nice not to have to do this check every time to save
    // some performance.
    if (fieldIdPath.length == 0) {
      deserialize(tb, bytes);
      return;
    }

    TProtocol iprot = protocolFactory_.getProtocol(
        new TIOStreamTransport(
          new ByteArrayInputStream(bytes))); 

    // index into field ID path being currently searched for
    int curPathIndex = 0;

    iprot.readStructBegin();

    while (curPathIndex < fieldIdPath.length) {
      TField field = iprot.readFieldBegin();
      // we can stop searching if we either see a stop or we go past the field 
      // id we're looking for (since fields should now be serialized in asc
      // order).
      if (field.type == TType.STOP || field.id > fieldIdPath[curPathIndex].getThriftFieldId()) { 
        return;
      }

      if (field.id != fieldIdPath[curPathIndex].getThriftFieldId()) {
        // Not the field we're looking for. Skip field.
        TProtocolUtil.skip(iprot, field.type);
        iprot.readFieldEnd();
      } else {
        // This field is the next step in the path. Step into field.
        curPathIndex++;
        if (curPathIndex < fieldIdPath.length) {
          iprot.readStructBegin();
        }
      }
    }

    // when this line is reached, iprot will be positioned at the start of tb.
    tb.read(iprot);
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

