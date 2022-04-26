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

package org.apache.thrift.protocol;

import java.nio.ByteBuffer;

import org.apache.thrift.TException;
import org.apache.thrift.partial.TFieldData;
import org.apache.thrift.scheme.IScheme;
import org.apache.thrift.scheme.StandardScheme;
import org.apache.thrift.transport.TTransport;

/**
 * Protocol interface definition.
 *
 */
public abstract class TProtocol {

  /**
   * Prevent direct instantiation
   */
  @SuppressWarnings("unused")
  private TProtocol() {}

  /**
   * Transport
   */
  protected TTransport trans_;

  /**
   * Constructor
   */
  protected TProtocol(TTransport trans) {
    trans_ = trans;
  }

  /**
   * Transport accessor
   */
  public TTransport getTransport() {
    return trans_;
  }

  protected void checkReadBytesAvailable(TMap map) throws TException {
    long elemSize = getMinSerializedSize(map.keyType) + getMinSerializedSize(map.valueType);
    trans_.checkReadBytesAvailable(map.size * elemSize);
  }

  protected void checkReadBytesAvailable(TList list) throws TException {
    trans_.checkReadBytesAvailable(list.size * getMinSerializedSize(list.elemType));
  }

  protected void checkReadBytesAvailable(TSet set) throws TException {
    trans_.checkReadBytesAvailable(set.size * getMinSerializedSize(set.elemType));
  }

  /**
   * Return
   * @param type  Returns the minimum amount of bytes needed to store the smallest possible instance of TType.
   * @return
   * @throws TException
   */
  public abstract int getMinSerializedSize(byte type) throws TException;

  /**
   * Writing methods.
   */

  public abstract void writeMessageBegin(TMessage message) throws TException;

  public abstract void writeMessageEnd() throws TException;

  public abstract void writeStructBegin(TStruct struct) throws TException;

  public abstract void writeStructEnd() throws TException;

  public abstract void writeFieldBegin(TField field) throws TException;

  public abstract void writeFieldEnd() throws TException;

  public abstract void writeFieldStop() throws TException;

  public abstract void writeMapBegin(TMap map) throws TException;

  public abstract void writeMapEnd() throws TException;

  public abstract void writeListBegin(TList list) throws TException;

  public abstract void writeListEnd() throws TException;

  public abstract void writeSetBegin(TSet set) throws TException;

  public abstract void writeSetEnd() throws TException;

  public abstract void writeBool(boolean b) throws TException;

  public abstract void writeByte(byte b) throws TException;

  public abstract void writeI16(short i16) throws TException;

  public abstract void writeI32(int i32) throws TException;

  public abstract void writeI64(long i64) throws TException;

  public abstract void writeDouble(double dub) throws TException;

  public abstract void writeString(String str) throws TException;

  public abstract void writeBinary(ByteBuffer buf) throws TException;

  /**
   * Reading methods.
   */

  public abstract TMessage readMessageBegin() throws TException;

  public abstract void readMessageEnd() throws TException;

  public abstract TStruct readStructBegin() throws TException;

  public abstract void readStructEnd() throws TException;

  public abstract TField readFieldBegin() throws TException;

  public abstract void readFieldEnd() throws TException;

  public abstract TMap readMapBegin() throws TException;

  public abstract void readMapEnd() throws TException;

  public abstract TList readListBegin() throws TException;

  public abstract void readListEnd() throws TException;

  public abstract TSet readSetBegin() throws TException;

  public abstract void readSetEnd() throws TException;

  public abstract boolean readBool() throws TException;

  public abstract byte readByte() throws TException;

  public abstract short readI16() throws TException;

  public abstract int readI32() throws TException;

  public abstract long readI64() throws TException;

  public abstract double readDouble() throws TException;

  public abstract String readString() throws TException;

  public abstract ByteBuffer readBinary() throws TException;

  /**
   * Reset any internal state back to a blank slate. This method only needs to
   * be implemented for stateful protocols.
   */
  public void reset() {}

  /**
   * Scheme accessor
   */
  public Class<? extends IScheme> getScheme() {
    return StandardScheme.class;
  }

  // -----------------------------------------------------------------
  // Additional methods to improve performance.

  public int readFieldBeginData() throws TException {
    // Derived classes should provide a more efficient version of this
    // method if allowed by the encoding used by that protocol.
    TField tfield = this.readFieldBegin();
    return TFieldData.encode(tfield.type, tfield.id);
  }

  public void skip(byte fieldType) throws TException {
    this.skip(fieldType, Integer.MAX_VALUE);
  }

  public void skip(byte fieldType, int maxDepth) throws TException {
    if (maxDepth <= 0) {
      throw new TException("Maximum skip depth exceeded");
    }

    switch (fieldType) {
      case TType.BOOL:
        this.skipBool();
        break;

      case TType.BYTE:
        this.skipByte();
        break;

      case TType.I16:
        this.skipI16();
        break;

      case TType.I32:
        this.skipI32();
        break;

      case TType.I64:
        this.skipI64();
        break;

      case TType.DOUBLE:
        this.skipDouble();
        break;

      case TType.STRING:
        this.skipBinary();
        break;

      case TType.STRUCT:
        this.readStructBegin();
        while (true) {
          int tfieldData = this.readFieldBeginData();
          byte tfieldType = TFieldData.getType(tfieldData);
          if (tfieldType == TType.STOP) {
            break;
          }
          this.skip(tfieldType, maxDepth - 1);
          this.readFieldEnd();
        }
        this.readStructEnd();
        break;

      case TType.MAP:
        TMap map = this.readMapBegin();
        for (int i = 0; i < map.size; i++) {
          this.skip(map.keyType, maxDepth - 1);
          this.skip(map.valueType, maxDepth - 1);
        }
        this.readMapEnd();
        break;

      case TType.SET:
        TSet set = this.readSetBegin();
        for (int i = 0; i < set.size; i++) {
          this.skip(set.elemType, maxDepth - 1);
        }
        this.readSetEnd();
        break;

      case TType.LIST:
        TList list = this.readListBegin();
        for (int i = 0; i < list.size; i++) {
          this.skip(list.elemType, maxDepth - 1);
        }
        this.readListEnd();
        break;

      default:
        throw new TProtocolException(
            TProtocolException.INVALID_DATA, "Unrecognized type " + fieldType);
    }
  }

  /**
   * The default implementation of all skip() methods calls the corresponding read() method.
   * Protocols that derive from this class are strongly encouraged to provide
   * a more efficient alternative.
   */

  protected void skipBool() throws TException {
    this.readBool();
  }

  protected void skipByte() throws TException {
    this.readByte();
  }

  protected void skipI16() throws TException {
    this.readI16();
  }

  protected void skipI32() throws TException {
    this.readI32();
  }

  protected void skipI64() throws TException {
    this.readI64();
  }

  protected void skipDouble() throws TException {
    this.readDouble();
  }

  protected void skipBinary() throws TException {
    this.readBinary();
  }

  static final int MAX_SKIPPED_BYTES = 256;
  protected byte[] skippedBytes = new byte[MAX_SKIPPED_BYTES];

  protected void skipBytes(int numBytes) throws TException {
    if (numBytes <= MAX_SKIPPED_BYTES) {
      if (this.getTransport().getBytesRemainingInBuffer() >= numBytes) {
        this.getTransport().consumeBuffer(numBytes);
      } else {
        this.getTransport().readAll(skippedBytes, 0, numBytes);
      }
    } else {
      int remaining = numBytes;
      while (remaining > 0) {
        skipBytes(Math.min(remaining, MAX_SKIPPED_BYTES));
        remaining -= MAX_SKIPPED_BYTES;
      }
    }
  }
}
