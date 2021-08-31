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

package org.apache.thrift.partial;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TList;
import org.apache.thrift.protocol.TMap;
import org.apache.thrift.protocol.TMessage;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.protocol.TSet;
import org.apache.thrift.protocol.TStruct;
import org.apache.thrift.protocol.TType;
import org.apache.thrift.transport.TMemoryInputTransport;

import java.nio.ByteBuffer;

/**
 * Enables partial deserialization of thrift objects.
 *
 * This class is meant to be a helper class for {@link PartialThriftDeserializer}.
 * It cannot be used separately on its own.
 */
abstract class PartialThriftProtocol extends TProtocol {

  protected static ByteBuffer emptyByteBuffer = ByteBuffer.allocate(0);

  protected TProtocol tprot;
  protected TMemoryInputTransport transport;

  public PartialThriftProtocol() {
    super(null);
    try {
      this.transport = this.createTransport();
    } catch (TException e) {
      // TMemoryInputTransport() never throws exception when invoked without an arg.
      // Therefore, we do not propage this checked exception further as it causes
      // a many rippling changes. That said, we do not want to silently mask it either
      // therefore wrap in RuntimeException and rethrow.
      throw new RuntimeException(e);
    }
    this.tprot = createProtocol();
  }

  @Override
  public int getMinSerializedSize(byte type) throws TException {
    return this.tprot.getMinSerializedSize(type);
  }

  protected TMemoryInputTransport createTransport() throws TException {
    return new TMemoryInputTransport();
  }

  protected abstract TProtocol createProtocol();

  @Override
  public void reset() {
    tprot.reset();
  }

  public void reset(byte[] bytes) {
    reset(bytes, 0, bytes.length);
  }

  public void reset(byte[] bytes, int offset, int length) {
    transport.reset(bytes, offset, length);
    tprot.reset();
  }

  // -----------------------------------------------------------------
  // Additional methods to improve performance.

  public abstract int readFieldBeginData() throws TException;

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
        tprot.readStructBegin();
        while (true) {
          int tfieldData = this.readFieldBeginData();
          byte tfieldType = TFieldData.getType(tfieldData);
          if (tfieldType == TType.STOP) {
            break;
          }
          this.skip(tfieldType, maxDepth - 1);
          tprot.readFieldEnd();
        }
        tprot.readStructEnd();
        break;

      case TType.MAP:
        TMap map = tprot.readMapBegin();
        for (int i = 0; i < map.size; i++) {
          this.skip(map.keyType, maxDepth - 1);
          this.skip(map.valueType, maxDepth - 1);
        }
        tprot.readMapEnd();
        break;

      case TType.SET:
        TSet set = tprot.readSetBegin();
        for (int i = 0; i < set.size; i++) {
          this.skip(set.elemType, maxDepth - 1);
        }
        tprot.readSetEnd();
        break;

      case TType.LIST:
        TList list = tprot.readListBegin();
        for (int i = 0; i < list.size; i++) {
          this.skip(list.elemType, maxDepth - 1);
        }
        tprot.readListEnd();
        break;

      default:
        throw new TProtocolException(
            TProtocolException.INVALID_DATA, "Unrecognized type " + fieldType);
    }
  }

  protected void skipBool() throws TException {
    this.skipBytes(1);
  }

  protected void skipByte() throws TException {
    this.skipBytes(1);
  }

  protected void skipI16() throws TException {
    this.skipBytes(2);
  }

  protected void skipI32() throws TException {
    this.skipBytes(4);
  }

  protected void skipI64() throws TException {
    this.skipBytes(8);
  }

  protected void skipDouble() throws TException {
    this.skipBytes(8);
  }

  protected void skipBinary() throws TException {
    int size = readI32();
    this.skipBytes(size);
  }

  static final int MAX_SKIPPED_BYTES = 256;
  protected byte[] skippedBytes = new byte[MAX_SKIPPED_BYTES];

  protected void skipBytes(int numBytes) throws TException {
    if (numBytes <= MAX_SKIPPED_BYTES) {
      if (this.transport.getBytesRemainingInBuffer() >= numBytes) {
        this.transport.consumeBuffer(numBytes);
      } else {
        this.transport.readAll(skippedBytes, 0, numBytes);
      }
    } else {
      int remaining = numBytes;
      while (remaining > 0) {
        skipBytes(Math.min(remaining, MAX_SKIPPED_BYTES));
        remaining -= MAX_SKIPPED_BYTES;
      }
    }
  }

  // -----------------------------------------------------------------
  // Methods passed through to the wrapped protocol when not skipping.

  @Override
  public String readString() throws TException {
    return tprot.readString();
  }

  @Override
  public ByteBuffer readBinary() throws TException {
    return tprot.readBinary();
  }

  // -----------------------------------------------------------------
  // Methods passed through to the wrapped protocol.


  @Override
  public TMessage readMessageBegin() throws TException {
    return tprot.readMessageBegin();
  }

  @Override
  public TStruct readStructBegin() throws TException {
    return tprot.readStructBegin();
  }

  @Override
  public void readStructEnd() throws TException {
    tprot.readStructEnd();
  }

  @Override
  public TField readFieldBegin() throws TException {
    return tprot.readFieldBegin();
  }

  @Override
  public TMap readMapBegin() throws TException {
    return tprot.readMapBegin();
  }

  @Override
  public TList readListBegin() throws TException {
    return tprot.readListBegin();
  }

  @Override
  public TSet readSetBegin() throws TException {
    return tprot.readSetBegin();
  }

  @Override
  public boolean readBool() throws TException {
    return tprot.readBool();
  }

  @Override
  public byte readByte() throws TException {
    return tprot.readByte();
  }

  @Override
  public short readI16() throws TException {
    return tprot.readI16();
  }

  @Override
  public int readI32() throws TException {
    return tprot.readI32();
  }

  @Override
  public long readI64() throws TException {
    return tprot.readI64();
  }

  @Override
  public double readDouble() throws TException {
    return tprot.readDouble();
  }

  @Override
  public void readMessageEnd() throws TException {
    tprot.readMessageEnd();
  }

  @Override
  public void readFieldEnd() throws TException {
    tprot.readFieldEnd();
  }

  @Override
  public void readMapEnd() throws TException {
    tprot.readMapEnd();
  }

  @Override
  public void readListEnd() throws TException {
    tprot.readListEnd();
  }

  @Override
  public void readSetEnd() throws TException {
    tprot.readSetEnd();
  }

  // -----------------------------------------------------------------
  // Writing methods are not supported.

  @Override
  public void writeMessageBegin(TMessage message) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeStructBegin(TStruct struct) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeStructEnd() throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeFieldBegin(TField field) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeFieldStop() throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeMapBegin(TMap map) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeListBegin(TList list) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeSetBegin(TSet set) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeBool(boolean b) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeByte(byte b) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeI16(short i16) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeI32(int i32) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeI64(long i64) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeDouble(double dub) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeString(String str) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeBinary(ByteBuffer bin) throws TException {
    throw writeNotSupported();
  }

  @Override
  public void writeMessageEnd() throws TException {}

  @Override
  public void writeMapEnd() throws TException {}

  @Override
  public void writeListEnd() throws TException {}

  @Override
  public void writeSetEnd() throws TException {}

  @Override
  public void writeFieldEnd() throws TException {}

  private UnsupportedOperationException writeNotSupported() {
    return new UnsupportedOperationException("This protocol does not support write methods.");
  }
}
