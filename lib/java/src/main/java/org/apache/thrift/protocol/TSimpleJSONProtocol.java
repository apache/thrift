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
import java.nio.charset.StandardCharsets;
import java.util.Stack;
import org.apache.thrift.TException;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

/**
 * JSON protocol implementation for thrift.
 *
 * <p>This protocol is write-only and produces a simple output format suitable for parsing by
 * scripting languages. It should not be confused with the full-featured TJSONProtocol.
 */
public class TSimpleJSONProtocol extends TProtocol {

  /** Factory */
  public static class Factory implements TProtocolFactory {
    public TProtocol getProtocol(TTransport trans) {
      return new TSimpleJSONProtocol(trans);
    }
  }

  private static final byte[] COMMA = new byte[] {','};
  private static final byte[] COLON = new byte[] {':'};
  private static final byte[] LBRACE = new byte[] {'{'};
  private static final byte[] RBRACE = new byte[] {'}'};
  private static final byte[] LBRACKET = new byte[] {'['};
  private static final byte[] RBRACKET = new byte[] {']'};
  private static final char QUOTE = '"';

  private static final TStruct ANONYMOUS_STRUCT = new TStruct();
  private static final TField ANONYMOUS_FIELD = new TField();
  private static final TMessage EMPTY_MESSAGE = new TMessage();
  private static final TSet EMPTY_SET = new TSet();
  private static final TList EMPTY_LIST = new TList();
  private static final TMap EMPTY_MAP = new TMap();
  private static final String LIST = "list";
  private static final String SET = "set";
  private static final String MAP = "map";

  protected class Context {
    protected void write() throws TException {}

    /** Returns whether the current value is a key in a map */
    protected boolean isMapKey() {
      return false;
    }
  }

  protected class ListContext extends Context {
    protected boolean first_ = true;

    protected void write() throws TException {
      if (first_) {
        first_ = false;
      } else {
        trans_.write(COMMA);
      }
    }
  }

  protected class StructContext extends Context {
    protected boolean first_ = true;
    protected boolean colon_ = true;

    protected void write() throws TException {
      if (first_) {
        first_ = false;
        colon_ = true;
      } else {
        trans_.write(colon_ ? COLON : COMMA);
        colon_ = !colon_;
      }
    }
  }

  protected class MapContext extends StructContext {
    protected boolean isKey = true;

    @Override
    protected void write() throws TException {
      super.write();
      isKey = !isKey;
    }

    protected boolean isMapKey() {
      // we want to coerce map keys to json strings regardless
      // of their type
      return isKey;
    }
  }

  protected final Context BASE_CONTEXT = new Context();

  /** Stack of nested contexts that we may be in. */
  protected Stack<Context> writeContextStack_ = new Stack<Context>();

  /** Current context that we are in */
  protected Context writeContext_ = BASE_CONTEXT;

  /** Push a new write context onto the stack. */
  protected void pushWriteContext(Context c) {
    writeContextStack_.push(writeContext_);
    writeContext_ = c;
  }

  /** Pop the last write context off the stack */
  protected void popWriteContext() {
    writeContext_ = writeContextStack_.pop();
  }

  /** Reset the write context stack to its initial state. */
  protected void resetWriteContext() {
    while (!writeContextStack_.isEmpty()) {
      popWriteContext();
    }
  }

  /** Used to make sure that we are not encountering a map whose keys are containers */
  protected void assertContextIsNotMapKey(String invalidKeyType) throws CollectionMapKeyException {
    if (writeContext_.isMapKey()) {
      throw new CollectionMapKeyException(
          "Cannot serialize a map with keys that are of type " + invalidKeyType);
    }
  }

  /** Constructor */
  public TSimpleJSONProtocol(TTransport trans) {
    super(trans);
  }

  @Override
  public void writeMessageBegin(TMessage message) throws TException {
    resetWriteContext(); // THRIFT-3743
    trans_.write(LBRACKET);
    pushWriteContext(new ListContext());
    writeString(message.name);
    writeByte(message.type);
    writeI32(message.seqid);
  }

  @Override
  public void writeMessageEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACKET);
  }

  @Override
  public void writeStructBegin(TStruct struct) throws TException {
    writeContext_.write();
    trans_.write(LBRACE);
    pushWriteContext(new StructContext());
  }

  @Override
  public void writeStructEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACE);
  }

  @Override
  public void writeFieldBegin(TField field) throws TException {
    // Note that extra type information is omitted in JSON!
    writeString(field.name);
  }

  @Override
  public void writeFieldEnd() throws TException {}

  @Override
  public void writeFieldStop() throws TException {}

  @Override
  public void writeMapBegin(TMap map) throws TException {
    assertContextIsNotMapKey(MAP);
    writeContext_.write();
    trans_.write(LBRACE);
    pushWriteContext(new MapContext());
    // No metadata!
  }

  @Override
  public void writeMapEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACE);
  }

  @Override
  public void writeListBegin(TList list) throws TException {
    assertContextIsNotMapKey(LIST);
    writeContext_.write();
    trans_.write(LBRACKET);
    pushWriteContext(new ListContext());
    // No metadata!
  }

  @Override
  public void writeListEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACKET);
  }

  @Override
  public void writeSetBegin(TSet set) throws TException {
    assertContextIsNotMapKey(SET);
    writeContext_.write();
    trans_.write(LBRACKET);
    pushWriteContext(new ListContext());
    // No metadata!
  }

  @Override
  public void writeSetEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACKET);
  }

  @Override
  public void writeBool(boolean b) throws TException {
    writeByte(b ? (byte) 1 : (byte) 0);
  }

  @Override
  public void writeByte(byte b) throws TException {
    writeI32(b);
  }

  @Override
  public void writeI16(short i16) throws TException {
    writeI32(i16);
  }

  @Override
  public void writeI32(int i32) throws TException {
    if (writeContext_.isMapKey()) {
      writeString(Integer.toString(i32));
    } else {
      writeContext_.write();
      _writeStringData(Integer.toString(i32));
    }
  }

  public void _writeStringData(String s) throws TException {
    byte[] b = s.getBytes(StandardCharsets.UTF_8);
    trans_.write(b);
  }

  @Override
  public void writeI64(long i64) throws TException {
    if (writeContext_.isMapKey()) {
      writeString(Long.toString(i64));
    } else {
      writeContext_.write();
      _writeStringData(Long.toString(i64));
    }
  }

  @Override
  public void writeDouble(double dub) throws TException {
    if (writeContext_.isMapKey()) {
      writeString(Double.toString(dub));
    } else {
      writeContext_.write();
      _writeStringData(Double.toString(dub));
    }
  }

  @Override
  public void writeString(String str) throws TException {
    writeContext_.write();
    int length = str.length();
    StringBuilder escape = new StringBuilder(length + 16);
    escape.append(QUOTE);
    for (int i = 0; i < length; ++i) {
      char c = str.charAt(i);
      switch (c) {
        case '"':
        case '\\':
          escape.append('\\');
          escape.append(c);
          break;
        case '\b':
          escape.append('\\');
          escape.append('b');
          break;
        case '\f':
          escape.append('\\');
          escape.append('f');
          break;
        case '\n':
          escape.append('\\');
          escape.append('n');
          break;
        case '\r':
          escape.append('\\');
          escape.append('r');
          break;
        case '\t':
          escape.append('\\');
          escape.append('t');
          break;
        default:
          // Control characters! According to JSON RFC u0020 (space)
          if (c < ' ') {
            String hex = Integer.toHexString(c);
            escape.append('\\');
            escape.append('u');
            for (int j = 4; j > hex.length(); --j) {
              escape.append('0');
            }
            escape.append(hex);
          } else {
            escape.append(c);
          }
          break;
      }
    }
    escape.append(QUOTE);
    _writeStringData(escape.toString());
  }

  @Override
  public void writeBinary(ByteBuffer bin) throws TException {
    // TODO(mcslee): Fix this
    writeString(
        new String(
            bin.array(),
            bin.position() + bin.arrayOffset(),
            bin.limit() - bin.position() - bin.arrayOffset(),
            StandardCharsets.UTF_8));
  }

  /**
   * Reading methods.
   *
   * <p>simplejson is not meant to be read back into thrift - see
   * http://wiki.apache.org/thrift/ThriftUsageJava - use JSON instead
   */
  @Override
  public TMessage readMessageBegin() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public void readMessageEnd() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public TStruct readStructBegin() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public void readStructEnd() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public TField readFieldBegin() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public void readFieldEnd() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public TMap readMapBegin() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public void readMapEnd() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public TList readListBegin() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public void readListEnd() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public TSet readSetBegin() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public void readSetEnd() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public boolean readBool() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public byte readByte() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public short readI16() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public int readI32() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public long readI64() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public double readDouble() throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public String readString() throws TException {
    throw new TException("Not implemented");
  }

  public String readStringBody(int size) throws TException {
    throw new TException("Not implemented");
  }

  @Override
  public ByteBuffer readBinary() throws TException {
    throw new TException("Not implemented");
  }

  public static class CollectionMapKeyException extends TException {
    public CollectionMapKeyException(String message) {
      super(message);
    }
  }

  /** Return the minimum number of bytes a type will consume on the wire */
  @Override
  public int getMinSerializedSize(byte type) throws TException {
    switch (type) {
      case 0:
        return 0; // Stop
      case 1:
        return 0; // Void
      case 2:
        return 1; // Bool
      case 3:
        return 1; // Byte
      case 4:
        return 1; // Double
      case 6:
        return 1; // I16
      case 8:
        return 1; // I32
      case 10:
        return 1; // I64
      case 11:
        return 2; // string length
      case 12:
        return 2; // empty struct
      case 13:
        return 2; // element count Map
      case 14:
        return 2; // element count Set
      case 15:
        return 2; // element count List
      default:
        throw new TTransportException(TTransportException.UNKNOWN, "unrecognized type code");
    }
  }
}
