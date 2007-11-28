// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.protocol;

import com.facebook.thrift.TByteArrayOutputStream;
import com.facebook.thrift.TException;
import com.facebook.thrift.transport.TTransport;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.util.Stack;

/**
 * JSON protocol implementation for thrift.
 *
 * @author Joydeep Sen Sarma <jssarma@facebook.com>
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TJSONProtocol extends TProtocol {

  /**
   * Factory
   */
  public static class Factory implements TProtocolFactory {
    public TProtocol getProtocol(TTransport trans) {
      return new TJSONProtocol(trans);
    }
  }

  public static final byte[] COMMA = new byte[] {','};
  public static final byte[] COLON = new byte[] {':'};
  public static final byte[] LBRACE = new byte[] {'{'};
  public static final byte[] RBRACE = new byte[] {'}'};
  public static final byte[] LBRACKET = new byte[] {'['};
  public static final byte[] RBRACKET = new byte[] {']'};
  public static final char QUOTE = '"';

  protected class Context {
    protected void write() throws TException {}
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

  protected final Context BASE_CONTEXT = new Context();

  /**
   * Stack of nested contexts that we may be in.
   */
  protected Stack<Context> writeContextStack_ = new Stack<Context>();

  /**
   * Current context that we are in
   */
  protected Context writeContext_ = BASE_CONTEXT;

  /**
   * Push a new write context onto the stack.
   */
  protected void pushWriteContext(Context c) {
    writeContextStack_.push(writeContext_);
    writeContext_ = c;
  }

  /**
   * Pop the last write context off the stack
   */
  protected void popWriteContext() {
    writeContext_ = writeContextStack_.pop();
  }

  /**
   * Constructor
   */
  public TJSONProtocol(TTransport trans) {
    super(trans);
  }

  public void writeMessageBegin(TMessage message) throws TException {
    trans_.write(LBRACKET);
    pushWriteContext(new ListContext());
    writeString(message.name);
    writeByte(message.type);
    writeI32(message.seqid);
  }

  public void writeMessageEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACKET);
  }

  public void writeStructBegin(TStruct struct) throws TException {
    writeContext_.write();
    trans_.write(LBRACE);
    pushWriteContext(new StructContext());
  }

  public void writeStructEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACE);
  }

  public void writeFieldBegin(TField field) throws TException {
    // Note that extra type information is omitted in JSON!
    writeString(field.name);
  }

  public void writeFieldEnd() {}

  public void writeFieldStop() {}

  public void writeMapBegin(TMap map) throws TException {
    writeContext_.write();
    trans_.write(LBRACE);
    pushWriteContext(new StructContext());
    // No metadata!
  }

  public void writeMapEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACE);
  }

  public void writeListBegin(TList list) throws TException {
    writeContext_.write();
    trans_.write(LBRACKET);
    pushWriteContext(new ListContext());
    // No metadata!
  }

  public void writeListEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACKET);
  }

  public void writeSetBegin(TSet set) throws TException {
    writeContext_.write();
    trans_.write(LBRACKET);
    pushWriteContext(new ListContext());
    // No metadata!
  }

  public void writeSetEnd() throws TException {
    popWriteContext();
    trans_.write(RBRACKET);
  }

  public void writeBool(boolean b) throws TException {
    writeByte(b ? (byte)1 : (byte)0);
  }

  public void writeByte(byte b) throws TException {
    writeI32(b);
  }

  public void writeI16(short i16) throws TException {
    writeI32(i16);
  }

  public void writeI32(int i32) throws TException {
    writeContext_.write();
    _writeStringData(Integer.toString(i32));
  }

  public void _writeStringData(String s) throws TException {
    try {
      byte[] b = s.getBytes("UTF-8");
      trans_.write(b);
    } catch (UnsupportedEncodingException uex) {
      throw new TException("JVM DOES NOT SUPPORT UTF-8");
    }
  }

  public void writeI64(long i64) throws TException {
    writeContext_.write();
    _writeStringData(Long.toString(i64));
  }

  public void writeDouble(double dub) throws TException {
    writeContext_.write();
    _writeStringData(Double.toString(dub));
  }

  public void writeString(String str) throws TException {
    writeContext_.write();
    int length = str.length();
    StringBuffer escape = new StringBuffer(length + 16);
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
        // Control characeters! According to JSON RFC u0020 (space)
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

  public void writeBinary(byte[] bin) throws TException {
    try {
      // TODO(mcslee): Fix this
      writeString(new String(bin, "UTF-8"));
    } catch (UnsupportedEncodingException uex) {
      throw new TException("JVM DOES NOT SUPPORT UTF-8");
    }
  }

  /**
   * Reading methods.
   */

  public TMessage readMessageBegin() throws TException {
    TMessage message = new TMessage();
    // TODO(mcslee): implement
    return message;
  }

  public void readMessageEnd() {}

  public TStruct readStructBegin() {
    // TODO(mcslee): implement
    return new TStruct();
  }

  public void readStructEnd() {}

  public TField readFieldBegin() throws TException {
    TField field = new TField();
    // TODO(mcslee): implement
    return field;
  }

  public void readFieldEnd() {}

  public TMap readMapBegin() throws TException {
    TMap map = new TMap();
    // TODO(mcslee): implement
    return map;
  }

  public void readMapEnd() {}

  public TList readListBegin() throws TException {
    TList list = new TList();
    // TODO(mcslee): implement
    return list;
  }

  public void readListEnd() {}

  public TSet readSetBegin() throws TException {
    TSet set = new TSet();
    // TODO(mcslee): implement
    return set;
  }

  public void readSetEnd() {}

  public boolean readBool() throws TException {
    return (readByte() == 1);
  }

  public byte readByte() throws TException {
    // TODO(mcslee): implement
    return 0;
  }

  public short readI16() throws TException {
    // TODO(mcslee): implement
    return 0;
  }

  public int readI32() throws TException {
    // TODO(mcslee): implement
    return 0;
  }

  public long readI64() throws TException {
    // TODO(mcslee): implement
    return 0;
  }

  public double readDouble() throws TException {
    // TODO(mcslee): implement
    return 0;
  }

  public String readString() throws TException {
    // TODO(mcslee): implement
    return "";
  }

  public String readStringBody(int size) throws TException {
    // TODO(mcslee): implement
    return "";
  }

  public byte[] readBinary() throws TException {
    // TODO(mcslee): implement
    return new byte[0];
  }

}
