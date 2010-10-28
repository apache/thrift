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


package org.apache.thrift.test;

import java.util.Arrays;
import java.util.List;

import org.apache.thrift.TBase;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TMessage;
import org.apache.thrift.protocol.TMessageType;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.protocol.TStruct;
import org.apache.thrift.protocol.TType;
import org.apache.thrift.transport.TMemoryBuffer;

import thrift.test.CompactProtoTestStruct;
import thrift.test.HolyMoley;
import thrift.test.Nesting;
import thrift.test.OneOfEach;
import thrift.test.Srv;

public class TCompactProtocolTest {

  static TProtocolFactory factory = new TCompactProtocol.Factory();

  public static void main(String[] args) throws Exception {
    testNakedByte();
    for (int i = 0; i < 128; i++) {
      testByteField((byte)i);
      testByteField((byte)-i);
    }
    
    testNakedI16((short)0);
    testNakedI16((short)1);
    testNakedI16((short)15000);
    testNakedI16((short)0x7fff);
    testNakedI16((short)-1);
    testNakedI16((short)-15000);
    testNakedI16((short)-0x7fff);
    
    testI16Field((short)0);
    testI16Field((short)1);
    testI16Field((short)7);
    testI16Field((short)150);
    testI16Field((short)15000);
    testI16Field((short)0x7fff);
    testI16Field((short)-1);
    testI16Field((short)-7);
    testI16Field((short)-150);
    testI16Field((short)-15000);
    testI16Field((short)-0x7fff);
    
    testNakedI32(0);
    testNakedI32(1);
    testNakedI32(15000);
    testNakedI32(0xffff);
    testNakedI32(-1);
    testNakedI32(-15000);
    testNakedI32(-0xffff);
    
    testI32Field(0);
    testI32Field(1);
    testI32Field(7);
    testI32Field(150);
    testI32Field(15000);
    testI32Field(31337);
    testI32Field(0xffff);
    testI32Field(0xffffff);
    testI32Field(-1);
    testI32Field(-7);
    testI32Field(-150);
    testI32Field(-15000);
    testI32Field(-0xffff);
    testI32Field(-0xffffff);
    
    testNakedI64(0);
    for (int i = 0; i < 62; i++) {
      testNakedI64(1L << i);
      testNakedI64(-(1L << i));
    }

    testI64Field(0);
    for (int i = 0; i < 62; i++) {
      testI64Field(1L << i);
      testI64Field(-(1L << i));
    }

    testDouble();
    
    testNakedString("");
    testNakedString("short");
    testNakedString("borderlinetiny");
    testNakedString("a bit longer than the smallest possible");
    
    testStringField("");
    testStringField("short");
    testStringField("borderlinetiny");
    testStringField("a bit longer than the smallest possible");
    
    testNakedBinary(new byte[]{});
    testNakedBinary(new byte[]{0,1,2,3,4,5,6,7,8,9,10});
    testNakedBinary(new byte[]{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14});
    testNakedBinary(new byte[128]);
    
    testBinaryField(new byte[]{});
    testBinaryField(new byte[]{0,1,2,3,4,5,6,7,8,9,10});
    testBinaryField(new byte[]{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14});
    testBinaryField(new byte[128]);
    
    testSerialization(OneOfEach.class, Fixtures.oneOfEach);
    testSerialization(Nesting.class, Fixtures.nesting);
    testSerialization(HolyMoley.class, Fixtures.holyMoley);
    testSerialization(CompactProtoTestStruct.class, Fixtures.compactProtoTestStruct);
    
    testMessage();
    
    testServerRequest();
  }
  
  public static void testNakedByte() throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    TProtocol proto = factory.getProtocol(buf);
    proto.writeByte((byte)123);
    byte out = proto.readByte();
    if (out != 123) {
      throw new RuntimeException("Byte was supposed to be " + (byte)123 + " but was " + out);
    }
  }
  
  public static void testByteField(final byte b) throws Exception {
    testStructField(new StructFieldTestCase(TType.BYTE, (short)15) {
      public void writeMethod(TProtocol proto) throws TException {
        proto.writeByte(b);
      }
      
      public void readMethod(TProtocol proto) throws TException {
        byte result = proto.readByte();
        if (result != b) {
          throw new RuntimeException("Byte was supposed to be " + (byte)b + " but was " + result);
        }
      }
    });
  }

  public static void testNakedI16(short n) throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    TProtocol proto = factory.getProtocol(buf);
    proto.writeI16(n);
    // System.out.println(buf.inspect());
    int out = proto.readI16();
    if (out != n) {
      throw new RuntimeException("I16 was supposed to be " + n + " but was " + out);
    }
  }

  public static void testI16Field(final short n) throws Exception {
    testStructField(new StructFieldTestCase(TType.I16, (short)15) {
      public void writeMethod(TProtocol proto) throws TException {
        proto.writeI16(n);
      }
      
      public void readMethod(TProtocol proto) throws TException {
        short result = proto.readI16();
        if (result != n) {
          throw new RuntimeException("I16 was supposed to be " + n + " but was " + result);
        }
      }
    });
  }
  
  public static void testNakedI32(int n) throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    TProtocol proto = factory.getProtocol(buf);
    proto.writeI32(n);
    // System.out.println(buf.inspect());
    int out = proto.readI32();
    if (out != n) {
      throw new RuntimeException("I32 was supposed to be " + n + " but was " + out);
    }
  }
  
  public static void testI32Field(final int n) throws Exception {
    testStructField(new StructFieldTestCase(TType.I32, (short)15) {
      public void writeMethod(TProtocol proto) throws TException {
        proto.writeI32(n);
      }
      
      public void readMethod(TProtocol proto) throws TException {
        int result = proto.readI32();
        if (result != n) {
          throw new RuntimeException("I32 was supposed to be " + n + " but was " + result);
        }
      }
    });
    
  }

  public static void testNakedI64(long n) throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    TProtocol proto = factory.getProtocol(buf);
    proto.writeI64(n);
    // System.out.println(buf.inspect());
    long out = proto.readI64();
    if (out != n) {
      throw new RuntimeException("I64 was supposed to be " + n + " but was " + out);
    }
  }
  
  public static void testI64Field(final long n) throws Exception {
    testStructField(new StructFieldTestCase(TType.I64, (short)15) {
      public void writeMethod(TProtocol proto) throws TException {
        proto.writeI64(n);
      }
      
      public void readMethod(TProtocol proto) throws TException {
        long result = proto.readI64();
        if (result != n) {
          throw new RuntimeException("I64 was supposed to be " + n + " but was " + result);
        }
      }
    });
  }
    
  public static void testDouble() throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(1000);
    TProtocol proto = factory.getProtocol(buf);
    proto.writeDouble(123.456);
    double out = proto.readDouble();
    if (out != 123.456) {
      throw new RuntimeException("Double was supposed to be " + 123.456 + " but was " + out);
    }
  }
    
  public static void testNakedString(String str) throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    TProtocol proto = factory.getProtocol(buf);
    proto.writeString(str);
    // System.out.println(buf.inspect());
    String out = proto.readString();
    if (!str.equals(out)) {
      throw new RuntimeException("String was supposed to be '" + str + "' but was '" + out + "'");
    }
  }
  
  public static void testStringField(final String str) throws Exception {
    testStructField(new StructFieldTestCase(TType.STRING, (short)15) {
      public void writeMethod(TProtocol proto) throws TException {
        proto.writeString(str);
      }
      
      public void readMethod(TProtocol proto) throws TException {
        String result = proto.readString();
        if (!result.equals(str)) {
          throw new RuntimeException("String was supposed to be " + str + " but was " + result);
        }
      }
    });
  }

  public static void testNakedBinary(byte[] data) throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    TProtocol proto = factory.getProtocol(buf);
    proto.writeBinary(data);
    // System.out.println(buf.inspect());
    byte[] out = proto.readBinary();
    if (!Arrays.equals(data, out)) {
      throw new RuntimeException("Binary was supposed to be '" + data + "' but was '" + out + "'");
    }
  }

  public static void testBinaryField(final byte[] data) throws Exception {
    testStructField(new StructFieldTestCase(TType.STRING, (short)15) {
      public void writeMethod(TProtocol proto) throws TException {
        proto.writeBinary(data);
      }
      
      public void readMethod(TProtocol proto) throws TException {
        byte[] result = proto.readBinary();
        if (!Arrays.equals(data, result)) {
          throw new RuntimeException("Binary was supposed to be '" + bytesToString(data) + "' but was '" + bytesToString(result) + "'");
        }
      }
    });
    
  }

  public static <T extends TBase> void testSerialization(Class<T> klass, T obj) throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    TBinaryProtocol binproto = new TBinaryProtocol(buf);
    
    try {
      obj.write(binproto);
      // System.out.println("Size in binary protocol: " + buf.length());
    
      buf = new TMemoryBuffer(0);
      TProtocol proto = factory.getProtocol(buf);
    
      obj.write(proto);
      System.out.println("Size in compact protocol: " + buf.length());
      // System.out.println(buf.inspect());
    
      T objRead = klass.newInstance();
      objRead.read(proto);
      if (!obj.equals(objRead)) {
        System.out.println("Expected: " + obj.toString());
        System.out.println("Actual: " + objRead.toString());
        // System.out.println(buf.inspect());
        throw new RuntimeException("Objects didn't match!");
      }
    } catch (Exception e) {
      System.out.println(buf.inspect());
      throw e;
    }
  }

  public static void testMessage() throws Exception {
    List<TMessage> msgs = Arrays.asList(new TMessage[]{
      new TMessage("short message name", TMessageType.CALL, 0),
      new TMessage("1", TMessageType.REPLY, 12345),
      new TMessage("loooooooooooooooooooooooooooooooooong", TMessageType.EXCEPTION, 1 << 16),
      new TMessage("Janky", TMessageType.CALL, 0),
    });
    
    for (TMessage msg : msgs) {
      TMemoryBuffer buf = new TMemoryBuffer(0);
      TProtocol proto = factory.getProtocol(buf);
      TMessage output = null;
      
      proto.writeMessageBegin(msg);
      proto.writeMessageEnd();

      output = proto.readMessageBegin();

      if (!msg.equals(output)) {
        throw new RuntimeException("Message was supposed to be " + msg + " but was " + output);
      }
    }
  }

  public static void testServerRequest() throws Exception {
    Srv.Iface handler = new Srv.Iface() {
      public int Janky(int i32arg) throws TException {
        return i32arg * 2;
      }

      public int primitiveMethod() throws TException {
        // TODO Auto-generated method stub
        return 0;
      }

      public CompactProtoTestStruct structMethod() throws TException {
        // TODO Auto-generated method stub
        return null;
      }

      public void voidMethod() throws TException {
        // TODO Auto-generated method stub
        
      }
    };
    
    Srv.Processor testProcessor = new Srv.Processor(handler);

    TMemoryBuffer clientOutTrans = new TMemoryBuffer(0);
    TProtocol clientOutProto = factory.getProtocol(clientOutTrans);
    TMemoryBuffer clientInTrans = new TMemoryBuffer(0);
    TProtocol clientInProto = factory.getProtocol(clientInTrans);
    
    Srv.Client testClient = new Srv.Client(clientInProto, clientOutProto);
    
    testClient.send_Janky(1);
    // System.out.println(clientOutTrans.inspect());
    testProcessor.process(clientOutProto, clientInProto);
    // System.out.println(clientInTrans.inspect());
    int result = testClient.recv_Janky();
    if (result != 2) {
      throw new RuntimeException("Got an unexpected result: " + result);
    }
  }

  //
  // Helper methods
  //
  
  private static String bytesToString(byte[] bytes) {
    String s = "";
    for (int i = 0; i < bytes.length; i++) {
      s += Integer.toHexString((int)bytes[i]) + " ";
    }
    return s;
  }

  private static void testStructField(StructFieldTestCase testCase) throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    TProtocol proto = factory.getProtocol(buf);
    
    TField field = new TField("test_field", testCase.type_, testCase.id_);
    proto.writeStructBegin(new TStruct("test_struct"));
    proto.writeFieldBegin(field);
    testCase.writeMethod(proto);
    proto.writeFieldEnd();
    proto.writeStructEnd();
    
    // System.out.println(buf.inspect());

    proto.readStructBegin();
    TField readField = proto.readFieldBegin();
    // TODO: verify the field is as expected
    if (!field.equals(readField)) {
      throw new RuntimeException("Expected " + field + " but got " + readField);
    }
    testCase.readMethod(proto);
    proto.readStructEnd();
  }
  
  public static abstract class StructFieldTestCase {
    byte type_;
    short id_;
    public StructFieldTestCase(byte type, short id) {
      type_ = type;
      id_ = id;
    }
    
    public abstract void writeMethod(TProtocol proto) throws TException;
    public abstract void readMethod(TProtocol proto) throws TException;
  }
}