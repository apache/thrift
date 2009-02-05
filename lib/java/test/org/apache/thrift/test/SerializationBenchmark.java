
package org.apache.thrift.test;

import java.io.ByteArrayInputStream;

import org.apache.thrift.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;

import thrift.test.*;

public class SerializationBenchmark {
  private final static int HOW_MANY = 10000000;
  
  public static void main(String[] args) throws Exception {
    TProtocolFactory factory = new TBinaryProtocol.Factory();

    OneOfEach ooe = new OneOfEach();
    ooe.im_true   = true;
    ooe.im_false  = false;
    ooe.a_bite    = (byte)0xd6;
    ooe.integer16 = 27000;
    ooe.integer32 = 1<<24;
    ooe.integer64 = (long)6000 * 1000 * 1000;
    ooe.double_precision = Math.PI;
    ooe.some_characters  = "JSON THIS! \"\u0001";
    ooe.base64 = new byte[]{1,2,3,(byte)255};

    testSerialization(factory, ooe);
    testDeserialization(factory, ooe, OneOfEach.class);
  }
  
  public static void testSerialization(TProtocolFactory factory, TBase object) throws Exception {
    TTransport trans = new TTransport() {
      public void write(byte[] bin, int x, int y) throws TTransportException {}
      public int read(byte[] bin, int x, int y) throws TTransportException {return 0;}
      public void close() {}
      public void open() {}
      public boolean isOpen() {return true;}
    };
    
    TProtocol proto = factory.getProtocol(trans);
    
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < HOW_MANY; i++) {
      object.write(proto);
    }
    long endTime = System.currentTimeMillis();
    
    System.out.println("Test time: " + (endTime - startTime) + " ms");
  }
  
  public static <T extends TBase> void testDeserialization(TProtocolFactory factory, T object, Class<T> klass) throws Exception {
    TMemoryBuffer buf = new TMemoryBuffer(0);
    object.write(factory.getProtocol(buf));
    byte[] serialized = new byte[100*1024];
    buf.read(serialized, 0, 100*1024);
    
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < HOW_MANY; i++) {
      T o2 = klass.newInstance();
      o2.read(factory.getProtocol(new TIOStreamTransport(new ByteArrayInputStream(serialized))));
    }
    long endTime = System.currentTimeMillis();
    
    System.out.println("Test time: " + (endTime - startTime) + " ms");
  }
}