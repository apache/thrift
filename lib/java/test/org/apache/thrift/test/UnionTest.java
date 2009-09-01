package org.apache.thrift.test;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TMemoryBuffer;

import thrift.test.Empty;
import thrift.test.StructWithAUnion;
import thrift.test.TestUnion;

public class UnionTest {

  /**
   * @param args
   */
  public static void main(String[] args) throws Exception {
    testBasic();
    testEquality();    
    testSerialization();
  }

  public static void testBasic() throws Exception {
    TestUnion union = new TestUnion();

    if (union.isSet()) {
      throw new RuntimeException("new union with default constructor counts as set!");
    }

    if (union.getFieldValue() != null) {
      throw new RuntimeException("unset union didn't return null for value");
    }

    union = new TestUnion(TestUnion.I32_FIELD, 25);

    if ((Integer)union.getFieldValue() != 25) {
      throw new RuntimeException("set i32 field didn't come out as planned");
    }

    if ((Integer)union.getFieldValue(TestUnion.I32_FIELD) != 25) {
      throw new RuntimeException("set i32 field didn't come out of TBase getFieldValue");
    }

    try {
      union.getFieldValue(TestUnion.STRING_FIELD);
      throw new RuntimeException("was expecting an exception around wrong set field");
    } catch (IllegalArgumentException e) {
      // cool!
    }

    System.out.println(union);

    union = new TestUnion();
    union.setI32_field(1);
    if (union.getI32_field() != 1) {
      throw new RuntimeException("didn't get the right value for i32 field!");
    }

    try {
      union.getString_field();
      throw new RuntimeException("should have gotten an exception");
    } catch (Exception e) {
      // sweet
    }

  }


  public static void testEquality() throws Exception {
    TestUnion union = new TestUnion(TestUnion.I32_FIELD, 25);

    TestUnion otherUnion = new TestUnion(TestUnion.STRING_FIELD, "blah!!!");

    if (union.equals(otherUnion)) {
      throw new RuntimeException("shouldn't be equal");
    }

    otherUnion = new TestUnion(TestUnion.I32_FIELD, 400);

    if (union.equals(otherUnion)) {
      throw new RuntimeException("shouldn't be equal");
    }

    otherUnion = new TestUnion(TestUnion.OTHER_I32_FIELD, 25);

    if (union.equals(otherUnion)) {
      throw new RuntimeException("shouldn't be equal");
    }
  }


  public static void testSerialization() throws Exception {
    TestUnion union = new TestUnion(TestUnion.I32_FIELD, 25);

    TMemoryBuffer buf = new TMemoryBuffer(0);
    TProtocol proto = new TBinaryProtocol(buf);

    union.write(proto);

    TestUnion u2 = new TestUnion();

    u2.read(proto);

    if (!u2.equals(union)) {
      throw new RuntimeException("serialization fails!");
    }

    StructWithAUnion swau = new StructWithAUnion(u2);

    buf = new TMemoryBuffer(0);
    proto = new TBinaryProtocol(buf);

    swau.write(proto);

    StructWithAUnion swau2 = new StructWithAUnion();
    if (swau2.equals(swau)) {
      throw new RuntimeException("objects match before they are supposed to!");
    }
    swau2.read(proto);
    if (!swau2.equals(swau)) {
      throw new RuntimeException("objects don't match when they are supposed to!");
    }

    // this should NOT throw an exception.
    buf = new TMemoryBuffer(0);
    proto = new TBinaryProtocol(buf);

    swau.write(proto);
    new Empty().read(proto);

  }
}
