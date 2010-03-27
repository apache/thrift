package org.apache.thrift;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import junit.framework.TestCase;

import org.apache.thrift.protocol.TBinaryProtocol;

import thrift.test.Bonk;
import thrift.test.HolyMoley;
import thrift.test.Nesting;
import thrift.test.OneOfEach;

public class TestStruct extends TestCase {

  public static Object deepCopyViaSerialization(Object oldObj) throws Exception {
    ObjectOutputStream oos = null;
    ObjectInputStream ois = null;
    try {
      ByteArrayOutputStream bos =
        new ByteArrayOutputStream();
      oos = new ObjectOutputStream(bos);
      oos.writeObject(oldObj);
      oos.flush();
      ByteArrayInputStream bis =
        new ByteArrayInputStream(bos.toByteArray());
      ois = new ObjectInputStream(bis);
      return ois.readObject();
    } finally {
      oos.close();
      ois.close();
    }
  }

  public void testIdentity() throws Exception {
    TSerializer   binarySerializer   = new   TSerializer(new TBinaryProtocol.Factory());
    TDeserializer binaryDeserializer = new TDeserializer(new TBinaryProtocol.Factory());

    OneOfEach ooe = Fixtures.oneOfEach;

    Nesting n = new Nesting();
    n.my_ooe = (OneOfEach)deepCopyViaSerialization(ooe);
    n.my_ooe.integer16 = 16;
    n.my_ooe.integer32 = 32;
    n.my_ooe.integer64 = 64;
    n.my_ooe.double_precision = (Math.sqrt(5)+1)/2;
    n.my_ooe.some_characters  = ":R (me going \"rrrr\")";
    n.my_ooe.zomg_unicode     = "\u04c0\u216e\u039d\u0020\u041d\u03bf\u217f"+
                                "\u043e\u0261\u0433\u0430\u03c1\u210e\u0020"+
                                "\u0391\u0074\u0074\u03b1\u217d\u03ba\u01c3"+
                                "\u203c";
    n.my_bonk = Fixtures.nesting.my_bonk;

    HolyMoley hm = Fixtures.holyMoley;

    OneOfEach ooe2 = new OneOfEach();
    binaryDeserializer.deserialize(
        ooe2,
        binarySerializer.serialize(ooe));

    assertEquals(ooe, ooe2);
    assertEquals(ooe.hashCode(), ooe2.hashCode());


    Nesting n2 = new Nesting();
    binaryDeserializer.deserialize(
        n2,
        binarySerializer.serialize(n));

    assertEquals(n, n2);
    assertEquals(n.hashCode(), n2.hashCode());

    HolyMoley hm2 = new HolyMoley();
    binaryDeserializer.deserialize(
        hm2,
        binarySerializer.serialize(hm));

    assertEquals(hm, hm2);
    assertEquals(hm.hashCode(), hm2.hashCode());
  }

  public void testDeepCopy() throws Exception {
    TSerializer   binarySerializer   = new   TSerializer(new TBinaryProtocol.Factory());
    TDeserializer binaryDeserializer = new TDeserializer(new TBinaryProtocol.Factory());

    HolyMoley hm = Fixtures.holyMoley;

    byte[] binaryCopy = binarySerializer.serialize(hm);
    HolyMoley hmCopy = new HolyMoley();
    binaryDeserializer.deserialize(hmCopy, binaryCopy);
    HolyMoley hmCopy2 = new HolyMoley(hm);

    assertEquals(hm, hmCopy);
    assertEquals(hmCopy, hmCopy2);

    // change binary value in original object
    hm.big.get(0).base64[0]++;
    // make sure the change didn't propagate to the copied object
    assertFalse(hm.equals(hmCopy2));
    hm.big.get(0).base64[0]--; // undo change

    hmCopy2.bonks.get("two").get(1).message = "What else?";

    assertFalse(hm.equals(hmCopy2));
  }

  public void testCompareTo() throws Exception {
    Bonk bonk1 = new Bonk();
    Bonk bonk2 = new Bonk();

    // Compare empty thrift objects.
    assertEquals(0, bonk1.compareTo(bonk2));

    bonk1.setMessage("m");

    // Compare one thrift object with a filled in field and another without it.
    assertTrue(bonk1.compareTo(bonk2) > 0);
    assertTrue(bonk2.compareTo(bonk1) < 0);

    // Compare both have filled-in fields.
    bonk2.setMessage("z");
    assertTrue(bonk1.compareTo(bonk2) < 0);
    assertTrue(bonk2.compareTo(bonk1) > 0);

    // Compare bonk1 has a field filled in that bonk2 doesn't.
    bonk1.setType(123);
    assertTrue(bonk1.compareTo(bonk2) > 0);
    assertTrue(bonk2.compareTo(bonk1) < 0);

    // Compare bonk1 and bonk2 equal.
    bonk2.setType(123);
    bonk2.setMessage("m");
    assertEquals(0, bonk1.compareTo(bonk2));
  }
}
