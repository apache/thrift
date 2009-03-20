package org.apache.thrift.test;

// Generated code
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import org.apache.thrift.TDeserializer;
import org.apache.thrift.TSerializer;
import org.apache.thrift.protocol.TBinaryProtocol;

import thrift.test.Bonk;
import thrift.test.HolyMoley;
import thrift.test.Nesting;
import thrift.test.OneOfEach;

/**
 *
 */
public class IdentityTest {
  public static Object deepCopy(Object oldObj) throws Exception {
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

  public static void main(String[] args) throws Exception {
    TSerializer   binarySerializer   = new   TSerializer(new TBinaryProtocol.Factory());
    TDeserializer binaryDeserializer = new TDeserializer(new TBinaryProtocol.Factory());

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

    Nesting n = new Nesting();
    n.my_ooe = (OneOfEach)deepCopy(ooe);
    n.my_ooe.integer16 = 16;
    n.my_ooe.integer32 = 32;
    n.my_ooe.integer64 = 64;
    n.my_ooe.double_precision = (Math.sqrt(5)+1)/2;
    n.my_ooe.some_characters  = ":R (me going \"rrrr\")";
    n.my_ooe.zomg_unicode     = "\u04c0\u216e\u039d\u0020\u041d\u03bf\u217f"+
                                "\u043e\u0261\u0433\u0430\u03c1\u210e\u0020"+
                                "\u0391\u0074\u0074\u03b1\u217d\u03ba\u01c3"+
                                "\u203c";
    n.my_bonk = new Bonk();
    n.my_bonk.type    = 31337;
    n.my_bonk.message = "I am a bonk... xor!";

    HolyMoley hm = new HolyMoley();
    hm.big = new ArrayList<OneOfEach>();
    hm.contain = new HashSet<List<String>>();
    hm.bonks = new HashMap<String,List<Bonk>>();

    hm.big.add((OneOfEach)deepCopy(ooe));
    hm.big.add((OneOfEach)deepCopy(n.my_ooe));
    hm.big.get(0).a_bite = 0x22;
    hm.big.get(1).a_bite = 0x33;

    List<String> stage1 = new ArrayList<String>();
    stage1.add("and a one");
    stage1.add("and a two");
    hm.contain.add(stage1);
    stage1 = new ArrayList<String>();
    stage1.add("then a one, two");
    stage1.add("three!");
    stage1.add("FOUR!!");
    hm.contain.add(stage1);
    stage1 = new ArrayList<String>();
    hm.contain.add(stage1);

    List<Bonk> stage2 = new ArrayList<Bonk>();
    hm.bonks.put("nothing", stage2);
    stage2.add(new Bonk());
    stage2.get(0).type = 1;
    stage2.get(0).message = "Wait.";
    stage2.add(new Bonk());
    stage2.get(1).type = 2;
    stage2.get(1).message = "What?";
    hm.bonks.put("something", stage2);
    stage2 = new ArrayList<Bonk>();
    stage2.add(new Bonk());
    stage2.get(0).type = 3;
    stage2.get(0).message = "quoth";
    stage2.add(new Bonk());
    stage2.get(1).type = 4;
    stage2.get(1).message = "the raven";
    stage2.add(new Bonk());
    stage2.get(2).type = 5;
    stage2.get(2).message = "nevermore";
    hm.bonks.put("poe", stage2);

    OneOfEach ooe2 = new OneOfEach();
    binaryDeserializer.deserialize(
        ooe2,
        binarySerializer.serialize(ooe));

    if (!ooe.equals(ooe2)) {
      throw new RuntimeException("Failure: ooe (equals)");
    }
    if (ooe.hashCode() != ooe2.hashCode()) {
      throw new RuntimeException("Failure: ooe (hash)");
    }


    Nesting n2 = new Nesting();
    binaryDeserializer.deserialize(
        n2,
        binarySerializer.serialize(n));

    if (!n.equals(n2)) {
      throw new RuntimeException("Failure: n (equals)");
    }
    if (n.hashCode() != n2.hashCode()) {
      throw new RuntimeException("Failure: n (hash)");
    }

    HolyMoley hm2 = new HolyMoley();
    binaryDeserializer.deserialize(
        hm2,
        binarySerializer.serialize(hm));

    if (!hm.equals(hm2)) {
      throw new RuntimeException("Failure: hm (equals)");
    }
    if (hm.hashCode() != hm2.hashCode()) {
      throw new RuntimeException("Failure: hm (hash)");
    }

  }
}
