package org.apache.thrift.test;

// Generated code
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.transport.TMemoryBuffer;

import thrift.test.Base64;
import thrift.test.Bonk;
import thrift.test.HolyMoley;
import thrift.test.Nesting;
import thrift.test.OneOfEach;

/**
 * Tests for the Java implementation of TJSONProtocol. Mirrors the C++ version
 *
 */
public class JSONProtoTest {

  private static final byte[] kUnicodeBytes = {
    (byte)0xd3, (byte)0x80, (byte)0xe2, (byte)0x85, (byte)0xae, (byte)0xce,
    (byte)0x9d, (byte)0x20, (byte)0xd0, (byte)0x9d, (byte)0xce, (byte)0xbf,
    (byte)0xe2, (byte)0x85, (byte)0xbf, (byte)0xd0, (byte)0xbe, (byte)0xc9,
    (byte)0xa1, (byte)0xd0, (byte)0xb3, (byte)0xd0, (byte)0xb0, (byte)0xcf,
    (byte)0x81, (byte)0xe2, (byte)0x84, (byte)0x8e, (byte)0x20, (byte)0xce,
    (byte)0x91, (byte)0x74, (byte)0x74, (byte)0xce, (byte)0xb1, (byte)0xe2,
    (byte)0x85, (byte)0xbd, (byte)0xce, (byte)0xba, (byte)0x83, (byte)0xe2,
    (byte)0x80, (byte)0xbc
  };

  public static void main(String [] args) throws Exception {
   try {
      System.out.println("In JSON Proto test");

      OneOfEach ooe = new OneOfEach();
      ooe.im_true   = true;
      ooe.im_false  = false;
      ooe.a_bite    = (byte)0xd6;
      ooe.integer16 = 27000;
      ooe.integer32 = 1<<24;
      ooe.integer64 = (long)6000 * 1000 * 1000;
      ooe.double_precision = Math.PI;
      ooe.some_characters  = "JSON THIS! \"\1";
      ooe.zomg_unicode     = new String(kUnicodeBytes, "UTF-8");


      Nesting n = new Nesting(new Bonk(), new OneOfEach());
      n.my_ooe.integer16 = 16;
      n.my_ooe.integer32 = 32;
      n.my_ooe.integer64 = 64;
      n.my_ooe.double_precision = (Math.sqrt(5)+1)/2;
      n.my_ooe.some_characters  = ":R (me going \"rrrr\")";
      n.my_ooe.zomg_unicode     = new String(kUnicodeBytes, "UTF-8");
      n.my_bonk.type    = 31337;
      n.my_bonk.message = "I am a bonk... xor!";

      HolyMoley hm = new HolyMoley();

      hm.big = new ArrayList<OneOfEach>();
      hm.big.add(ooe);
      hm.big.add(n.my_ooe);
      hm.big.get(0).a_bite = (byte)0x22;
      hm.big.get(1).a_bite = (byte)0x23;

      hm.contain = new HashSet<List<String>>();
      ArrayList<String> stage1 = new ArrayList<String>(2);
      stage1.add("and a one");
      stage1.add("and a two");
      hm.contain.add(stage1);
      stage1 = new ArrayList<String>(3);
      stage1.add("then a one, two");
      stage1.add("three!");
      stage1.add("FOUR!!");
      hm.contain.add(stage1);
      stage1 = new ArrayList<String>(0);
      hm.contain.add(stage1);

      ArrayList<Bonk> stage2 = new ArrayList<Bonk>();
      hm.bonks = new HashMap<String, List<Bonk>>();
      hm.bonks.put("nothing", stage2);
      Bonk b = new Bonk();
      b.type = 1;
      b.message = "Wait.";
      stage2.add(b);
      b = new Bonk();
      b.type = 2;
      b.message = "What?";
      stage2.add(b);
      stage2 = new ArrayList<Bonk>();
      hm.bonks.put("something", stage2);
      b = new Bonk();
      b.type = 3;
      b.message = "quoth";
      b = new Bonk();
      b.type = 4;
      b.message = "the raven";
      b = new Bonk();
      b.type = 5;
      b.message = "nevermore";
      hm.bonks.put("poe", stage2);

      TMemoryBuffer buffer = new TMemoryBuffer(1024);
      TJSONProtocol proto = new TJSONProtocol(buffer);

      System.out.println("Writing ooe");
      ooe.write(proto);
      System.out.println("Reading ooe");
      OneOfEach ooe2 = new OneOfEach();
      ooe2.read(proto);

      System.out.println("Comparing ooe");
      if (!ooe.equals(ooe2)) {
        throw new RuntimeException("ooe != ooe2");
      }

      System.out.println("Writing hm");
      hm.write(proto);

      System.out.println("Reading hm");
      HolyMoley hm2 = new HolyMoley();
      hm2.read(proto);

      System.out.println("Comparing hm");
      if (!hm.equals(hm2)) {
        throw new RuntimeException("hm != hm2");
      }

      hm2.big.get(0).a_bite = (byte)0xFF;
      if (hm.equals(hm2)) {
        throw new RuntimeException("hm should not equal hm2");
      }

      Base64 base = new Base64();
      base.a = 123;
      base.b1 = "1".getBytes("UTF-8");
      base.b2 = "12".getBytes("UTF-8");
      base.b3 = "123".getBytes("UTF-8");
      base.b4 = "1234".getBytes("UTF-8");
      base.b5 = "12345".getBytes("UTF-8");
      base.b6 = "123456".getBytes("UTF-8");

      System.out.println("Writing base");
      base.write(proto);

      System.out.println("Reading base");
      Base64 base2 = new Base64();
      base2.read(proto);

      System.out.println("Comparing base");
      if (!base.equals(base2)) {
        throw new RuntimeException("base != base2");
      }

    } catch (Exception ex) {
      ex.printStackTrace();
      throw ex;
   }
  }

}
