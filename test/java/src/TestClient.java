package com.facebook.thrift.test;

import ThriftTest.*;
import com.facebook.thrift.types.*;
import com.facebook.thrift.transport.TSocket;
import com.facebook.thrift.transport.TTransportException;
import com.facebook.thrift.protocol.TBinaryProtocol;
import com.facebook.thrift.protocol.TString;

import java.util.HashMap;
import java.util.HashSet;
import java.util.ArrayList;

/**
 * Test Java client for thrift. Essentially just a copy of the C++ version,
 * this makes a variety of requests to enable testing for both performance and
 * correctness of the output.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TestClient {
  public static void main(String [] args) {
    try {
      String host = "localhost";
      int port = 9090;
      int numTests = 1;
      
      if (args.length > 0) {
        host = args[0];
      }
      if (args.length > 1) {
        port = Integer.valueOf(args[1]);
      }
      if (args.length > 2) {
        numTests = Integer.valueOf(args[2]);
      }
      
      TSocket tSocket =
        new TSocket(host, port);
      TBinaryProtocol binaryProtocol =
        new TBinaryProtocol();
      ThriftTestClient testClient =
        new ThriftTestClient(tSocket, binaryProtocol);

      for (int test = 0; test < numTests; ++test) {

        /**
         * CONNECT TEST
         */
        System.out.println("Test #" + (test+1) + ", " +
                           "connect " + host + ":" + port);
        try {
          tSocket.open();
        } catch (TTransportException ttx) {
          System.out.println("Connect failed: " + ttx.getMessage());
          continue;
        }

        long start = System.currentTimeMillis();
    
        /**
         * VOID TEST
         */
        System.out.print("testVoid()");
        testClient.testVoid();
        System.out.print(" = void\n");

        /**
         * STRING TEST
         */
        System.out.print("testString(\"Test\")");
        TString s = testClient.testString(new TString("Test"));
        System.out.print(" = \"" + s.value + "\"\n");
   
        /**
         * BYTE TEST
         */
        System.out.print("testByte(1)");
        UInt8 u8 = testClient.testByte(new UInt8((short)1));
        System.out.print(" = " + u8.get() + "\n");

        /**
         * U32 TEST
         */
        System.out.print("testU32(1)");
        UInt32 u32 = testClient.testU32(new UInt32(1));
        System.out.print(" = " + u32.get() + "\n");
    
        /**
         * I32 TEST
         */
        System.out.print("testI32(-1)");
        Int32 i32 = testClient.testI32(new Int32(-1));
        System.out.print(" = " + i32.get() + "\n");

        /**
         * U64 TEST
         */
        System.out.print("testU64(34359738368)");
        UInt64 u64 = testClient.testU64(new UInt64(34359738368L));
        System.out.print(" = " + u64.toLong() + "\n");

        /**
         * I64 TEST
         */
        System.out.print("testI64(-34359738368)");
        Int64 i64 = testClient.testI64(new Int64(-34359738368L));
        System.out.print(" = " + i64.get() + "\n");

        /**
         * STRUCT TEST
         */
        System.out.print("testStruct({\"Zero\", 1, 2, -3, 4, -5})");
        Xtruct out = new Xtruct();
        out.string_thing.value = "Zero";
        out.byte_thing.set((short)1);
        out.u32_thing.set(2);
        out.i32_thing.set(-3);
        out.u64_thing.set(4);
        out.i64_thing.set(-5);
        Xtruct in = testClient.testStruct(out);
        System.out.print(" = {" +
                         "\"" + in.string_thing.value + "\", " +
                         in.byte_thing.get() + ", " +
                         in.u32_thing.get() + ", " +
                         in.i32_thing.get() + ", " +
                         in.u64_thing.toLong() + ", " +
                         in.i64_thing.get() + "}\n");

        /**
         * NESTED STRUCT TEST
         */
        System.out.print("testNest({1, {\"Zero\", 1, 2, -3, 4, -5}), 5}");
        Xtruct2 out2 = new Xtruct2();
        out2.byte_thing.set((short)1);
        out2.struct_thing = out;
        out2.i32_thing.set(5);
        Xtruct2 in2 = testClient.testNest(out2);
        in = in2.struct_thing;
        System.out.print(" = {" +
                         in2.byte_thing.get() + ", {" +
                         "\"" + in.string_thing.value + "\", " +
                         in.byte_thing.get() + ", " +
                         in.u32_thing.get() + ", " +
                         in.i32_thing.get() + ", " +
                         in.u64_thing.toLong() + ", " +
                         in.i64_thing.get() + "}, " +
                         in2.i32_thing.get() + "}\n");

        /**
         * MAP TEST
         */
        HashMap<Int32,Int32> mapout = new HashMap<Int32,Int32>();
        for (int i = 0; i < 5; ++i) {
          mapout.put(new Int32(i), new Int32(i-10));
        }
        System.out.print("testMap({");
        boolean first = true;
        for (Int32 key : mapout.keySet()) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(key.get() + " => " + mapout.get(key).get());
        }
        System.out.print("})");
        HashMap<Int32,Int32> mapin = testClient.testMap(mapout);
        System.out.print(" = {");
        first = true;
        for (Int32 key : mapin.keySet()) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(key.get() + " => " + mapout.get(key).get());
        }
        System.out.print("}\n");

        /**
         * SET TEST
         */
        HashSet<Int32> setout = new HashSet<Int32>();
        for (int i = -2; i < 3; ++i) {
          setout.add(new Int32(i));
        }
        System.out.print("testSet({");
        first = true;
        for (Int32 elem : setout) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem.get());
        }
        System.out.print("})");
        HashSet<Int32> setin = testClient.testSet(setout);
        System.out.print(" = {");
        first = true;
        for (Int32 elem : setin) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem.get());
        }
        System.out.print("}\n");

        /**
         * LIST TEST
         */
        ArrayList<Int32> listout = new ArrayList<Int32>();
        for (int i = -2; i < 3; ++i) {
          listout.add(new Int32(i));
        }
        System.out.print("testList({");
        first = true;
        for (Int32 elem : listout) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem.get());
        }
        System.out.print("})");
        ArrayList<Int32> listin = testClient.testList(listout);
        System.out.print(" = {");
        first = true;
        for (Int32 elem : listin) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem.get());
        }
        System.out.print("}\n");

        /**
         * ENUM TEST
         */
        System.out.print("testEnum(ONE)");
        Int32 ret = testClient.testEnum(Numberz.ONE);
        System.out.print(" = " + ret.get() + "\n");

        System.out.print("testEnum(TWO)");
        ret = testClient.testEnum(Numberz.TWO);
        System.out.print(" = " + ret.get() + "\n");

        System.out.print("testEnum(THREE)");
        ret = testClient.testEnum(Numberz.THREE);
        System.out.print(" = " + ret.get() + "\n");

        System.out.print("testEnum(FIVE)");
        ret = testClient.testEnum(Numberz.FIVE);
        System.out.print(" = " + ret.get() + "\n");

        System.out.print("testEnum(EIGHT)");
        ret = testClient.testEnum(Numberz.EIGHT);
        System.out.print(" = " + ret.get() + "\n");

        /**
         * TYPEDEF TEST
         */
        System.out.print("testTypedef(309858235082523)");
        UInt64 uid = testClient.testTypedef(new UInt64(309858235082523L));
        System.out.print(" = " + uid.toLong() + "\n");

        /**
         * NESTED MAP TEST
         */
        System.out.print("testMapMap(1)");
        HashMap<Int32,HashMap<Int32,Int32>> mm =
          testClient.testMapMap(new Int32(1));
        System.out.print(" = {");
        for (Int32 key : mm.keySet()) {
          System.out.print(key.get() + " => {");
          HashMap<Int32,Int32> m2 = mm.get(key);
          for (Int32 k2 : m2.keySet()) {
            System.out.print(k2.get() + " => " + m2.get(k2).get() + ", ");
          }
          System.out.print("}, ");
        }
        System.out.print("}\n");

        /**
         * INSANITY TEST
         */
        Insanity insane = new Insanity();
        insane.userMap.put(Numberz.FIVE, new UInt64(5000));
        Xtruct truck = new Xtruct();
        truck.string_thing.value = "Truck";
        truck.byte_thing.set((short)8);
        truck.u32_thing.set(8);
        truck.i32_thing.set(8);
        truck.u64_thing.set(8);
        truck.i64_thing.set(8);
        insane.xtructs.add(truck);
        System.out.print("testInsanity()");
        HashMap<UInt64,HashMap<Int32,Insanity>> whoa =
          testClient.testInsanity(insane);
        System.out.print(" = {");
        for (UInt64 key : whoa.keySet()) {
          HashMap<Int32,Insanity> val = whoa.get(key);
          System.out.print(key.toLong() + " => {");

          for (Int32 k2 : val.keySet()) {
            Insanity v2 = val.get(k2);
            System.out.print(k2.get() + " => {");
            HashMap<Int32, UInt64> userMap = v2.userMap;
            System.out.print("{");
            for (Int32 k3 : userMap.keySet()) {
              System.out.print(k3.get() + " => " +
                               userMap.get(k3).toLong() + ", ");
            }
            System.out.print("}, ");

            ArrayList<Xtruct> xtructs = v2.xtructs;
            System.out.print("{");
            for (Xtruct x : xtructs) {
              System.out.print("{" +
                               "\"" + x.string_thing.value + "\", " +
                               x.byte_thing.get() + ", " +
                               x.u32_thing.get() + ", "+
                               x.i32_thing.get() + ", "+
                               x.u64_thing.toLong() + ", "+
                               x.i64_thing.get() + "}, ");
            }
            System.out.print("}");

            System.out.print("}, ");
          }
          System.out.print("}, ");
        }
        System.out.print("}\n");

        long stop = System.currentTimeMillis();
        System.out.println("Total time: " + (stop-start) + "ms");

        tSocket.close();
      }
      
    } catch (Exception x) {
      x.printStackTrace();
    }
  }
}
