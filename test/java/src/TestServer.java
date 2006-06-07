package com.facebook.thrift.test;

import com.facebook.thrift.types.*;
import com.facebook.thrift.protocol.TBinaryProtocol;
import com.facebook.thrift.protocol.TProtocol;
import com.facebook.thrift.protocol.TString;
import com.facebook.thrift.server.TServer;
import com.facebook.thrift.server.TSimpleServer;
import com.facebook.thrift.transport.TServerSocket;
import com.facebook.thrift.transport.TServerTransport;

import ThriftTest.*;

import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public class TestServer extends ThriftTestServerIf {
  public TestServer(TProtocol prot) {
    super(prot);
  }
  
  public void testVoid() {
    System.out.print("testVoid()\n");
  }

  public TString testString(TString thing) {
    System.out.print("testString(\"" + thing.value + "\")\n");
    return thing;
  }

  public UInt8 testByte(UInt8 thing) {
    System.out.print("testByte(" + thing.get() + ")\n");
    return thing;
  }

  public UInt32 testU32(UInt32 thing) {
    System.out.print("testU32(" + thing.get() + ")\n");
    return thing;
  }

  public Int32 testI32(Int32 thing) {
    System.out.print("testI32(" + thing.get() + ")\n");
    return thing;
  }

  public UInt64 testU64(UInt64 thing) {
    System.out.print("testU64(" + thing.toLong() + ")\n");
    return thing;
  }

  public Int64 testI64(Int64 thing) {
    System.out.print("testI64(" + thing.get() + ")\n");
    return thing;
  }
  
  public Xtruct testStruct(Xtruct thing) {
    System.out.print("testStruct({" +
                     "\"" + thing.string_thing.value + "\", " +
                     thing.byte_thing.get() + ", " +
                     thing.u32_thing.get() + ", " +
                     thing.i32_thing.get() + ", " +
                     thing.u64_thing.toLong() + ", " +
                     thing.i64_thing.get() + "})\n");
    return thing;
  }
  
  public Xtruct2 testNest(Xtruct2 nest) {
    Xtruct thing = nest.struct_thing;
    System.out.print("testNest({" +
                     nest.byte_thing.get() + ", {" +
                     "\"" + thing.string_thing.value + "\", " +
                     thing.byte_thing.get() + ", " +
                     thing.u32_thing.get() + ", " +
                     thing.i32_thing.get() + ", " +
                     thing.u64_thing.toLong() + ", " +
                     thing.i64_thing.get() + "}, " +
                     nest.i32_thing.get() + "})\n");
    return nest;
  }
  
  public HashMap<Int32,Int32> testMap(HashMap<Int32,Int32> thing) {
    System.out.print("testMap({");
    boolean first = true;
    for (Int32 key : thing.keySet()) {
      if (first) {
        first = false;
      } else {
        System.out.print(", ");
      }
      System.out.print(key.get() + " => " + thing.get(key).get());
    }
    System.out.print("})\n");
    return thing;
  }

  public HashSet<Int32> testSet(HashSet<Int32> thing) {
    System.out.print("testSet({");
    boolean first = true;
    for (Int32 elem : thing) {
      if (first) {
        first = false;
      } else {
        System.out.print(", ");
      }
      System.out.print(elem.get());
    }
    System.out.print("})\n");
    return thing;
  }

  public ArrayList<Int32> testList(ArrayList<Int32> thing) {
    System.out.print("testList({");
    boolean first = true;
    for (Int32 elem : thing) {
      if (first) {
        first = false;
      } else {
        System.out.print(", ");
      }
      System.out.print(elem.get());
    }
    System.out.print("})\n");
    return thing;
  }

  public Int32 testEnum(Int32 thing) {
    System.out.print("testEnum(" + thing.get() + ")\n");
    return thing;
  }

  public UInt64 testTypedef(UInt64 thing) {
    System.out.print("testTypedef(" + thing.toLong() + ")\n");
    return thing;
  }

  public HashMap<Int32,HashMap<Int32,Int32>> testMapMap(Int32 hello) {
    System.out.print("testMapMap(" + hello.get() + ")\n");
    HashMap<Int32,HashMap<Int32,Int32>> mapmap =
      new HashMap<Int32,HashMap<Int32,Int32>>();

    HashMap<Int32,Int32> pos = new HashMap<Int32,Int32>();
    HashMap<Int32,Int32> neg = new HashMap<Int32,Int32>();
    for (int i = 1; i < 5; i++) {
      pos.put(new Int32(i), new Int32(i));
      neg.put(new Int32(-i), new Int32(-i));
    }

    mapmap.put(new Int32(4), pos);
    mapmap.put(new Int32(-4), neg);

    return mapmap;
  }

  public HashMap<UInt64, HashMap<Int32,Insanity>> testInsanity(Insanity argument) {
    System.out.print("testInsanity()\n");
    
    Xtruct hello = new Xtruct();
    hello.string_thing.value = "Hello2";
    hello.byte_thing.set((short)2);
    hello.u32_thing.set(2);
    hello.i32_thing.set(2);
    hello.u64_thing.set(2);
    hello.i64_thing.set(2);

    Xtruct goodbye = new Xtruct();
    goodbye.string_thing.value = "Goodbye4";
    goodbye.byte_thing.set((short)4);
    goodbye.u32_thing.set(4);
    goodbye.i32_thing.set(4);
    goodbye.u64_thing.set(4);
    goodbye.i64_thing.set(4);

    Insanity crazy = new Insanity();
    crazy.userMap.put(Numberz.EIGHT, new UInt64(8));
    crazy.xtructs.add(goodbye);

    Insanity looney = new Insanity();
    crazy.userMap.put(Numberz.FIVE, new UInt64(5));
    crazy.xtructs.add(hello);

    HashMap<Int32,Insanity> first_map = new HashMap<Int32, Insanity>();
    HashMap<Int32,Insanity> second_map = new HashMap<Int32, Insanity>();;

    first_map.put(Numberz.TWO, crazy);
    first_map.put(Numberz.THREE, crazy);

    second_map.put(Numberz.SIX, looney);

    HashMap<UInt64,HashMap<Int32,Insanity>> insane =
      new HashMap<UInt64, HashMap<Int32,Insanity>>();
    insane.put(new UInt64(1), first_map);
    insane.put(new UInt64(2), second_map);

    return insane;
  }

  public static void main(String [] args) {
    try {
      int port = 9090;
      if (args.length > 1) {
        port = Integer.valueOf(args[0]);
      }
      
      // Processor
      TBinaryProtocol binaryProtocol = new TBinaryProtocol();
      TestServer testServer = new TestServer(binaryProtocol);

      // Options
      TServer.Options serverOptions = new TServer.Options();

      // Transport
      ServerSocket serverSocket = new ServerSocket(port);
      TServerSocket tServerSocket = new TServerSocket(serverSocket);

      // Server
      TSimpleServer simpleServer = new TSimpleServer(testServer,
                                                     serverOptions,
                                                     tServerSocket);

      // Run it
      System.out.println("Starting the server on port " + port + "...");
      simpleServer.run();
    } catch (Exception x) {
      x.printStackTrace();
    }
    System.out.println("done.");
  }
}
