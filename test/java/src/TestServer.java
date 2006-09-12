package com.facebook.thrift.test;

import com.facebook.thrift.TException;
import com.facebook.thrift.protocol.TBinaryProtocol;
import com.facebook.thrift.protocol.TProtocol;
import com.facebook.thrift.server.TServer;
import com.facebook.thrift.server.TSimpleServer;
import com.facebook.thrift.server.TThreadPoolServer;
import com.facebook.thrift.transport.TServerSocket;
import com.facebook.thrift.transport.TServerTransport;

// Generated code
import thrift.test.*;

import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public class TestServer {

  public static class TestHandler implements ThriftTest.Iface {

    public TestHandler() {}
  
    public void testVoid() {
      System.out.print("testVoid()\n");
    }

    public String testString(String thing) {
      System.out.print("testString(\"" + thing + "\")\n");
      return thing;
    }

    public byte testByte(byte thing) {
      System.out.print("testByte(" + thing + ")\n");
      return thing;
    }

    public int testI32(int thing) {
      System.out.print("testI32(" + thing + ")\n");
      return thing;
    }

    public long testI64(long thing) {
      System.out.print("testI64(" + thing + ")\n");
      return thing;
    }

    public double testDouble(double thing) {
      System.out.print("testDouble(" + thing + ")\n");
      return thing;
    }
  
    public Xtruct testStruct(Xtruct thing) {
      System.out.print("testStruct({" +
                       "\"" + thing.string_thing + "\", " +
                       thing.byte_thing + ", " +
                       thing.i32_thing + ", " +
                       thing.i64_thing + "})\n");
      return thing;
    }
  
    public Xtruct2 testNest(Xtruct2 nest) {
      Xtruct thing = nest.struct_thing;
      System.out.print("testNest({" +
                       nest.byte_thing + ", {" +
                       "\"" + thing.string_thing + "\", " +
                       thing.byte_thing + ", " +
                       thing.i32_thing + ", " +
                       thing.i64_thing + "}, " +
                       nest.i32_thing + "})\n");
      return nest;
    }
  
    public HashMap<Integer,Integer> testMap(HashMap<Integer,Integer> thing) {
      System.out.print("testMap({");
      boolean first = true;
      for (int key : thing.keySet()) {
        if (first) {
          first = false;
        } else {
          System.out.print(", ");
        }
        System.out.print(key + " => " + thing.get(key));
      }
      System.out.print("})\n");
      return thing;
    }

    public HashSet<Integer> testSet(HashSet<Integer> thing) {
      System.out.print("testSet({");
      boolean first = true;
      for (int elem : thing) {
        if (first) {
          first = false;
        } else {
          System.out.print(", ");
        }
        System.out.print(elem);
      }
      System.out.print("})\n");
      return thing;
    }

    public ArrayList<Integer> testList(ArrayList<Integer> thing) {
      System.out.print("testList({");
      boolean first = true;
      for (int elem : thing) {
        if (first) {
          first = false;
        } else {
          System.out.print(", ");
        }
        System.out.print(elem);
      }
      System.out.print("})\n");
      return thing;
    }

    public int testEnum(int thing) {
      System.out.print("testEnum(" + thing + ")\n");
      return thing;
    }

    public long testTypedef(long thing) {
      System.out.print("testTypedef(" + thing + ")\n");
      return thing;
    }

    public HashMap<Integer,HashMap<Integer,Integer>> testMapMap(int hello) {
      System.out.print("testMapMap(" + hello + ")\n");
      HashMap<Integer,HashMap<Integer,Integer>> mapmap =
        new HashMap<Integer,HashMap<Integer,Integer>>();

      HashMap<Integer,Integer> pos = new HashMap<Integer,Integer>();
      HashMap<Integer,Integer> neg = new HashMap<Integer,Integer>();
      for (int i = 1; i < 5; i++) {
        pos.put(i, i);
        neg.put(-i, -i);
      }

      mapmap.put(4, pos);
      mapmap.put(-4, neg);

      return mapmap;
    }

    public HashMap<Long, HashMap<Integer,Insanity>> testInsanity(Insanity argument) {
      System.out.print("testInsanity()\n");
    
      Xtruct hello = new Xtruct();
      hello.string_thing = "Hello2";
      hello.byte_thing = 2;
      hello.i32_thing = 2;
      hello.i64_thing = 2;

      Xtruct goodbye = new Xtruct();
      goodbye.string_thing = "Goodbye4";
      goodbye.byte_thing = (byte)4;
      goodbye.i32_thing = 4;
      goodbye.i64_thing = (long)4;

      Insanity crazy = new Insanity();
      crazy.userMap.put(Numberz.EIGHT, (long)8);
      crazy.xtructs.add(goodbye);

      Insanity looney = new Insanity();
      crazy.userMap.put(Numberz.FIVE, (long)5);
      crazy.xtructs.add(hello);

      HashMap<Integer,Insanity> first_map = new HashMap<Integer, Insanity>();
      HashMap<Integer,Insanity> second_map = new HashMap<Integer, Insanity>();;

      first_map.put(Numberz.TWO, crazy);
      first_map.put(Numberz.THREE, crazy);

      second_map.put(Numberz.SIX, looney);

      HashMap<Long,HashMap<Integer,Insanity>> insane =
        new HashMap<Long, HashMap<Integer,Insanity>>();
      insane.put((long)1, first_map);
      insane.put((long)2, second_map);

      return insane;
    }

    public Xtruct testMulti(byte arg0, int arg1, long arg2, HashMap<Short,String> arg3, int arg4, long arg5) {
      System.out.print("testMulti()\n");
    
      Xtruct hello = new Xtruct();;
      hello.string_thing = "Hello2";
      hello.byte_thing = arg0;
      hello.i32_thing = arg1;
      hello.i64_thing = arg2;
      return hello;
    }

    public void testException(String arg) throws Xception {
      System.out.print("testException("+arg+")\n");
      if (arg.equals("Xception")) {
        Xception x = new Xception();
        x.errorCode = 1001;
        x.message = "This is an Xception";
        throw x;
      }
      return;
    }

    public Xtruct testMultiException(String arg0, String arg1) throws Xception, Xception2 {
      System.out.print("testMultiException(" + arg0 + ", " + arg1 + ")\n");
      if (arg0.equals("Xception")) {
        Xception x = new Xception();
        x.errorCode = 1001;
        x.message = "This is an Xception";
        throw x;
      } else if (arg0.equals("Xception2")) {
        Xception2 x = new Xception2();
        x.errorCode = 2002;
        x.struct_thing = new Xtruct();
        x.struct_thing.string_thing = "This is an Xception2";
        throw x;
      }
     
      Xtruct result = new Xtruct();
      result.string_thing = arg1;
      return result;
    }

  } // class TestHandler

  public static void main(String [] args) {
    try {
      int port = 9090;
      if (args.length > 1) {
        port = Integer.valueOf(args[0]);
      }
      
      // Processor
      TBinaryProtocol binaryProtocol =
        new TBinaryProtocol();
      TestHandler testHandler =
        new TestHandler();
      ThriftTest.Processor testProcessor =
        new ThriftTest.Processor(testHandler, binaryProtocol);

      // Transport
      TServerSocket tServerSocket =
        new TServerSocket(port);

      TServer serverEngine;

      // Simple Server
      // serverEngine = new TSimpleServer(testProcessor, tServerSocket);

      // ThreadPool Server
      serverEngine =  new TThreadPoolServer(testProcessor, tServerSocket);

      // Run it
      System.out.println("Starting the server on port " + port + "...");
      serverEngine.serve();

    } catch (Exception x) {
      x.printStackTrace();
    }
    System.out.println("done.");
  }
}
