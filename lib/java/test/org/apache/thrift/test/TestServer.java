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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TThreadPoolServer;
import org.apache.thrift.transport.TServerSocket;

import thrift.test.Insanity;
import thrift.test.Numberz;
import thrift.test.ThriftTest;
import thrift.test.Xception;
import thrift.test.Xception2;
import thrift.test.Xtruct;
import thrift.test.Xtruct2;

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

    public Map<Integer,Integer> testMap(Map<Integer,Integer> thing) {
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

    public Set<Integer> testSet(Set<Integer> thing) {
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

    public List<Integer> testList(List<Integer> thing) {
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

    public Numberz testEnum(Numberz thing) {
      System.out.print("testEnum(" + thing + ")\n");
      return thing;
    }

    public long testTypedef(long thing) {
      System.out.print("testTypedef(" + thing + ")\n");
      return thing;
    }

    public Map<Integer,Map<Integer,Integer>> testMapMap(int hello) {
      System.out.print("testMapMap(" + hello + ")\n");
      Map<Integer,Map<Integer,Integer>> mapmap =
        new HashMap<Integer,Map<Integer,Integer>>();

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

    public Map<Long, Map<Numberz,Insanity>> testInsanity(Insanity argument) {
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
      crazy.userMap = new HashMap<Numberz, Long>();
      crazy.xtructs = new ArrayList<Xtruct>();

      crazy.userMap.put(Numberz.EIGHT, (long)8);
      crazy.xtructs.add(goodbye);

      Insanity looney = new Insanity();
      crazy.userMap.put(Numberz.FIVE, (long)5);
      crazy.xtructs.add(hello);

      HashMap<Numberz,Insanity> first_map = new HashMap<Numberz, Insanity>();
      HashMap<Numberz,Insanity> second_map = new HashMap<Numberz, Insanity>();;

      first_map.put(Numberz.TWO, crazy);
      first_map.put(Numberz.THREE, crazy);

      second_map.put(Numberz.SIX, looney);

      Map<Long,Map<Numberz,Insanity>> insane =
        new HashMap<Long, Map<Numberz,Insanity>>();
      insane.put((long)1, first_map);
      insane.put((long)2, second_map);

      return insane;
    }

    public Xtruct testMulti(byte arg0, int arg1, long arg2, Map<Short,String> arg3, Numberz arg4, long arg5) {
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

    public void testOneway(int sleepFor) {
      System.out.println("testOneway(" + Integer.toString(sleepFor) +
                         ") => sleeping...");
      try {
        Thread.sleep(sleepFor * 1000);
        System.out.println("Done sleeping!");
      } catch (InterruptedException ie) {
        throw new RuntimeException(ie);
      }
    }

  } // class TestHandler

  public static void main(String [] args) {
    try {
      int port = 9090;
      if (args.length > 1) {
        port = Integer.valueOf(args[0]);
      }

      // Processor
      TestHandler testHandler =
        new TestHandler();
      ThriftTest.Processor testProcessor =
        new ThriftTest.Processor(testHandler);

      // Transport
      TServerSocket tServerSocket =
        new TServerSocket(port);

      // Protocol factory
      TProtocolFactory tProtocolFactory =
        new TBinaryProtocol.Factory();

      TServer serverEngine;

      // Simple Server
      // serverEngine = new TSimpleServer(testProcessor, tServerSocket);

      // ThreadPool Server
      serverEngine = new TThreadPoolServer(testProcessor, tServerSocket, tProtocolFactory);

      // Run it
      System.out.println("Starting the server on port " + port + "...");
      serverEngine.serve();

    } catch (Exception x) {
      x.printStackTrace();
    }
    System.out.println("done.");
  }
}
