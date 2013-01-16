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

// Generated code
import thrift.test.*;

import org.apache.thrift.TApplicationException;
import org.apache.thrift.TException;
import org.apache.thrift.TSerializer;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.THttpClient;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TFastFramedTransport;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TSimpleJSONProtocol;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;

/**
 * Test Java client for thrift. Essentially just a copy of the C++ version,
 * this makes a variety of requests to enable testing for both performance and
 * correctness of the output.
 *
 */
public class TestClient {
  public static void main(String [] args) {
    String host = "localhost";
    int port = 9090;
    int numTests = 1;
    String protocol_type = "binary";
    String transport_type = "buffered";
    boolean ssl = false;

    int socketTimeout = 1000;

    try {
      for (int i = 0; i < args.length; ++i) {
        if (args[i].startsWith("--host")) {
          host = args[i].split("=")[1];
          host.trim();
        } else if (args[i].startsWith("--port")) {
          port = Integer.valueOf(args[i].split("=")[1]); 
        } else if (args[i].startsWith("--n") || 
            args[i].startsWith("--testloops")){
          numTests = Integer.valueOf(args[i].split("=")[1]);
        } else if (args[i].equals("--timeout")) {
          socketTimeout = Integer.valueOf(args[i].split("=")[1]);
        } else if (args[i].startsWith("--protocol")) {
          protocol_type = args[i].split("=")[1];
          protocol_type.trim();
        } else if (args[i].startsWith("--transport")) {
          transport_type = args[i].split("=")[1];
          transport_type.trim();
        } else if (args[i].equals("--ssl")) {
          ssl = true;
        } else if (args[i].equals("--help")) {
          System.out.println("Allowed options:");
          System.out.println("  --help\t\t\tProduce help message"); 
          System.out.println("  --host=arg (=" + host + ")\tHost to connect");
          System.out.println("  --port=arg (=" + port + ")\tPort number to connect");
          System.out.println("  --transport=arg (=" + transport_type + ")\n\t\t\t\tTransport: buffered, framed, fastframed, http");
          System.out.println("  --protocol=arg (=" + protocol_type + ")\tProtocol: binary, json, compact");
          System.out.println("  --ssl\t\t\tEncrypted Transport using SSL");
          System.out.println("  --testloops[--n]=arg (=" + numTests + ")\tNumber of Tests");
          System.exit(0);
        }
      }
    } catch (Exception x) {
      System.err.println("Can not parse arguments! See --help");
      System.exit(1);
    }

    try {
      if (protocol_type.equals("binary")) {
      } else if (protocol_type.equals("compact")) {
      } else if (protocol_type.equals("json")) {
      } else {
        throw new Exception("Unknown protocol type! " + protocol_type); 
      }
      if (transport_type.equals("buffered")) {
      } else if (transport_type.equals("framed")) {
      } else if (transport_type.equals("fastframed")) {
      } else if (transport_type.equals("http")) {
      } else {
        throw new Exception("Unknown transport type! " + transport_type);
      }
      if (transport_type.equals("http") && ssl == true) {
        throw new Exception("SSL is not supported over http.");
      }
    } catch (Exception e) {
      System.err.println("Error: " + e.getMessage());
      System.exit(1);
    }

    TTransport transport = null;

    try {
      if (transport_type.equals("http")) {
        String url = "http://" + host + ":" + port + "/service";
        transport = new THttpClient(url);
      } else {
        TSocket socket = null;
        if (ssl == true) {
          socket = TSSLTransportFactory.getClientSocket(host, port, 0);
        } else {
          socket = new TSocket(host, port);
        }
        socket.setTimeout(socketTimeout);
        transport = socket;
        if (transport_type.equals("buffered")) {
        } else if (transport_type.equals("framed")) {
          transport = new TFramedTransport(transport);
        } else if (transport_type.equals("fastframed")) {
          transport = new TFastFramedTransport(transport);
        }
      }
    } catch (Exception x) {
      x.printStackTrace();
      System.exit(1);
    }

    TProtocol tProtocol = null;
    if (protocol_type.equals("json")) {
      tProtocol = new TJSONProtocol(transport);
    } else if (protocol_type.equals("compact")) {
      tProtocol = new TCompactProtocol(transport);
    } else {
      tProtocol = new TBinaryProtocol(transport);
    }

    ThriftTest.Client testClient =
      new ThriftTest.Client(tProtocol);
    Insanity insane = new Insanity();

    long timeMin = 0;
    long timeMax = 0;
    long timeTot = 0;

    int failCount = 0;
    for (int test = 0; test < numTests; ++test) {
      try {
        /**
         * CONNECT TEST
         */
        System.out.println("Test #" + (test+1) + ", " + "connect " + host + ":" + port);

        if (transport.isOpen() == false) {
          try {
            transport.open();
          } catch (TTransportException ttx) {
            ttx.printStackTrace();
            System.out.println("Connect failed: " + ttx.getMessage());
            System.exit(1);
          }
        }

        long start = System.nanoTime();

        /**
         * VOID TEST
         */
        try {
          System.out.print("testVoid()");
          testClient.testVoid();
          System.out.print(" = void\n");
        } catch (TApplicationException tax) {
          tax.printStackTrace();
          failCount++;
        }

        /**
         * STRING TEST
         */
        System.out.print("testString(\"Test\")");
        String s = testClient.testString("Test");
        System.out.print(" = \"" + s + "\"\n");
        if (!s.equals("Test")) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        /**
         * BYTE TEST
         */
        System.out.print("testByte(1)");
        byte i8 = testClient.testByte((byte)1);
        System.out.print(" = " + i8 + "\n");
        if (i8 != 1) {
          failCount++; 
          System.out.println("FAILURE\n");
        }

        /**
         * I32 TEST
         */
        System.out.print("testI32(-1)");
        int i32 = testClient.testI32(-1);
        System.out.print(" = " + i32 + "\n");
        if (i32 != -1) {
          failCount++; 
          System.out.println("FAILURE\n");
        }

        /**
         * I64 TEST
         */
        System.out.print("testI64(-34359738368)");
        long i64 = testClient.testI64(-34359738368L);
        System.out.print(" = " + i64 + "\n");
        if (i64 != -34359738368L) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        /**
         * DOUBLE TEST
         */
        System.out.print("testDouble(-5.325098235)");
        double dub = testClient.testDouble(-5.325098235);
        System.out.print(" = " + dub + "\n");
        if (Math.abs(dub - (-5.325098235)) > 0.001) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        /**
         * STRUCT TEST
         */
        System.out.print("testStruct({\"Zero\", 1, -3, -5})");
        Xtruct out = new Xtruct();
        out.string_thing = "Zero";
        out.byte_thing = (byte) 1;
        out.i32_thing = -3;
        out.i64_thing = -5;
        Xtruct in = testClient.testStruct(out);
        System.out.print(" = {" + "\"" + 
                         in.string_thing + "\"," + 
                         in.byte_thing + ", " + 
                         in.i32_thing + ", " + 
                         in.i64_thing + "}\n");
        if (!in.equals(out)) {
          failCount++; 
          System.out.println("FAILURE\n");
        }

        /**
         * NESTED STRUCT TEST
         */
        System.out.print("testNest({1, {\"Zero\", 1, -3, -5}), 5}");
        Xtruct2 out2 = new Xtruct2();
        out2.byte_thing = (short)1;
        out2.struct_thing = out;
        out2.i32_thing = 5;
        Xtruct2 in2 = testClient.testNest(out2);
        in = in2.struct_thing;
        System.out.print(" = {" + in2.byte_thing + ", {" + "\"" + 
                         in.string_thing + "\", " + 
                         in.byte_thing + ", " +
                         in.i32_thing + ", " +
                         in.i64_thing + "}, " +
                         in2.i32_thing + "}\n");
        if (!in2.equals(out2)) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        /**
         * MAP TEST
         */
        Map<Integer,Integer> mapout = new HashMap<Integer,Integer>();
        for (int i = 0; i < 5; ++i) {
          mapout.put(i, i-10);
        }
        System.out.print("testMap({");
        boolean first = true;
        for (int key : mapout.keySet()) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(key + " => " + mapout.get(key));
        }
        System.out.print("})");
        Map<Integer,Integer> mapin = testClient.testMap(mapout);
        System.out.print(" = {");
        first = true;
        for (int key : mapin.keySet()) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(key + " => " + mapout.get(key));
        }
        System.out.print("}\n");
        if (!mapout.equals(mapin)) {
          failCount++; 
          System.out.println("FAILURE\n");
        }

        /**
         * STRING MAP TEST
         *  missing
         */

        /**
         * SET TEST
         */
        Set<Integer> setout = new HashSet<Integer>();
        for (int i = -2; i < 3; ++i) {
          setout.add(i);
        }
        System.out.print("testSet({");
        first = true;
        for (int elem : setout) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem);
        }
        System.out.print("})");
        Set<Integer> setin = testClient.testSet(setout);
        System.out.print(" = {");
        first = true;
        for (int elem : setin) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem);
        }
        System.out.print("}\n");
        if (!setout.equals(setin)) {
          failCount++; 
          System.out.println("FAILURE\n");
        }

        /**
         * LIST TEST
         */
        List<Integer> listout = new ArrayList<Integer>();
        for (int i = -2; i < 3; ++i) {
          listout.add(i);
        }
        System.out.print("testList({");
        first = true;
        for (int elem : listout) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem);
        }
        System.out.print("})");
        List<Integer> listin = testClient.testList(listout);
        System.out.print(" = {");
        first = true;
        for (int elem : listin) {
          if (first) {
            first = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem);
        }
        System.out.print("}\n");
        if (!listout.equals(listin)) {
          failCount++; 
          System.out.println("FAILURE\n");
        }

        /**
         * ENUM TEST
         */
        System.out.print("testEnum(ONE)");
        Numberz ret = testClient.testEnum(Numberz.ONE);
        System.out.print(" = " + ret + "\n");
        if (ret != Numberz.ONE) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        System.out.print("testEnum(TWO)");
        ret = testClient.testEnum(Numberz.TWO);
        System.out.print(" = " + ret + "\n");
        if (ret != Numberz.TWO) {
          failCount++; 
          System.out.println("FAILURE\n");
        }

        System.out.print("testEnum(THREE)");
        ret = testClient.testEnum(Numberz.THREE);
        System.out.print(" = " + ret + "\n");
        if (ret != Numberz.THREE) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        System.out.print("testEnum(FIVE)");
        ret = testClient.testEnum(Numberz.FIVE);
        System.out.print(" = " + ret + "\n");
        if (ret != Numberz.FIVE) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        System.out.print("testEnum(EIGHT)");
        ret = testClient.testEnum(Numberz.EIGHT);
        System.out.print(" = " + ret + "\n");
        if (ret != Numberz.EIGHT) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        /**
         * TYPEDEF TEST
         */
        System.out.print("testTypedef(309858235082523)");
        long uid = testClient.testTypedef(309858235082523L);
        System.out.print(" = " + uid + "\n");
        if (uid != 309858235082523L) {
          failCount++;
          System.out.println("FAILURE\n");
        }

        /**
         * NESTED MAP TEST
         */
        System.out.print("testMapMap(1)");
        Map<Integer,Map<Integer,Integer>> mm =
          testClient.testMapMap(1);
        System.out.print(" = {");
        for (int key : mm.keySet()) {
          System.out.print(key + " => {");
          Map<Integer,Integer> m2 = mm.get(key);
          for (int k2 : m2.keySet()) {
            System.out.print(k2 + " => " + m2.get(k2) + ", ");
          }
          System.out.print("}, ");
        }
        System.out.print("}\n");

        /**
         * INSANITY TEST
         */
        insane = new Insanity();
        insane.userMap = new HashMap<Numberz, Long>();
        insane.userMap.put(Numberz.FIVE, (long)5000);
        Xtruct truck = new Xtruct();
        truck.string_thing = "Truck";
        truck.byte_thing = (byte)8;
        truck.i32_thing = 8;
        truck.i64_thing = 8;
        insane.xtructs = new ArrayList<Xtruct>();
        insane.xtructs.add(truck);
        System.out.print("testInsanity()");
        Map<Long,Map<Numberz,Insanity>> whoa =
          testClient.testInsanity(insane);
        System.out.print(" = {");
        for (long key : whoa.keySet()) {
          Map<Numberz,Insanity> val = whoa.get(key);
          System.out.print(key + " => {");

          for (Numberz k2 : val.keySet()) {
            Insanity v2 = val.get(k2);
            System.out.print(k2 + " => {");
            Map<Numberz, Long> userMap = v2.userMap;
            System.out.print("{");
            if (userMap != null) {
              for (Numberz k3 : userMap.keySet()) {
                System.out.print(k3 + " => " + userMap.get(k3) + ", ");
              }
            }
            System.out.print("}, ");

            List<Xtruct> xtructs = v2.xtructs;
            System.out.print("{");
            if (xtructs != null) {
              for (Xtruct x : xtructs) {
                System.out.print("{" + "\"" + x.string_thing + "\", " + x.byte_thing + ", " + x.i32_thing + ", "+ x.i64_thing + "}, ");
              }
            }
            System.out.print("}");

            System.out.print("}, ");
          }
          System.out.print("}, ");
        }
        System.out.print("}\n");

        
        /**
         * EXECPTION TEST
         */
        try {
          System.out.print("testClient.testException(\"Xception\") =>");
          testClient.testException("Xception");
          System.out.print("  void\nFAILURE\n");
          failCount++;
        } catch(Xception e) {
          System.out.printf("  {%d, \"%s\"}\n", e.errorCode, e.message);
        }
        
        try {
          System.out.print("testClient.testException(\"TException\") =>");
          testClient.testException("TException");
          System.out.print("  void\nFAILURE\n");
          failCount++;
        } catch(TException e) {
          System.out.printf("  {\"%s\"}\n", e.getMessage());
        }
        
        try {
          System.out.print("testClient.testException(\"success\") =>");
          testClient.testException("success");
          System.out.print("  void\n");
        }catch(Exception e) {
          System.out.printf("  exception\nFAILURE\n");
          failCount++;
        }
        
        
        /**
         * MULTI EXCEPTION TEST
         */
        
        try {
          System.out.printf("testClient.testMultiException(\"Xception\", \"test 1\") =>");
          testClient.testMultiException("Xception", "test 1");
          System.out.print("  result\nFAILURE\n");
          failCount++;
        } catch(Xception e) {
          System.out.printf("  {%d, \"%s\"}\n", e.errorCode, e.message);
        }
        
        try {
          System.out.printf("testClient.testMultiException(\"Xception2\", \"test 2\") =>");
          testClient.testMultiException("Xception2", "test 2");
          System.out.print("  result\nFAILURE\n");
          failCount++;
        } catch(Xception2 e) {
          System.out.printf("  {%d, {\"%s\"}}\n", e.errorCode, e.struct_thing.string_thing);
        }
        
        try {
          System.out.print("testClient.testMultiException(\"success\", \"test 3\") =>");
          Xtruct result;
          result = testClient.testMultiException("success", "test 3");
          System.out.printf("  {{\"%s\"}}\n", result.string_thing);
        } catch(Exception e) {
          System.out.printf("  exception\nFAILURE\n");
          failCount++;
        }


        
        /**
         * ONEWAY TEST
         */
        System.out.print("testOneway(3)...");
        long startOneway = System.nanoTime();
        testClient.testOneway(3);
        long onewayElapsedMillis = (System.nanoTime() - startOneway) / 1000000;
        if (onewayElapsedMillis > 200) {
          System.out.println("Oneway test failed: took " +
                              Long.toString(onewayElapsedMillis) +
                              "ms");
          failCount++;
        } else {
          System.out.println("Success - took " +
                             Long.toString(onewayElapsedMillis) +
                             "ms");
        }


        long stop = System.nanoTime();
        long tot = stop-start;

        System.out.println("Total time: " + tot/1000 + "us");

        if (timeMin == 0 || tot < timeMin) {
          timeMin = tot;
        }
        if (tot > timeMax) {
          timeMax = tot;
        }
        timeTot += tot;

        transport.close();
      } catch (Exception x) {
        x.printStackTrace();
        failCount++;
      }
    }

    long timeAvg = timeTot / numTests;

    System.out.println("Min time: " + timeMin/1000 + "us");
    System.out.println("Max time: " + timeMax/1000 + "us");
    System.out.println("Avg time: " + timeAvg/1000 + "us");

    try {
      String json = (new TSerializer(new TSimpleJSONProtocol.Factory())).toString(insane);
      System.out.println("\nFor good meausre here is some JSON:\n" + json);
    } catch (TException x) {
      x.printStackTrace();
      System.exit(1);
    }


    System.exit(failCount);
  }
}
