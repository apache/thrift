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

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import org.apache.thrift.TException;
import org.apache.thrift.TSerializer;
import org.apache.thrift.async.AsyncMethodCallback;
import org.apache.thrift.async.TAsyncClientManager;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.protocol.TSimpleJSONProtocol;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TSSLTransportFactory;
import thrift.test.Insanity;
import thrift.test.Numberz;
import thrift.test.SecondService;
import thrift.test.ThriftTest;
import thrift.test.Xception;
import thrift.test.Xception2;
import thrift.test.Xtruct;
import thrift.test.Xtruct2;

public class TestNonblockingClient extends TestClient {
  private static String host = "localhost";
  private static int port = 9090;
  private static int socketTimeout = 1000;
  private static String protocol_type = "binary";
  private static boolean ssl = false;

  private static CountDownLatch latch;
  private static int returnCode = 0;
  private static ThriftTest.AsyncClient testClient;
  private static TAsyncClientManager clientManager;

  public static void main(String[] args) {
    int numTests = 1;
    try {
      for (int i = 0; i < args.length; ++i) {
        if (args[i].startsWith("--host")) {
          host = args[i].split("=")[1];
          host.trim();
        } else if (args[i].startsWith("--port")) {
          port = Integer.valueOf(args[i].split("=")[1]);
        } else if (args[i].startsWith("--n") || args[i].startsWith("--testloops")) {
          numTests = Integer.valueOf(args[i].split("=")[1]);
        } else if (args[i].equals("--timeout")) {
          socketTimeout = Integer.valueOf(args[i].split("=")[1]);
        } else if (args[i].startsWith("--protocol")) {
          protocol_type = args[i].split("=")[1];
          protocol_type.trim();
        } else if (args[i].equals("--ssl")) {
          ssl = true;
        } else if (args[i].equals("--help")) {
          System.out.println("Allowed options:");
          System.out.println("  --help\t\t\tProduce help message");
          System.out.println("  --host=arg (=" + host + ")\tHost to connect");
          System.out.println("  --port=arg (=" + port + ")\tPort number to connect");
          System.out.println(
              "  --protocol=arg (=" + protocol_type + ")\tProtocol: binary, compact, json");
          System.out.println("  --ssl\t\t\tEncrypted Transport using SSL");
          System.out.println("  --testloops[--n]=arg (=" + numTests + ")\tNumber of Tests");
          System.exit(0);
        }
      }
    } catch (Exception x) {
      System.err.println("Can not parse arguments! See --help");
      System.exit(ERR_UNKNOWN);
    }

    try {
      if (protocol_type.equals("binary")) {
      } else if (protocol_type.equals("compact")) {
      } else if (protocol_type.equals("json")) {
      } else if (protocol_type.equals("multi")) {
      } else if (protocol_type.equals("multic")) {
      } else if (protocol_type.equals("multij")) {
      } else {
        throw new Exception("Unknown protocol type! " + protocol_type);
      }
    } catch (Exception e) {
      System.err.println("Error: " + e.getMessage());
      System.exit(ERR_UNKNOWN);
    }

    try {
      clientManager = new TAsyncClientManager();
    } catch (Exception e) {
      System.err.println("Error: " + e.getMessage());
      System.exit(ERR_UNKNOWN);
    }

    testClient = getClient();

    Insanity insane = new Insanity();

    long timeMin = 0;
    long timeMax = 0;
    long timeTot = 0;

    for (int test = 0; test < numTests; ++test) {
      try {

        long start = System.nanoTime();

        /** VOID TEST */
        System.out.print("testVoid()");
        latch = new CountDownLatch(1);
        testClient.testVoid(
            new AsyncMethodCallback<Void>() {
              @Override
              public void onComplete(Void response) {
                System.out.print(" = void\n");
                latch.countDown();
              }

              @Override
              public void onError(Exception exception) {
                exception.printStackTrace();
                returnCode |= ERR_BASETYPES;
                latch.countDown();
              }
            });
        latch.await();

        /** STRING TEST */
        System.out.print("testString(\"Test\")");
        latch = new CountDownLatch(1);
        testClient.testString(
            "Test",
            new FailureLessCallback<String>() {
              @Override
              public void onComplete(String s) {
                System.out.print(" = \"" + s + "\"\n");
                if (!s.equals("Test")) {
                  returnCode |= ERR_BASETYPES;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** UUID TEST */
        System.out.println("testUuid(\"00112233-4455-6677-8899-aabbccddeeff\")");
        latch = new CountDownLatch(1);
        testClient.testUuid(
            UUID.fromString("00112233-4455-6677-8899-aabbccddeeff"),
            new FailureLessCallback<UUID>() {

              @Override
              public void onComplete(UUID uuid) {
                System.out.print(" = \"" + uuid + "\"\n");
                if (!uuid.equals(UUID.fromString("00112233-4455-6677-8899-aabbccddeeff"))) {
                  returnCode |= ERR_BASETYPES;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** Multiplexed test */
        if (protocol_type.startsWith("multi")) {
          latch = new CountDownLatch(1);
          SecondService.AsyncClient secondClient = getSecondServiceClient();
          System.out.print("secondtestString(\"Test2\")");
          secondClient.secondtestString(
              "Test2",
              new FailureLessCallback<String>() {
                @Override
                public void onComplete(String s) {
                  System.out.print(" = \"" + s + "\"\n");
                  if (!s.equals("testString(\"Test2\")")) {
                    returnCode |= ERR_PROTOCOLS;
                    System.out.println("*** FAILURE ***\n");
                  }
                  latch.countDown();
                }
              });
          latch.await();
        }

        /** BYTE TEST */
        System.out.print("testByte(1)");
        latch = new CountDownLatch(1);
        testClient.testByte(
            (byte) 1,
            new FailureLessCallback<Byte>() {
              @Override
              public void onComplete(Byte i8) {
                System.out.print(" = " + i8 + "\n");
                if (i8 != 1) {
                  returnCode |= ERR_BASETYPES;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** I32 TEST */
        System.out.print("testI32(-1)");
        latch = new CountDownLatch(1);
        testClient.testI32(
            -1,
            new FailureLessCallback<Integer>() {
              @Override
              public void onComplete(Integer i32) {
                System.out.print(" = " + i32 + "\n");
                if (i32 != -1) {
                  returnCode |= ERR_BASETYPES;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** I64 TEST */
        System.out.print("testI64(-34359738368)");
        latch = new CountDownLatch(1);
        testClient.testI64(
            -34359738368L,
            new FailureLessCallback<Long>() {
              @Override
              public void onComplete(Long i64) {
                System.out.print(" = " + i64 + "\n");
                if (i64 != -34359738368L) {
                  returnCode |= ERR_BASETYPES;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** DOUBLE TEST */
        System.out.print("testDouble(-5.325098235)");
        latch = new CountDownLatch(1);
        testClient.testDouble(
            -5.325098235,
            new FailureLessCallback<Double>() {
              @Override
              public void onComplete(Double dub) {
                System.out.print(" = " + dub + "\n");
                if (Math.abs(dub - (-5.325098235)) > 0.001) {
                  returnCode |= ERR_BASETYPES;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** BINARY TEST */
        System.out.print("testBinary(-128...127) = ");
        byte[] data = getBytesData();
        latch = new CountDownLatch(1);
        testClient.testBinary(
            ByteBuffer.wrap(data),
            new AsyncMethodCallback<ByteBuffer>() {
              @Override
              public void onComplete(ByteBuffer bin) {
                bin.mark();
                byte[] bytes = new byte[bin.limit() - bin.position()];
                bin.get(bytes);
                bin.reset();
                System.out.print("{");
                boolean first = true;
                for (int i = 0; i < bytes.length; ++i) {
                  if (first) first = false;
                  else System.out.print(", ");
                  System.out.print(bytes[i]);
                }
                System.out.println("}");
                if (!ByteBuffer.wrap(data).equals(bin)) {
                  returnCode |= ERR_BASETYPES;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }

              @Override
              public void onError(Exception ex) {
                returnCode |= ERR_BASETYPES;
                System.out.println("\n*** FAILURE ***\n");
                ex.printStackTrace(System.out);
                latch.countDown();
              }
            });
        latch.await();

        /** STRUCT TEST */
        System.out.print("testStruct({\"Zero\", 1, -3, -5})");
        Xtruct out = new Xtruct();
        out.string_thing = "Zero";
        out.byte_thing = (byte) 1;
        out.i32_thing = -3;
        out.i64_thing = -5;
        final Xtruct[] in = {null};
        latch = new CountDownLatch(1);
        testClient.testStruct(
            out,
            new FailureLessCallback<Xtruct>() {
              @Override
              public void onComplete(Xtruct response) {
                in[0] = response;
                System.out.print(
                    " = {"
                        + "\""
                        + in[0].string_thing
                        + "\","
                        + in[0].byte_thing
                        + ", "
                        + in[0].i32_thing
                        + ", "
                        + in[0].i64_thing
                        + "}\n");
                if (!in[0].equals(out)) {
                  returnCode |= ERR_STRUCTS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** NESTED STRUCT TEST */
        System.out.print("testNest({1, {\"Zero\", 1, -3, -5}), 5}");
        Xtruct2 out2 = new Xtruct2();
        out2.byte_thing = (short) 1;
        out2.struct_thing = out;
        out2.i32_thing = 5;
        latch = new CountDownLatch(1);
        testClient.testNest(
            out2,
            new FailureLessCallback<Xtruct2>() {

              @Override
              public void onComplete(Xtruct2 in2) {
                in[0] = in2.struct_thing;
                System.out.print(
                    " = {"
                        + in2.byte_thing
                        + ", {"
                        + "\""
                        + in[0].string_thing
                        + "\", "
                        + in[0].byte_thing
                        + ", "
                        + in[0].i32_thing
                        + ", "
                        + in[0].i64_thing
                        + "}, "
                        + in2.i32_thing
                        + "}\n");
                if (!in2.equals(out2)) {
                  returnCode |= ERR_STRUCTS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** MAP TEST */
        Map<Integer, Integer> mapout = new HashMap<Integer, Integer>();
        for (int i = 0; i < 5; ++i) {
          mapout.put(i, i - 10);
        }
        System.out.print("testMap({");
        final boolean[] first = {true};
        for (int key : mapout.keySet()) {
          if (first[0]) {
            first[0] = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(key + " => " + mapout.get(key));
        }
        System.out.print("})");
        latch = new CountDownLatch(1);
        testClient.testMap(
            mapout,
            new FailureLessCallback<Map<Integer, Integer>>() {
              @Override
              public void onComplete(Map<Integer, Integer> mapin) {
                System.out.print(" = {");
                first[0] = true;
                for (int key : mapin.keySet()) {
                  if (first[0]) {
                    first[0] = false;
                  } else {
                    System.out.print(", ");
                  }
                  System.out.print(key + " => " + mapout.get(key));
                }
                System.out.print("}\n");
                if (!mapout.equals(mapin)) {
                  returnCode |= ERR_CONTAINERS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** STRING MAP TEST */
        Map<String, String> smapout = new HashMap<String, String>();
        smapout.put("a", "2");
        smapout.put("b", "blah");
        smapout.put("some", "thing");
        for (String key : smapout.keySet()) {
          if (first[0]) {
            first[0] = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(key + " => " + smapout.get(key));
        }
        System.out.print("})");
        latch = new CountDownLatch(1);
        testClient.testStringMap(
            smapout,
            new AsyncMethodCallback<Map<String, String>>() {
              @Override
              public void onComplete(Map<String, String> smapin) {
                System.out.print(" = {");
                first[0] = true;
                for (String key : smapin.keySet()) {
                  if (first[0]) {
                    first[0] = false;
                  } else {
                    System.out.print(", ");
                  }
                  System.out.print(key + " => " + smapout.get(key));
                }
                System.out.print("}\n");
                if (!smapout.equals(smapin)) {
                  returnCode |= ERR_CONTAINERS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }

              @Override
              public void onError(Exception ex) {
                returnCode |= ERR_CONTAINERS;
                System.out.println("*** FAILURE ***\n");
                ex.printStackTrace(System.out);
                latch.countDown();
              }
            });
        latch.await();

        /** SET TEST */
        Set<Integer> setout = new HashSet<Integer>();
        for (int i = -2; i < 3; ++i) {
          setout.add(i);
        }
        System.out.print("testSet({");
        first[0] = true;
        for (int elem : setout) {
          if (first[0]) {
            first[0] = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem);
        }
        System.out.print("})");
        latch = new CountDownLatch(1);
        testClient.testSet(
            setout,
            new FailureLessCallback<Set<Integer>>() {
              @Override
              public void onComplete(Set<Integer> setin) {
                System.out.print(" = {");
                first[0] = true;
                for (int elem : setin) {
                  if (first[0]) {
                    first[0] = false;
                  } else {
                    System.out.print(", ");
                  }
                  System.out.print(elem);
                }
                System.out.print("}\n");
                if (!setout.equals(setin)) {
                  returnCode |= ERR_CONTAINERS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** LIST TEST */
        List<Integer> listout = new ArrayList<Integer>();
        for (int i = -2; i < 3; ++i) {
          listout.add(i);
        }
        System.out.print("testList({");
        first[0] = true;
        for (int elem : listout) {
          if (first[0]) {
            first[0] = false;
          } else {
            System.out.print(", ");
          }
          System.out.print(elem);
        }
        System.out.print("})");
        latch = new CountDownLatch(1);
        testClient.testList(
            listout,
            new FailureLessCallback<List<Integer>>() {
              @Override
              public void onComplete(List<Integer> listin) {
                System.out.print(" = {");
                first[0] = true;
                for (int elem : listin) {
                  if (first[0]) {
                    first[0] = false;
                  } else {
                    System.out.print(", ");
                  }
                  System.out.print(elem);
                }
                System.out.print("}\n");
                if (!listout.equals(listin)) {
                  returnCode |= ERR_CONTAINERS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** ENUM TEST */
        System.out.print("testEnum(ONE)");
        latch = new CountDownLatch(1);
        testClient.testEnum(
            Numberz.ONE,
            new FailureLessCallback<Numberz>() {
              @Override
              public void onComplete(Numberz ret) {
                System.out.print(" = " + ret + "\n");
                if (ret != Numberz.ONE) {
                  returnCode |= ERR_STRUCTS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        System.out.print("testEnum(TWO)");
        latch = new CountDownLatch(1);
        testClient.testEnum(
            Numberz.TWO,
            new FailureLessCallback<Numberz>() {
              @Override
              public void onComplete(Numberz ret) {
                System.out.print(" = " + ret + "\n");
                if (ret != Numberz.TWO) {
                  returnCode |= ERR_STRUCTS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        System.out.print("testEnum(THREE)");
        latch = new CountDownLatch(1);
        testClient.testEnum(
            Numberz.THREE,
            new FailureLessCallback<Numberz>() {
              @Override
              public void onComplete(Numberz ret) {
                System.out.print(" = " + ret + "\n");
                if (ret != Numberz.THREE) {
                  returnCode |= ERR_STRUCTS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        System.out.print("testEnum(FIVE)");
        latch = new CountDownLatch(1);
        testClient.testEnum(
            Numberz.FIVE,
            new FailureLessCallback<Numberz>() {
              @Override
              public void onComplete(Numberz ret) {
                System.out.print(" = " + ret + "\n");
                if (ret != Numberz.FIVE) {
                  returnCode |= ERR_STRUCTS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        System.out.print("testEnum(EIGHT)");
        latch = new CountDownLatch(1);
        testClient.testEnum(
            Numberz.EIGHT,
            new FailureLessCallback<Numberz>() {
              @Override
              public void onComplete(Numberz ret) {
                System.out.print(" = " + ret + "\n");
                if (ret != Numberz.EIGHT) {
                  returnCode |= ERR_STRUCTS;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** TYPEDEF TEST */
        System.out.print("testTypedef(309858235082523)");
        latch = new CountDownLatch(1);
        testClient.testTypedef(
            309858235082523L,
            new FailureLessCallback<Long>() {
              @Override
              public void onComplete(Long uid) {
                System.out.print(" = " + uid + "\n");
                if (uid != 309858235082523L) {
                  returnCode |= ERR_BASETYPES;
                  System.out.println("*** FAILURE ***\n");
                }
                latch.countDown();
              }
            });
        latch.await();

        /** NESTED MAP TEST */
        System.out.print("testMapMap(1)");
        latch = new CountDownLatch(1);
        testClient.testMapMap(
            1,
            new FailureLessCallback<Map<Integer, Map<Integer, Integer>>>() {
              @Override
              public void onComplete(Map<Integer, Map<Integer, Integer>> mm) {
                System.out.print(" = {");
                for (int key : mm.keySet()) {
                  System.out.print(key + " => {");
                  Map<Integer, Integer> m2 = mm.get(key);
                  for (int k2 : m2.keySet()) {
                    System.out.print(k2 + " => " + m2.get(k2) + ", ");
                  }
                  System.out.print("}, ");
                }
                System.out.print("}\n");
                if (mm.size() != 2 || !mm.containsKey(4) || !mm.containsKey(-4)) {
                  returnCode |= ERR_CONTAINERS;
                  System.out.println("*** FAILURE ***\n");
                } else {
                  Map<Integer, Integer> m1 = mm.get(4);
                  Map<Integer, Integer> m2 = mm.get(-4);
                  if (m1.get(1) != 1
                      || m1.get(2) != 2
                      || m1.get(3) != 3
                      || m1.get(4) != 4
                      || m2.get(-1) != -1
                      || m2.get(-2) != -2
                      || m2.get(-3) != -3
                      || m2.get(-4) != -4) {
                    returnCode |= ERR_CONTAINERS;
                    System.out.println("*** FAILURE ***\n");
                  }
                }
                latch.countDown();
              }
            });
        latch.await();

        /** INSANITY TEST */
        final boolean[] insanityFailed = {true};
        Xtruct hello = new Xtruct();
        hello.string_thing = "Hello2";
        hello.byte_thing = 2;
        hello.i32_thing = 2;
        hello.i64_thing = 2;

        Xtruct goodbye = new Xtruct();
        goodbye.string_thing = "Goodbye4";
        goodbye.byte_thing = (byte) 4;
        goodbye.i32_thing = 4;
        goodbye.i64_thing = (long) 4;

        insane.userMap = new HashMap<Numberz, Long>();
        insane.userMap.put(Numberz.EIGHT, (long) 8);
        insane.userMap.put(Numberz.FIVE, (long) 5);
        insane.xtructs = new ArrayList<Xtruct>();
        insane.xtructs.add(goodbye);
        insane.xtructs.add(hello);

        System.out.print("testInsanity()");
        latch = new CountDownLatch(1);
        testClient.testInsanity(
            insane,
            new AsyncMethodCallback<Map<Long, Map<Numberz, Insanity>>>() {
              @Override
              public void onComplete(Map<Long, Map<Numberz, Insanity>> whoa) {
                System.out.print(" = {");
                for (long key : whoa.keySet()) {
                  Map<Numberz, Insanity> val = whoa.get(key);
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
                        System.out.print(
                            "{"
                                + "\""
                                + x.string_thing
                                + "\", "
                                + x.byte_thing
                                + ", "
                                + x.i32_thing
                                + ", "
                                + x.i64_thing
                                + "}, ");
                      }
                    }
                    System.out.print("}");

                    System.out.print("}, ");
                  }
                  System.out.print("}, ");
                }
                System.out.print("}\n");
                if (whoa.size() == 2 && whoa.containsKey(1L) && whoa.containsKey(2L)) {
                  Map<Numberz, Insanity> first_map = whoa.get(1L);
                  Map<Numberz, Insanity> second_map = whoa.get(2L);
                  if (first_map.size() == 2
                      && first_map.containsKey(Numberz.TWO)
                      && first_map.containsKey(Numberz.THREE)
                      && second_map.size() == 1
                      && second_map.containsKey(Numberz.SIX)
                      && insane.equals(first_map.get(Numberz.TWO))
                      && insane.equals(first_map.get(Numberz.THREE))) {
                    Insanity six = second_map.get(Numberz.SIX);
                    // Cannot use "new Insanity().equals(six)" because as of now, struct/container
                    // fields with default requiredness have isset=false for local instances and yet
                    // received empty values from other languages like C++ have isset=true .
                    if (six.getUserMapSize() == 0 && six.getXtructsSize() == 0) {
                      // OK
                      insanityFailed[0] = false;
                    }
                  }
                }
                latch.countDown();
              }

              @Override
              public void onError(Exception ex) {
                returnCode |= ERR_STRUCTS;
                System.out.println("*** FAILURE ***\n");
                ex.printStackTrace(System.out);
                insanityFailed[0] = false;
                latch.countDown();
              }
            });
        latch.await();
        if (insanityFailed[0]) {
          returnCode |= ERR_STRUCTS;
          System.out.println("*** FAILURE ***\n");
        }

        /** EXECPTION TEST */
        System.out.print("testClient.testException(\"Xception\") =>");
        latch = new CountDownLatch(1);
        testClient.testException(
            "Xception",
            new AsyncMethodCallback<Void>() {
              @Override
              public void onComplete(Void response) {
                System.out.print("  void\n*** FAILURE ***\n");
                returnCode |= ERR_EXCEPTIONS;
                latch.countDown();
              }

              @Override
              public void onError(Exception exception) {
                if (exception instanceof Xception) {
                  Xception e = (Xception) exception;
                  System.out.printf("  {%d, \"%s\"}\n", e.errorCode, e.message);
                }
                testClient = getClient();
                latch.countDown();
              }
            });
        latch.await();

        System.out.print("testClient.testException(\"TException\") =>");
        latch = new CountDownLatch(1);
        testClient.testException(
            "TException",
            new AsyncMethodCallback<Void>() {
              @Override
              public void onComplete(Void response) {
                System.out.print("  void\n*** FAILURE ***\n");
                returnCode |= ERR_EXCEPTIONS;
                latch.countDown();
              }

              @Override
              public void onError(Exception exception) {
                if (exception instanceof TException) {
                  TException e = (TException) exception;
                  System.out.printf("  {\"%s\"}\n", e.getMessage());
                }
                testClient = getClient();
                latch.countDown();
              }
            });
        latch.await();

        System.out.print("testClient.testException(\"success\") =>");
        latch = new CountDownLatch(1);
        testClient.testException(
            "success",
            new AsyncMethodCallback<Void>() {
              @Override
              public void onComplete(Void response) {
                System.out.print("  void\n");
                latch.countDown();
              }

              @Override
              public void onError(Exception exception) {
                System.out.printf("  exception\n*** FAILURE ***\n");
                returnCode |= ERR_EXCEPTIONS;
                latch.countDown();
              }
            });
        latch.await();

        /** MULTI EXCEPTION TEST */
        System.out.printf("testClient.testMultiException(\"Xception\", \"test 1\") =>");
        latch = new CountDownLatch(1);
        testClient.testMultiException(
            "Xception",
            "test 1",
            new AsyncMethodCallback<Xtruct>() {
              @Override
              public void onComplete(Xtruct response) {
                System.out.print("  result\n*** FAILURE ***\n");
                returnCode |= ERR_EXCEPTIONS;
                latch.countDown();
              }

              @Override
              public void onError(Exception exception) {
                if (exception instanceof Xception) {
                  Xception e = (Xception) exception;
                  System.out.printf("  {%d, \"%s\"}\n", e.errorCode, e.message);
                }
                testClient = getClient();
                latch.countDown();
              }
            });
        latch.await();

        System.out.printf("testClient.testMultiException(\"Xception2\", \"test 2\") =>");
        latch = new CountDownLatch(1);
        testClient.testMultiException(
            "Xception2",
            "test 2",
            new AsyncMethodCallback<Xtruct>() {
              @Override
              public void onComplete(Xtruct response) {
                System.out.print("  result\n*** FAILURE ***\n");
                returnCode |= ERR_EXCEPTIONS;
                latch.countDown();
              }

              @Override
              public void onError(Exception exception) {
                if (exception instanceof Xception2) {
                  Xception2 e = (Xception2) exception;
                  System.out.printf("  {%d, {\"%s\"}}\n", e.errorCode, e.struct_thing.string_thing);
                }
                testClient = getClient();
                latch.countDown();
              }
            });
        latch.await();

        System.out.print("testClient.testMultiException(\"success\", \"test 3\") =>");
        latch = new CountDownLatch(1);
        testClient.testMultiException(
            "success",
            "test 3",
            new AsyncMethodCallback<Xtruct>() {
              @Override
              public void onComplete(Xtruct result) {
                System.out.printf("  {{\"%s\"}}\n", result.string_thing);
                latch.countDown();
              }

              @Override
              public void onError(Exception exception) {
                System.out.printf("  exception\n*** FAILURE ***\n");
                returnCode |= ERR_EXCEPTIONS;
                latch.countDown();
              }
            });
        latch.await();

        /** ONEWAY TEST */
        System.out.print("testOneway(3)...");
        long startOneway = System.nanoTime();
        latch = new CountDownLatch(1);
        testClient.testOneway(
            3,
            new FailureLessCallback<Void>() {
              @Override
              public void onComplete(Void response) {
                long onewayElapsedMillis = (System.nanoTime() - startOneway) / 1000000;
                if (onewayElapsedMillis > 200) {
                  System.out.println(
                      "Oneway test took too long to execute failed: took "
                          + onewayElapsedMillis
                          + "ms");
                  System.out.println(
                      "oneway calls are 'fire and forget' and therefore should not cause blocking.");
                  System.out.println(
                      "Some transports (HTTP) have a required response, and typically this failure");
                  System.out.println(
                      "means the transport response was delayed until after the execution");
                  System.out.println(
                      "of the RPC.  The server should post the transport response immediately and");
                  System.out.println("before executing the RPC.");
                  System.out.println("*** FAILURE ***");
                  returnCode |= ERR_BASETYPES;
                } else {
                  System.out.println(
                      "Success - fire and forget only took " + onewayElapsedMillis + "ms");
                }
                latch.countDown();
              }
            });
        latch.await();

        long stop = System.nanoTime();
        long tot = stop - start;

        System.out.println("Total time: " + tot / 1000 + "us");

        if (timeMin == 0 || tot < timeMin) {
          timeMin = tot;
        }
        if (tot > timeMax) {
          timeMax = tot;
        }
        timeTot += tot;
        clientManager.stop();
      } catch (Exception x) {
        System.out.printf("*** FAILURE ***\n");
        x.printStackTrace();
        returnCode |= ERR_UNKNOWN;
      }
    }

    long timeAvg = timeTot / numTests;

    System.out.println("Min time: " + timeMin / 1000 + "us");
    System.out.println("Max time: " + timeMax / 1000 + "us");
    System.out.println("Avg time: " + timeAvg / 1000 + "us");

    try {
      String json = (new TSerializer(new TSimpleJSONProtocol.Factory())).toString(insane);
      System.out.println("\nSample TSimpleJSONProtocol output:\n" + json);
    } catch (TException x) {
      System.out.println("*** FAILURE ***");
      x.printStackTrace();
      returnCode |= ERR_BASETYPES;
    }

    System.exit(returnCode);
  }

  private static ThriftTest.AsyncClient getClient() {
    return new ThriftTest.AsyncClient(getProtocolFactory(), clientManager, getTransport());
  }

  private static SecondService.AsyncClient getSecondServiceClient() {
    return new SecondService.AsyncClient(getProtocolFactory(), clientManager, getTransport());
  }

  private static TProtocolFactory getProtocolFactory() {
    TProtocolFactory tProtocolFactory;
    if (protocol_type.equals("json") || protocol_type.equals("multij")) {
      tProtocolFactory = new TJSONProtocol.Factory();
    } else if (protocol_type.equals("compact") || protocol_type.equals("multic")) {
      tProtocolFactory = new TCompactProtocol.Factory();
    } else {
      tProtocolFactory = new TBinaryProtocol.Factory();
    }
    return tProtocolFactory;
  }

  private static TNonblockingTransport getTransport() {
    TNonblockingTransport transport = null;
    try {
      if (ssl) {
        transport = TSSLTransportFactory.getNonblockingClientSocket(host, port, socketTimeout);
      } else {
        transport = new TNonblockingSocket(host, port, socketTimeout);
      }
    } catch (Exception x) {
      x.printStackTrace();
      System.exit(ERR_UNKNOWN);
    }
    return transport;
  }

  private abstract static class FailureLessCallback<T> implements AsyncMethodCallback<T> {
    @Override
    public void onError(Exception exception) {
      latch.countDown();
    }
  }
}
