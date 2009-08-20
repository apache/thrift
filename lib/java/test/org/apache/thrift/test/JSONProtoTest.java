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

  public static void main(String [] args) throws Exception {
   try {
      System.out.println("In JSON Proto test");

      OneOfEach ooe = Fixtures.oneOfEach;
      Nesting n = Fixtures.nesting;

      HolyMoley hm = Fixtures.holyMoley;

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
