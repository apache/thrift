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


package org.apache.thrift;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import thrift.test.Bonk;
import thrift.test.CompactProtoTestStruct;
import thrift.test.HolyMoley;
import thrift.test.Nesting;
import thrift.test.OneOfEach;

public class Fixtures {
  public static final OneOfEach oneOfEach;
  public static final Nesting nesting;
  public static final HolyMoley holyMoley;
  public static final CompactProtoTestStruct compactProtoTestStruct;

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

  static {
    try {
      oneOfEach = new OneOfEach();
      oneOfEach.setIm_true(true);
      oneOfEach.setIm_false(false);
      oneOfEach.setA_bite((byte) 0xd6);
      oneOfEach.setInteger16((short)27000);
      oneOfEach.setInteger32(1 << 24);
      oneOfEach.setInteger64((long) 6000 * 1000 * 1000);
      oneOfEach.setDouble_precision(Math.PI);
      oneOfEach.setSome_characters("JSON THIS! \"\1");
      oneOfEach.setZomg_unicode(new String(kUnicodeBytes, "UTF-8"));
      oneOfEach.setBase64(ByteBuffer.wrap("base64".getBytes()));
      // byte, i16, and i64 lists are populated by default constructor

      Bonk bonk = new Bonk();
      bonk.setType(31337);
      bonk.setMessage("I am a bonk... xor!");
      nesting = new Nesting(bonk, oneOfEach);

      holyMoley = new HolyMoley();
      ArrayList big = new ArrayList<OneOfEach>();
      big.add(new OneOfEach(oneOfEach));
      big.add(nesting.my_ooe);
      holyMoley.setBig(big);
      holyMoley.getBig().get(0).setA_bite((byte) 0x22);
      holyMoley.getBig().get(0).setA_bite((byte) 0x23);

      holyMoley.setContain(new HashSet<List<String>>());
      ArrayList<String> stage1 = new ArrayList<String>(2);
      stage1.add("and a one");
      stage1.add("and a two");
      holyMoley.getContain().add(stage1);
      stage1 = new ArrayList<String>(3);
      stage1.add("then a one, two");
      stage1.add("three!");
      stage1.add("FOUR!!");
      holyMoley.getContain().add(stage1);
      stage1 = new ArrayList<String>(0);
      holyMoley.getContain().add(stage1);

      ArrayList<Bonk> stage2 = new ArrayList<Bonk>();
      holyMoley.setBonks(new HashMap<String, List<Bonk>>());
      // one empty
      holyMoley.getBonks().put("zero", stage2);

      // one with two
      stage2 = new ArrayList<Bonk>();
      Bonk b = new Bonk();
      b.setType(1);
      b.setMessage("Wait.");
      stage2.add(b);
      b = new Bonk();
      b.setType(2);
      b.setMessage("What?");
      stage2.add(b);      
      holyMoley.getBonks().put("two", stage2);

      // one with three
      stage2 = new ArrayList<Bonk>();
      b = new Bonk();
      b.setType(3);
      b.setMessage("quoth");
      b = new Bonk();
      b.setType(4);
      b.setMessage("the raven");
      b = new Bonk();
      b.setType(5);
      b.setMessage("nevermore");
      holyMoley.getBonks().put("three", stage2);

      // superhuge compact proto test struct
      compactProtoTestStruct = new CompactProtoTestStruct(thrift.test.Constants.COMPACT_TEST);
      compactProtoTestStruct.setA_binary(ByteBuffer.wrap(new byte[]{0,1,2,3,4,5,6,7,8}));
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
