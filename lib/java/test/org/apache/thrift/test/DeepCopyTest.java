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

import org.apache.thrift.TDeserializer;
import org.apache.thrift.TSerializer;
import org.apache.thrift.protocol.TBinaryProtocol;
import thrift.test.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

public class DeepCopyTest {
  public static void main(String[] args) throws Exception {
    TSerializer   binarySerializer   = new   TSerializer(new TBinaryProtocol.Factory());
    TDeserializer binaryDeserializer = new TDeserializer(new TBinaryProtocol.Factory());

    HolyMoley hm = Fixtures.holyMoley;

    byte[] binaryCopy = binarySerializer.serialize(hm);
    HolyMoley hmCopy = new HolyMoley();
    binaryDeserializer.deserialize(hmCopy, binaryCopy);
    HolyMoley hmCopy2 = new HolyMoley(hm);

    if (!hm.equals(hmCopy))
      throw new RuntimeException("copy constructor modified the original object!");
    if (!hmCopy.equals(hmCopy2))
      throw new RuntimeException("copy constructor generated incorrect copy");

    hm.big.get(0).base64[0]++; // change binary value in original object
    if (hm.equals(hmCopy2)) // make sure the change didn't propagate to the copied object
      throw new RuntimeException("Binary field not copied correctly!");
    hm.big.get(0).base64[0]--; // undo change

    hmCopy2.bonks.get("two").get(1).message = "What else?";

    if (hm.equals(hmCopy2))
      throw new RuntimeException("A deep copy was not done!");

  }
}
