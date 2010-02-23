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

public class CompareTest {
  public static void main(String[] args) throws Exception {
    Bonk bonk1 = new Bonk();
    Bonk bonk2 = new Bonk();

    // Compare empty thrift objects.
    if (bonk1.compareTo(bonk2) != 0)
      throw new RuntimeException("empty objects not equal according to compareTo");

    bonk1.setMessage("m");

    // Compare one thrift object with a filled in field and another without it.
    if (bonk1.compareTo(bonk2) <= 0)
      throw new RuntimeException("compare " + bonk1 + " to " + bonk2 + " returned " + bonk1.compareTo(bonk2));
    if (bonk2.compareTo(bonk1) >= 0)
      throw new RuntimeException("compare " + bonk2 + " to " + bonk1 + " returned " + bonk2.compareTo(bonk1));

    // Compare both have filled-in fields.
    bonk2.setMessage("z");
    if (bonk1.compareTo(bonk2) >= 0)
      throw new RuntimeException("compare " + bonk1 + " to " + bonk2 + " returned " + bonk1.compareTo(bonk2));
    if (bonk2.compareTo(bonk1) <= 0)
      throw new RuntimeException("compare " + bonk2 + " to " + bonk1 + " returned " + bonk2.compareTo(bonk1));

    // Compare bonk1 has a field filled in that bonk2 doesn't.
    bonk1.setType(123);
    if (bonk1.compareTo(bonk2) <= 0)
      throw new RuntimeException("compare " + bonk1 + " to " + bonk2 + " returned " + bonk1.compareTo(bonk2));
    if (bonk2.compareTo(bonk1) >= 0)
      throw new RuntimeException("compare " + bonk2 + " to " + bonk1 + " returned " + bonk2.compareTo(bonk1));

    // Compare bonk1 and bonk2 equal.
    bonk2.setType(123);
    bonk2.setMessage("m");
    if (bonk1.compareTo(bonk2) != 0)
      throw new RuntimeException("compare " + bonk1 + " to " + bonk2 + " returned " + bonk1.compareTo(bonk2));
  }
}
