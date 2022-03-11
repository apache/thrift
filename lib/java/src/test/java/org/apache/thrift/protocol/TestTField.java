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
package org.apache.thrift.protocol;

import junit.framework.TestCase;
import static org.junit.Assert.assertNotEquals;

public abstract class TestTField extends TestCase {

  public void testConstructor() {
    TField uut = new TField();
    assertEquals("", uut.name);
    assertEquals(TType.STOP, uut.type);
    assertEquals(0, uut.id);

    uut = new TField("foo", TType.VOID, (short)42);
    assertEquals("foo", uut.name);
    assertEquals(TType.VOID, uut.type);
    assertEquals(42, uut.id);
  }

  public void testEquality() {
    TField uut1 = new TField();
    TField uut2 = new TField();
    assertEquals(uut1, uut2);
    assertEquals(uut1.hashCode(), uut2.hashCode());

    uut1 = new TField("foo", TType.I32, (short)1);
    uut2 = new TField("foo", TType.I32, (short)2);
    assertNotEquals(uut1, uut2);
    assertNotEquals(uut1.hashCode(), uut2.hashCode());

    uut1 = new TField("foo", TType.VOID, (short)1);
    uut2 = new TField("foo", TType.I32, (short)1);
    assertNotEquals(uut1, uut2);
    assertNotEquals(uut1.hashCode(), uut2.hashCode());

    uut1 = new TField("foo", TType.VOID, (short)5);
    uut2 = new TField("bar", TType.I32, (short)5);
    assertEquals(uut1, uut2); // name field is ignored
    assertEquals(uut1.hashCode(), uut2.hashCode());
  }

}
