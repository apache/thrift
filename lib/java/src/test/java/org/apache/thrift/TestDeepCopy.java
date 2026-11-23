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

import static org.junit.jupiter.api.Assertions.assertNotSame;

import org.junit.jupiter.api.Test;
import thrift.test.DeepCopyBar;
import thrift.test.DeepCopyFoo;

public class TestDeepCopy {

  @Test
  public void testDeepCopy() throws Exception {
    final DeepCopyFoo foo = new DeepCopyFoo();

    foo.addToL(new DeepCopyBar());
    foo.addToS(new DeepCopyBar());
    foo.putToM("test 3", new DeepCopyBar());

    foo.addToLi(new thrift.test.Object());
    foo.addToSi(new thrift.test.Object());
    foo.putToMi("test 3", new thrift.test.Object());

    foo.setBar(new DeepCopyBar());

    final DeepCopyFoo deepCopyFoo = foo.deepCopy();

    assertNotSame(foo.getBar(), deepCopyFoo.getBar());

    assertNotSame(foo.getL().get(0), deepCopyFoo.getL().get(0));
    assertNotSame(
        foo.getS().toArray(new DeepCopyBar[0])[0],
        deepCopyFoo.getS().toArray(new DeepCopyBar[0])[0]);
    assertNotSame(foo.getM().get("test 3"), deepCopyFoo.getM().get("test 3"));

    assertNotSame(foo.getLi().get(0), deepCopyFoo.getLi().get(0));
    assertNotSame(
        foo.getSi().toArray(new thrift.test.Object[0])[0],
        deepCopyFoo.getSi().toArray(new thrift.test.Object[0])[0]);
    assertNotSame(foo.getMi().get("test 3"), deepCopyFoo.getMi().get("test 3"));
  }
}
