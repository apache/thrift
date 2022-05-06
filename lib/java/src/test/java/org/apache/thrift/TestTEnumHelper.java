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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import thrift.test.Numberz;

public class TestTEnumHelper {

  @Test
  public void testGetByValue_ValidValues() {
    for (Numberz n : Numberz.values()) {
      int value = n.getValue();
      assertEquals(n, TEnumHelper.getByValue(Numberz.class, value));
    }
  }

  @Test
  public void testGetByValue_InvalidValue() {
    assertNull(TEnumHelper.getByValue(Numberz.class, 0));
  }

  @Test
  public void testGetByValue_InvalidClass() {
    assertNull(TEnumHelper.getByValue(TEnum.class, 0));
  }
}
