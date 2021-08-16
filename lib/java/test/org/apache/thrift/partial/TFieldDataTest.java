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

package org.apache.thrift.partial;

import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TType;
import org.junit.Test;

import static org.junit.Assert.*;

public class TFieldDataTest {

  @Test
  public void testEncodeStop() {
    TField field = new TField("", TType.STOP, (short) 0);
    int data = TFieldData.encode(TType.STOP);

    assertEquals(field.type, TFieldData.getType(data));
    assertEquals(field.id, TFieldData.getId(data));
  }

  @Test
  public void testEncodeRest() {
    for (byte type = 1; type <= 16; type++) {
      for (short id = 0; id < Short.MAX_VALUE; id++) {
        TField field = new TField("", type, id);
        int data = TFieldData.encode(type, id);

        assertEquals(field.type, TFieldData.getType(data));
        assertEquals(field.id, TFieldData.getId(data));
      }
    }
  }
}
