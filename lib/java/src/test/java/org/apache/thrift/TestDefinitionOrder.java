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

import static org.junit.jupiter.api.Assertions.*;

import java.io.*;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;

// Tests that declaring fields in different order (esp. when they reference each other) generates
// identical code
public class TestDefinitionOrder {

  @Test
  public void testDefinitionOrder() throws Exception {
    List<String> filenames = Arrays.asList("Parent.java", "Child.java", "MyEnum.java");

    for (String fn : filenames) {
      String fnA = "definition-order-test/a/" + fn;
      String fnB = "definition-order-test/b/" + fn;

      try (InputStream isA = TestDefinitionOrder.class.getClassLoader().getResourceAsStream(fnA);
          InputStream isB = TestDefinitionOrder.class.getClassLoader().getResourceAsStream(fnB)) {

        assertNotNull(isA, "Resource not found: " + fnA);
        assertNotNull(isB, "Resource not found: " + fnB);

        int hashA = Arrays.hashCode(readAllBytes(isA));
        assertEquals(
            hashA,
            Arrays.hashCode(readAllBytes(isB)),
            String.format("Generated Java files %s and %s differ", fnA, fnB));
      }
    }
  }

  // TODO Use InputStream.readAllBytes post-Java8
  private byte[] readAllBytes(InputStream is) throws IOException {
    ByteArrayOutputStream os = new ByteArrayOutputStream();
    byte[] buff = new byte[1024];
    int bytesRead;
    while ((bytesRead = is.read(buff, 0, buff.length)) != -1) {
      os.write(buff, 0, bytesRead);
    }
    return os.toByteArray();
  }
}
