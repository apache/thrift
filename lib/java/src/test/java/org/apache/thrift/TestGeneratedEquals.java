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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.jupiter.api.Test;
import thrift.test.OneOfEach;
import thrift.test.Simple;

/** Behavior and shape of the generated equals() methods (THRIFT-4191). */
public class TestGeneratedEquals {

  @Test
  public void testRequiredAndDefaultPrimitives() {
    Simple a = new Simple((short) 1, (short) 2);
    Simple b = new Simple((short) 1, (short) 2);
    assertEquals(a, b);
    assertEquals(a.hashCode(), b.hashCode());

    b.setIm_default((short) 3);
    assertNotEquals(a, b);
    b.setIm_default((short) 1);
    b.setIm_required((short) 3);
    assertNotEquals(a, b);
  }

  @Test
  public void testOptionalPrimitiveUsesIssetBit() {
    Simple a = new Simple((short) 1, (short) 2);
    Simple b = new Simple((short) 1, (short) 2);

    // Set on one side only.
    b.setIm_optional((short) 0);
    assertNotEquals(a, b);
    assertNotEquals(b, a);

    // Set on both sides, equal and different.
    a.setIm_optional((short) 0);
    assertEquals(a, b);
    a.setIm_optional((short) 7);
    assertNotEquals(a, b);

    // Unset again: the stale value must not matter.
    a.unsetIm_optional();
    b.unsetIm_optional();
    assertEquals(a, b);
  }

  @Test
  public void testNullableAndBinaryFields() {
    OneOfEach a = new OneOfEach();
    OneOfEach b = new OneOfEach();
    assertEquals(a, b);

    a.setSome_characters("x");
    assertNotEquals(a, b);
    assertNotEquals(b, a);
    b.setSome_characters("x");
    assertEquals(a, b);

    a.setBase64(new byte[] {1, 2, 3});
    assertNotEquals(a, b);
    b.setBase64(new byte[] {1, 2, 3});
    assertEquals(a, b);
    b.setBase64(new byte[] {1, 2, 4});
    assertNotEquals(a, b);

    b.setBase64(new byte[] {1, 2, 3});
    b.setIm_true(true);
    assertNotEquals(a, b);
    a.setIm_true(true);
    assertEquals(a, b);
  }

  /**
   * A field that is neither optional nor nullable is compared directly; the generated code must not
   * guard it with presence flags that are constant true.
   */
  @Test
  public void testNoConstantPresenceFlagsForPrimitives() throws Exception {
    Path src = Paths.get(System.getProperty("test.genSrc"), "thrift", "test", "Simple.java");
    String code = new String(Files.readAllBytes(src), StandardCharsets.UTF_8);

    assertFalse(code.contains("this_present_im_default"), code);
    assertFalse(code.contains("this_present_im_required"), code);
    assertTrue(code.contains("if (this.im_default != that.im_default)"), code);
    assertTrue(code.contains("if (this.im_required != that.im_required)"), code);
    // The optional field still needs the isset-based guard.
    assertTrue(
        code.contains("boolean this_present_im_optional = true && this.isSetIm_optional();"));
  }
}
