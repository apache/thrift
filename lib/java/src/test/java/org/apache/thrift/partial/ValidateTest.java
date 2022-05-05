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

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import org.junit.jupiter.api.Test;

public class ValidateTest {
  @Test
  public void testCheckNotNull() {
    String nonNullArg = "nonNullArg";
    String nullArg = null;

    // Should not throw.
    Validate.checkNotNull(nonNullArg, "nonNullArg");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNull(nullArg, "nullArg"),
        "'nullArg' must not be null");
  }

  @Test
  public void testCheckPositiveInteger() {
    int positiveArg = 1;
    int zero = 0;
    int negativeArg = -1;

    // Should not throw.
    Validate.checkPositiveInteger(positiveArg, "positiveArg");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkPositiveInteger(negativeArg, "negativeArg"),
        "'negativeArg' must be a positive integer");
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkPositiveInteger(zero, "zero"),
        "'zero' must be a positive integer");
  }

  @Test
  public void testCheckNotNegative() {
    int positiveArg = 1;
    int zero = 0;
    int negativeArg = -1;

    // Should not throw.
    Validate.checkNotNegative(zero, "zeroArg");
    Validate.checkNotNegative(positiveArg, "positiveArg");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNegative(negativeArg, "negativeArg"),
        "'negativeArg' must not be negative");
  }

  @Test
  public void testCheckRequired() {
    // Should not throw.
    Validate.checkRequired(true, "arg");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkRequired(false, "arg"),
        "'arg' is required");
  }

  @Test
  public void testCheckValid() {
    // Should not throw.
    Validate.checkValid(true, "arg");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkValid(false, "arg"),
        "'arg' is invalid");
  }

  @Test
  public void testCheckValidWithValues() {
    String validValues = "foo, bar";

    // Should not throw.
    Validate.checkValid(true, "arg", validValues);

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkValid(false, "arg", validValues),
        "'arg' is invalid. Valid values are: foo, bar");
  }

  @Test
  public void testCheckNotNullAndNotEmpty() {
    // Should not throw.
    Validate.checkNotNullAndNotEmpty(TestData.nonEmptyArray, "array");
    Validate.checkNotNullAndNotEmpty(TestData.nonEmptyByteArray, "array");
    Validate.checkNotNullAndNotEmpty(TestData.nonEmptyShortArray, "array");
    Validate.checkNotNullAndNotEmpty(TestData.nonEmptyIntArray, "array");
    Validate.checkNotNullAndNotEmpty(TestData.nonEmptyLongArray, "array");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty("", "string"),
        "'string' must not be empty");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.nullArray, "array"),
        "'array' must not be null");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.emptyArray, "array"),
        "'array' must have at least one element");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.nullByteArray, "array"),
        "'array' must not be null");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.emptyByteArray, "array"),
        "'array' must have at least one element");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.nullShortArray, "array"),
        "'array' must not be null");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.emptyShortArray, "array"),
        "'array' must have at least one element");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.nullIntArray, "array"),
        "'array' must not be null");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.emptyIntArray, "array"),
        "'array' must have at least one element");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.nullLongArray, "array"),
        "'array' must not be null");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.emptyLongArray, "array"),
        "'array' must have at least one element");
  }

  @Test
  public void testCheckListNotNullAndNotEmpty() {
    // Should not throw.
    Validate.checkNotNullAndNotEmpty(TestData.validList, "list");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.nullList, "list"),
        "'list' must not be null");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNotEmpty(TestData.emptyList, "list"),
        "'list' must have at least one element");
  }

  @Test
  public void testCheckNotNullAndNumberOfElements() {
    // Should not throw.
    Validate.checkNotNullAndNumberOfElements(Arrays.asList(1, 2, 3), 3, "arg");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNumberOfElements(null, 3, "arg"),
        "'arg' must not be null");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkNotNullAndNumberOfElements(Arrays.asList(1, 2), 3, "arg"),
        "Number of elements in 'arg' must be exactly 3, 2 given.");
  }

  @Test
  public void testCheckValuesEqual() {
    // Should not throw.
    Validate.checkValuesEqual(1, "arg1", 1, "arg2");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkValuesEqual(1, "arg1", 2, "arg2"),
        "'arg1' (1) must equal 'arg2' (2)");
  }

  @Test
  public void testCheckIntegerMultiple() {
    // Should not throw.
    Validate.checkIntegerMultiple(10, "arg1", 5, "arg2");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkIntegerMultiple(10, "arg1", 3, "arg2"),
        "'arg1' (10) must be an integer multiple of 'arg2' (3)");
  }

  @Test
  public void testCheckGreater() {
    // Should not throw.
    Validate.checkGreater(10, "arg1", 5, "arg2");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkGreater(5, "arg1", 10, "arg2"),
        "'arg1' (5) must be greater than 'arg2' (10)");
  }

  @Test
  public void testCheckGreaterOrEqual() {
    // Should not throw.
    Validate.checkGreaterOrEqual(10, "arg1", 5, "arg2");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkGreaterOrEqual(5, "arg1", 10, "arg2"),
        "'arg1' (5) must be greater than or equal to 'arg2' (10)");
  }

  @Test
  public void testCheckWithinRange() {
    // Should not throw.
    Validate.checkWithinRange(10, "arg", 5, 15);
    Validate.checkWithinRange(10.0, "arg", 5.0, 15.0);

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkWithinRange(5, "arg", 10, 20),
        "'arg' (5) must be within the range [10, 20]");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkWithinRange(5.0, "arg", 10.0, 20.0),
        "'arg' (5.0) must be within the range [10.0, 20.0]");
  }

  @Test
  public void testCheckPathExists() throws IOException {
    Path tempFile = Files.createTempFile("foo", "bar");
    Path tempDir = tempFile.getParent();
    Path notFound = Paths.get("<not-found>");

    // Should not throw.
    Validate.checkPathExists(tempFile, "tempFile");
    Validate.checkPathExists(tempDir, "tempDir");

    // Verify it throws.
    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkPathExists(null, "nullArg"),
        "'nullArg' must not be null");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkPathExists(notFound, "notFound"),
        "Path notFound (<not-found>) does not exist");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkPathExistsAsDir(tempFile, "tempFile"),
        "must point to a directory");

    assertThrows(
        IllegalArgumentException.class,
        () -> Validate.checkPathExistsAsFile(tempDir, "tempDir"),
        "must point to a file");
  }
}
