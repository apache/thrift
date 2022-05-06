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

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;

/**
 * A superset of Validate class in Apache commons lang3.
 *
 * <p>It provides consistent message strings for frequently encountered checks. That simplifies
 * callers because they have to supply only the name of the argument that failed a check instead of
 * having to supply the entire message.
 */
public final class Validate {
  private Validate() {}

  /** Validates that the given reference argument is not null. */
  public static void checkNotNull(Object obj, String argName) {
    checkArgument(obj != null, "'%s' must not be null.", argName);
  }

  /** Validates that the given integer argument is not zero or negative. */
  public static void checkPositiveInteger(long value, String argName) {
    checkArgument(value > 0, "'%s' must be a positive integer.", argName);
  }

  /** Validates that the given integer argument is not negative. */
  public static void checkNotNegative(long value, String argName) {
    checkArgument(value >= 0, "'%s' must not be negative.", argName);
  }

  /*
   * Validates that the expression (that checks a required field is present) is true.
   */
  public static void checkRequired(boolean isPresent, String argName) {
    checkArgument(isPresent, "'%s' is required.", argName);
  }

  /** Validates that the expression (that checks a field is valid) is true. */
  public static void checkValid(boolean isValid, String argName) {
    checkArgument(isValid, "'%s' is invalid.", argName);
  }

  /** Validates that the expression (that checks a field is valid) is true. */
  public static void checkValid(boolean isValid, String argName, String validValues) {
    checkArgument(isValid, "'%s' is invalid. Valid values are: %s.", argName, validValues);
  }

  /** Validates that the given string is not null and has non-zero length. */
  public static void checkNotNullAndNotEmpty(String arg, String argName) {
    Validate.checkNotNull(arg, argName);
    Validate.checkArgument(arg.length() > 0, "'%s' must not be empty.", argName);
  }

  /** Validates that the given array is not null and has at least one element. */
  public static <T> void checkNotNullAndNotEmpty(T[] array, String argName) {
    Validate.checkNotNull(array, argName);
    checkNotEmpty(array.length, argName);
  }

  /** Validates that the given array is not null and has at least one element. */
  public static void checkNotNullAndNotEmpty(byte[] array, String argName) {
    Validate.checkNotNull(array, argName);
    checkNotEmpty(array.length, argName);
  }

  /** Validates that the given array is not null and has at least one element. */
  public static void checkNotNullAndNotEmpty(short[] array, String argName) {
    Validate.checkNotNull(array, argName);
    checkNotEmpty(array.length, argName);
  }

  /** Validates that the given array is not null and has at least one element. */
  public static void checkNotNullAndNotEmpty(int[] array, String argName) {
    Validate.checkNotNull(array, argName);
    checkNotEmpty(array.length, argName);
  }

  /** Validates that the given array is not null and has at least one element. */
  public static void checkNotNullAndNotEmpty(long[] array, String argName) {
    Validate.checkNotNull(array, argName);
    checkNotEmpty(array.length, argName);
  }

  /** Validates that the given buffer is not null and has non-zero capacity. */
  public static <T> void checkNotNullAndNotEmpty(Iterable<T> iter, String argName) {
    Validate.checkNotNull(iter, argName);
    int minNumElements = iter.iterator().hasNext() ? 1 : 0;
    checkNotEmpty(minNumElements, argName);
  }

  /** Validates that the given set is not null and has an exact number of items. */
  public static <T> void checkNotNullAndNumberOfElements(
      Collection<T> collection, int numElements, String argName) {
    Validate.checkNotNull(collection, argName);
    checkArgument(
        collection.size() == numElements,
        "Number of elements in '%s' must be exactly %s, %s given.",
        argName,
        numElements,
        collection.size());
  }

  /** Validates that the given two values are equal. */
  public static void checkValuesEqual(
      long value1, String value1Name, long value2, String value2Name) {
    checkArgument(
        value1 == value2,
        "'%s' (%s) must equal '%s' (%s).",
        value1Name,
        value1,
        value2Name,
        value2);
  }

  /** Validates that the first value is an integer multiple of the second value. */
  public static void checkIntegerMultiple(
      long value1, String value1Name, long value2, String value2Name) {
    checkArgument(
        (value1 % value2) == 0,
        "'%s' (%s) must be an integer multiple of '%s' (%s).",
        value1Name,
        value1,
        value2Name,
        value2);
  }

  /** Validates that the first value is greater than the second value. */
  public static void checkGreater(long value1, String value1Name, long value2, String value2Name) {
    checkArgument(
        value1 > value2,
        "'%s' (%s) must be greater than '%s' (%s).",
        value1Name,
        value1,
        value2Name,
        value2);
  }

  /** Validates that the first value is greater than or equal to the second value. */
  public static void checkGreaterOrEqual(
      long value1, String value1Name, long value2, String value2Name) {
    checkArgument(
        value1 >= value2,
        "'%s' (%s) must be greater than or equal to '%s' (%s).",
        value1Name,
        value1,
        value2Name,
        value2);
  }

  /** Validates that the first value is less than or equal to the second value. */
  public static void checkLessOrEqual(
      long value1, String value1Name, long value2, String value2Name) {
    checkArgument(
        value1 <= value2,
        "'%s' (%s) must be less than or equal to '%s' (%s).",
        value1Name,
        value1,
        value2Name,
        value2);
  }

  /** Validates that the given value is within the given range of values. */
  public static void checkWithinRange(
      long value, String valueName, long minValueInclusive, long maxValueInclusive) {
    checkArgument(
        (value >= minValueInclusive) && (value <= maxValueInclusive),
        "'%s' (%s) must be within the range [%s, %s].",
        valueName,
        value,
        minValueInclusive,
        maxValueInclusive);
  }

  /** Validates that the given value is within the given range of values. */
  public static void checkWithinRange(
      double value, String valueName, double minValueInclusive, double maxValueInclusive) {
    checkArgument(
        (value >= minValueInclusive) && (value <= maxValueInclusive),
        "'%s' (%s) must be within the range [%s, %s].",
        valueName,
        value,
        minValueInclusive,
        maxValueInclusive);
  }

  public static void checkPathExists(Path path, String argName) {
    checkNotNull(path, argName);
    checkArgument(Files.exists(path), "Path %s (%s) does not exist.", argName, path);
  }

  public static void checkPathExistsAsDir(Path path, String argName) {
    checkPathExists(path, argName);
    checkArgument(
        Files.isDirectory(path), "Path %s (%s) must point to a directory.", argName, path);
  }

  public static void checkPathExistsAsFile(Path path, String argName) {
    checkPathExists(path, argName);
    checkArgument(Files.isRegularFile(path), "Path %s (%s) must point to a file.", argName, path);
  }

  public static void checkArgument(boolean expression, String format, Object... args) {
    org.apache.commons.lang3.Validate.isTrue(expression, format, args);
  }

  public static void checkState(boolean expression, String format, Object... args) {
    org.apache.commons.lang3.Validate.validState(expression, format, args);
  }

  private static void checkNotEmpty(int arraySize, String argName) {
    Validate.checkArgument(arraySize > 0, "'%s' must have at least one element.", argName);
  }
}
