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

import org.apache.thrift.partial.ExceptionAsserts;

import org.apache.thrift.TEnum;
import org.apache.thrift.TException;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Test ThriftCodec serializes and deserializes thrift objects correctly.
 */
public class PartialThriftDeserializerFactoryTest {

  @Test
  public void testArgChecks() throws TException {
    List<String> fieldNames = Arrays.asList("i32Field");

    // Valid cases, should not throw.
    PartialThriftDeserializer binaryDeserializer =
        PartialThriftDeserializerFactory.createBinary(TestStruct.class, fieldNames);

    PartialThriftDeserializer compactDeserializer =
        PartialThriftDeserializerFactory.createCompact(TestStruct.class, fieldNames);

    // Verify it throws.
    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'thriftClass' must not be null",
        () -> PartialThriftDeserializerFactory.createBinary(null, fieldNames));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'fieldNames' must not be null",
        () -> PartialThriftDeserializerFactory.createBinary(TestStruct.class, null));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'thriftClass' must not be null",
        () -> PartialThriftDeserializerFactory.createCompact(null, fieldNames));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'fieldNames' must not be null",
        () -> PartialThriftDeserializerFactory.createCompact(TestStruct.class, null));
  }
}
