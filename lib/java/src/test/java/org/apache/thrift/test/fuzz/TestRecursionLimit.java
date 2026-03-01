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

package org.apache.thrift.test.fuzz;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import org.apache.thrift.TConfiguration;
import org.apache.thrift.fuzz.RecursiveStruct;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransport;
import org.junit.jupiter.api.Test;

/**
 * Tests for TConfiguration.recursionLimit enforcement during deserialization. This brings Java in
 * line with C++ which already enforces recursion limits.
 */
public class TestRecursionLimit {

  private static final int DEFAULT_RECURSION_LIMIT = 64;

  /**
   * Creates a nested RecursiveStruct with the specified depth.
   *
   * @param depth The depth of nesting
   * @return A RecursiveStruct with the specified depth
   */
  private RecursiveStruct createNestedStruct(int depth) {
    RecursiveStruct root = new RecursiveStruct(0);
    RecursiveStruct current = root;
    for (int i = 1; i < depth; i++) {
      RecursiveStruct next = new RecursiveStruct(i);
      current.setRecurse(next);
      current = next;
    }
    return root;
  }

  /** Tests that Binary protocol enforces the recursion limit. */
  @Test
  public void testBinaryProtocolRecursionLimit() throws Exception {
    RecursiveStruct deepStruct = createNestedStruct(DEFAULT_RECURSION_LIMIT + 10);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    TConfiguration config = new TConfiguration();
    TTransport transport = new TIOStreamTransport(config, baos);
    TProtocol protocol = new TBinaryProtocol(transport);
    deepStruct.write(protocol);
    byte[] serialized = baos.toByteArray();

    TTransport inputTransport = new TMemoryInputTransport(config, serialized);
    TProtocol inputProtocol = new TBinaryProtocol(inputTransport);
    RecursiveStruct result = new RecursiveStruct();

    TProtocolException exception =
        assertThrows(
            TProtocolException.class,
            () -> {
              result.read(inputProtocol);
            });
    assertTrue(
        exception.getType() == TProtocolException.DEPTH_LIMIT,
        "Expected DEPTH_LIMIT exception type, got: " + exception.getType());
  }

  /** Tests that Compact protocol enforces the recursion limit. */
  @Test
  public void testCompactProtocolRecursionLimit() throws Exception {
    RecursiveStruct deepStruct = createNestedStruct(DEFAULT_RECURSION_LIMIT + 10);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    TConfiguration config = new TConfiguration();
    TTransport transport = new TIOStreamTransport(config, baos);
    TProtocol protocol = new TCompactProtocol(transport);
    deepStruct.write(protocol);
    byte[] serialized = baos.toByteArray();

    TTransport inputTransport = new TMemoryInputTransport(config, serialized);
    TProtocol inputProtocol = new TCompactProtocol(inputTransport);
    RecursiveStruct result = new RecursiveStruct();

    TProtocolException exception =
        assertThrows(
            TProtocolException.class,
            () -> {
              result.read(inputProtocol);
            });
    assertTrue(
        exception.getType() == TProtocolException.DEPTH_LIMIT,
        "Expected DEPTH_LIMIT exception type, got: " + exception.getType());
  }

  /** Tests that JSON protocol enforces the recursion limit. */
  @Test
  public void testJSONProtocolRecursionLimit() throws Exception {
    RecursiveStruct deepStruct = createNestedStruct(DEFAULT_RECURSION_LIMIT + 10);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    TConfiguration config = new TConfiguration();
    TTransport transport = new TIOStreamTransport(config, baos);
    TProtocol protocol = new TJSONProtocol(transport);
    deepStruct.write(protocol);
    byte[] serialized = baos.toByteArray();

    TTransport inputTransport = new TMemoryInputTransport(config, serialized);
    TProtocol inputProtocol = new TJSONProtocol(inputTransport);
    RecursiveStruct result = new RecursiveStruct();

    TProtocolException exception =
        assertThrows(
            TProtocolException.class,
            () -> {
              result.read(inputProtocol);
            });
    assertTrue(
        exception.getType() == TProtocolException.DEPTH_LIMIT,
        "Expected DEPTH_LIMIT exception type, got: " + exception.getType());
  }

  /** Tests that structures within the recursion limit deserialize successfully. */
  @Test
  public void testWithinRecursionLimit() throws Exception {
    RecursiveStruct deepStruct = createNestedStruct(DEFAULT_RECURSION_LIMIT);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    TConfiguration config = new TConfiguration();
    TTransport transport = new TIOStreamTransport(config, baos);
    TProtocol protocol = new TBinaryProtocol(transport);
    deepStruct.write(protocol);
    byte[] serialized = baos.toByteArray();

    TTransport inputTransport = new TMemoryInputTransport(config, serialized);
    TProtocol inputProtocol = new TBinaryProtocol(inputTransport);
    RecursiveStruct result = new RecursiveStruct();

    assertDoesNotThrow(() -> result.read(inputProtocol));
  }
}
