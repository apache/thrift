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

import org.apache.thrift.TConfiguration;
import org.apache.thrift.TException;
import org.apache.thrift.fuzz.FuzzTest;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransport;

public class FuzzTestUtils {
  // 10MB message size limit to prevent over-allocation during fuzzing
  private static final int FUZZ_MAX_MESSAGE_SIZE = 10 * 1024 * 1024;

  /**
   * Common fuzzing test implementation that attempts to parse input data using the provided
   * protocol factory.
   *
   * @param input The input bytes to fuzz
   * @param factory The protocol factory to use for parsing
   * @throws Exception if an unexpected error occurs
   */
  public static void testParse(byte[] input, TProtocolFactory factory) throws Exception {
    try {
      TConfiguration config = new TConfiguration();
      config.setMaxMessageSize(FUZZ_MAX_MESSAGE_SIZE);
      TTransport transport = new TMemoryInputTransport(config, input);
      TProtocol protocol = factory.getProtocol(transport);

      // Try to read a FuzzTest object from the input
      FuzzTest obj = new FuzzTest();
      obj.read(protocol);
    } catch (TException e) {
      // Ignore Thrift exceptions - they're expected when fuzzing
    } catch (Exception e) {
      // TODO: For now we are ignoring unexpected exceptions, but we should fix this
      // and handle them appropriately. Need to understand the contract to see if it's a
      // spec violation or not for us to throw non-TException when parsing.
    }
  }

  /**
   * Common fuzzing test implementation that performs a roundtrip test using the provided protocol
   * factory. It deserializes the input, serializes it again, and verifies the objects are equal
   * after a second deserialization.
   *
   * @param input The input bytes to fuzz
   * @param factory The protocol factory to use for serialization/deserialization
   * @throws Exception if an unexpected error occurs
   */
  public static void testRoundtrip(byte[] input, TProtocolFactory factory) throws Exception {
    try {
      TConfiguration config = new TConfiguration();
      config.setMaxMessageSize(FUZZ_MAX_MESSAGE_SIZE);

      // First try to deserialize the raw input bytes
      TTransport inputTransport = new TMemoryInputTransport(config, input);
      TProtocol inProtocol = factory.getProtocol(inputTransport);

      // Try to read a FuzzTest object from the input
      FuzzTest inputObj = new FuzzTest();
      inputObj.read(inProtocol);

      // Now do the roundtrip test with the successfully deserialized object
      TMemoryBuffer memoryBuffer = new TMemoryBuffer(config, input.length);
      TProtocol outProtocol = factory.getProtocol(memoryBuffer);
      inputObj.write(outProtocol);

      // Get the serialized bytes
      byte[] serialized = memoryBuffer.getArray();

      // Deserialize again
      TTransport secondInputTransport = new TMemoryInputTransport(config, serialized);
      TProtocol secondInProtocol = factory.getProtocol(secondInputTransport);
      FuzzTest outputObj = new FuzzTest();
      outputObj.read(secondInProtocol);

      // Assert equality
      if (!inputObj.equals(outputObj)) {
        throw new AssertionError("Roundtrip objects are not equal");
      }
    } catch (TException e) {
      // Ignore Thrift exceptions - they're expected when fuzzing
    } catch (Exception e) {
      // TODO: For now we are ignoring unexpected exceptions, but we should fix this
      // and handle them appropriately. Need to understand the contract to see if it's a
      // spec violation or not for us to throw non-TException when parsing.
    }
  }
}
