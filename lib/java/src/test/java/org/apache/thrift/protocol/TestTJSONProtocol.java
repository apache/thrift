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
package org.apache.thrift.protocol;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.charset.StandardCharsets;
import org.apache.thrift.TException;
import org.apache.thrift.transport.TMemoryBuffer;
import org.junit.jupiter.api.Test;

public class TestTJSONProtocol extends ProtocolTestBase {
  @Override
  protected TProtocolFactory getFactory() {
    return new TJSONProtocol.Factory();
  }

  @Override
  protected boolean canBeUsedNaked() {
    return false;
  }

  @Test
  public void testEscapedUnicode() throws TException {
    String jsonString = "\"hello unicode \\u0e01\\ud834\\udd1e world\"";
    String expectedString = "hello unicode \u0e01\ud834\udd1e world";

    TMemoryBuffer buffer = new TMemoryBuffer(1000);
    TJSONProtocol protocol = new TJSONProtocol(buffer);
    buffer.write(jsonString.getBytes(StandardCharsets.UTF_8));

    assertEquals(expectedString, protocol.readString());
  }

  @Test
  public void testExactlySizedBuffer() throws TException {
    // Regression test for https://issues.apache.org/jira/browse/THRIFT-5383.
    // Ensures that a JSON string can be read after writing to a buffer just
    // large enough to contain it.
    String inputString = "abcdefg";
    TMemoryBuffer buffer = new TMemoryBuffer(inputString.length() + 2);

    TJSONProtocol protocol = new TJSONProtocol(buffer);
    protocol.writeString(inputString);
    String outputString = protocol.readString();

    assertEquals(inputString, outputString);
  }
}
