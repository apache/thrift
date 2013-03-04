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

import org.apache.thrift.TDeserializer;
import org.apache.thrift.TException;

import thrift.test.Bonk;

public class TestTBinaryProtocol extends ProtocolTestBase {
  @Override
  protected TProtocolFactory getFactory() {
    return new TBinaryProtocol.Factory();
  }

  @Override
  protected boolean canBeUsedNaked() {
    return true;
  }

  public void testOOMDenialOfService() throws Exception {
    TDeserializer deser = new TDeserializer(new TBinaryProtocol
					    .Factory(false, false, 1000));
    Bonk bonk = new Bonk();
    try {
      // Invalid read length specified here. Would cause an OOM
      // without the limit on the read length
      deser.deserialize(bonk, new byte[]{11, 0, 1, 127, -1, -1, -1});
    } catch (TException e) {
      // Ignore as we are only checking for OOM in the failure case
    }
  }
}
