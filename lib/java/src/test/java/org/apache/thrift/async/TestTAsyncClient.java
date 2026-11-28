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

package org.apache.thrift.async;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.thrift.TException;
import org.junit.jupiter.api.Test;
import thrift.test.Srv;
import thrift.test.Srv.AsyncClient;

public class TestTAsyncClient {
  @Test
  public void testRaisesExceptionWhenUsedConcurrently() throws Exception {
    TAsyncClientManager mockClientManager =
        new TAsyncClientManager() {
          @Override
          public void call(TAsyncMethodCall method) throws TException {
            // do nothing
          }
        };

    Srv.AsyncClient c = new AsyncClient(null, mockClientManager, null);
    c.Janky(0, null);
    assertThrows(Exception.class, c::checkReady);
  }
}
