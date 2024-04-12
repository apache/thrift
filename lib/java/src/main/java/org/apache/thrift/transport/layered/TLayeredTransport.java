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
package org.apache.thrift.transport.layered;

import java.util.Objects;
import org.apache.thrift.TConfiguration;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

public abstract class TLayeredTransport extends TTransport {

  private final TTransport innerTransport;

  @Override
  public TConfiguration getConfiguration() {
    return innerTransport.getConfiguration();
  }

  public TLayeredTransport(TTransport transport) {
    Objects.requireNonNull(transport, "TTransport cannot be null.");
    innerTransport = transport;
  }

  @Override
  public void updateKnownMessageSize(long size) throws TTransportException {
    innerTransport.updateKnownMessageSize(size);
  }

  @Override
  public void checkReadBytesAvailable(long numBytes) throws TTransportException {
    innerTransport.checkReadBytesAvailable(numBytes);
  }

  public TTransport getInnerTransport() {
    return innerTransport;
  }
}
