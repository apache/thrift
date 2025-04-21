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
package org.apache.thrift.transport;

import java.util.Objects;
import org.apache.thrift.TConfiguration;

public abstract class TEndpointTransport extends TTransport {

  protected long getMaxMessageSize() {
    return getConfiguration().getMaxMessageSize();
  }

  public int getMaxFrameSize() {
    return getConfiguration().getMaxFrameSize();
  }

  public void setMaxFrameSize(int maxFrameSize) {
    getConfiguration().setMaxFrameSize(maxFrameSize);
  }

  private long consumedMessage;

  private final TConfiguration _configuration;

  public TConfiguration getConfiguration() {
    return _configuration;
  }

  public TEndpointTransport(TConfiguration config) {
    _configuration = Objects.isNull(config) ? new TConfiguration() : config;
  }

  @Override
  public void readMessageBegin() {
    consumedMessage = 0;
  }

  public final void consumeReadMessageBytes(int size) throws TTransportException {
    consumedMessage += size;
    if (consumedMessage > getMaxMessageSize())
      throw new TTransportException(
          TTransportException.MESSAGE_SIZE_LIMIT,
          "Message size exceeds limit: " + getMaxMessageSize());
  }

  public long getConsumedMessage() {
    return consumedMessage;
  }
}
