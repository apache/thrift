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

package org.apache.thrift.transport.sasl;

import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

/**
 * Read headers for a frame. For each frame, the header contains payload size and other metadata.
 */
public interface FrameHeaderReader {

  /**
   * As the thrift sasl specification states, all sasl messages (both for negotiatiing and for
   * sending data) should have a header to indicate the size of the payload.
   *
   * @return size of the payload.
   */
  int payloadSize();

  /**
   *
   * @return The received bytes for the header.
   * @throws IllegalStateException if isComplete returns false.
   */
  byte[] toBytes();

  /**
   * @return true if this header has all its fields set.
   */
  boolean isComplete();

  /**
   * Clear the header and make it available to read a new header.
   */
  void clear();

  /**
   * (Nonblocking) Read fields from underlying transport layer.
   *
   * @param transport underlying transport.
   * @return true if header is complete after read.
   * @throws TSaslNegotiationException if fail to read a valid header of a sasl negotiation message.
   * @throws TTransportException if io error.
   */
  boolean read(TTransport transport) throws TSaslNegotiationException, TTransportException;
}
