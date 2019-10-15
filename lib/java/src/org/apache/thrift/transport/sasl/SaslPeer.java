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

import org.apache.thrift.transport.TTransportException;

/**
 * A peer in a sasl negotiation.
 */
public interface SaslPeer {

  /**
   * Evaluate and validate the negotiation message (response/challenge) received from peer.
   *
   * @param negotiationMessage response/challenge received from peer.
   * @return new response/challenge to send to peer, can be null if authentication becomes success.
   * @throws TSaslNegotiationException if sasl authentication fails.
   */
  byte[] evaluate(byte[] negotiationMessage) throws TSaslNegotiationException;

  /**
   * @return true if authentication is done.
   */
  boolean isAuthenticated();

  /**
   * This method can only be called when the negotiation is complete (isAuthenticated returns true).
   * Otherwise it will throw IllegalStateExceptiion.
   *
   * @return if the qop requires some integrity/confidential protection.
   * @throws IllegalStateException if negotiation is not yet complete.
   */
  boolean isDataProtected();

  /**
   * Wrap raw bytes to protect it.
   *
   * @param data raw bytes.
   * @param offset the start position of the content to wrap.
   * @param length the length of the content to wrap.
   * @return bytes with protection to send to peer.
   * @throws TTransportException if failure.
   */
  byte[] wrap(byte[] data, int offset, int length) throws TTransportException;

  /**
   * Wrap the whole byte array.
   *
   * @param data raw bytes.
   * @return wrapped bytes.
   * @throws TTransportException if failure.
   */
  default byte[] wrap(byte[] data) throws TTransportException {
    return wrap(data, 0, data.length);
  }

  /**
   * Unwrap protected data to raw bytes.
   *
   * @param data protected data received from peer.
   * @param offset the start position of the content to unwrap.
   * @param length the length of the content to unwrap.
   * @return raw bytes.
   * @throws TTransportException if failed.
   */
  byte[] unwrap(byte[] data, int offset, int length) throws TTransportException;

  /**
   * Unwrap the whole byte array.
   *
   * @param data wrapped bytes.
   * @return raw bytes.
   * @throws TTransportException if failure.
   */
  default byte[] unwrap(byte[] data) throws TTransportException {
    return unwrap(data, 0, data.length);
  }

  /**
   * Close this peer and release resources.
   */
  void dispose();
}
