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

import javax.security.sasl.Sasl;
import javax.security.sasl.SaslException;
import javax.security.sasl.SaslServer;

import org.apache.thrift.transport.TTransportException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.apache.thrift.transport.sasl.TSaslNegotiationException.ErrorType.AUTHENTICATION_FAILURE;

/**
 * Server side sasl peer, a wrapper around SaslServer to provide some handy methods.
 */
public class ServerSaslPeer implements SaslPeer {
  private static final Logger LOGGER = LoggerFactory.getLogger(ServerSaslPeer.class);

  private static final String QOP_AUTH_INT = "auth-int";
  private static final String QOP_AUTH_CONF = "auth-conf";

  private final SaslServer saslServer;

  public ServerSaslPeer(SaslServer saslServer) {
    this.saslServer = saslServer;
  }

  @Override
  public byte[] evaluate(byte[] negotiationMessage) throws TSaslNegotiationException {
    try {
      return saslServer.evaluateResponse(negotiationMessage);
    } catch (SaslException e) {
      throw new TSaslNegotiationException(AUTHENTICATION_FAILURE,
          "Authentication failed with " + saslServer.getMechanismName(), e);
    }
  }

  @Override
  public boolean isAuthenticated() {
    return saslServer.isComplete();
  }

  @Override
  public boolean isDataProtected() {
    Object qop = saslServer.getNegotiatedProperty(Sasl.QOP);
    if (qop == null) {
      return false;
    }
    for (String word : qop.toString().split("\\s*,\\s*")) {
      String lowerCaseWord = word.toLowerCase();
      if (QOP_AUTH_INT.equals(lowerCaseWord) || QOP_AUTH_CONF.equals(lowerCaseWord)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public byte[] wrap(byte[] data, int offset, int length) throws TTransportException {
    try {
      return saslServer.wrap(data, offset, length);
    } catch (SaslException e) {
      throw new TTransportException("Failed to wrap data", e);
    }
  }

  @Override
  public byte[] unwrap(byte[] data, int offset, int length) throws TTransportException {
    try {
      return saslServer.unwrap(data, offset, length);
    } catch (SaslException e) {
      throw new TTransportException(TTransportException.CORRUPTED_DATA, "Failed to unwrap data", e);
    }
  }

  @Override
  public void dispose() {
    try {
      saslServer.dispose();
    } catch (Exception e) {
      LOGGER.warn("Failed to close sasl server " + saslServer.getMechanismName(), e);
    }
  }

  SaslServer getSaslServer() {
    return saslServer;
  }

}
