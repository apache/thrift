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

import java.util.HashMap;
import java.util.Map;

import javax.security.auth.callback.CallbackHandler;
import javax.security.sasl.Sasl;
import javax.security.sasl.SaslException;
import javax.security.sasl.SaslServer;

import static org.apache.thrift.transport.sasl.TSaslNegotiationException.ErrorType.MECHANISME_MISMATCH;
import static org.apache.thrift.transport.sasl.TSaslNegotiationException.ErrorType.PROTOCOL_ERROR;

/**
 * Factory to create sasl server. Users can extend this class to customize the SaslServer creation.
 */
public class TSaslServerFactory {

  private final Map<String, TSaslServerDefinition> saslMechanisms;

  public TSaslServerFactory() {
    this.saslMechanisms = new HashMap<>();
  }

  public void addSaslMechanism(String mechanism, String protocol, String serverName,
                               Map<String, String> props, CallbackHandler cbh) {
    TSaslServerDefinition definition = new TSaslServerDefinition(mechanism, protocol, serverName,
        props, cbh);
    saslMechanisms.put(definition.mechanism, definition);
  }

  public ServerSaslPeer getSaslPeer(String mechanism) throws TSaslNegotiationException {
    if (!saslMechanisms.containsKey(mechanism)) {
      throw new TSaslNegotiationException(MECHANISME_MISMATCH, "Unsupported mechanism " + mechanism);
    }
    TSaslServerDefinition saslDef = saslMechanisms.get(mechanism);
    try {
      SaslServer saslServer = Sasl.createSaslServer(saslDef.mechanism, saslDef.protocol,
          saslDef.serverName, saslDef.props, saslDef.cbh);
      return new ServerSaslPeer(saslServer);
    } catch (SaslException e) {
      throw new TSaslNegotiationException(PROTOCOL_ERROR, "Fail to create sasl server " + mechanism, e);
    }
  }
}
