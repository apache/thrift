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

import javax.security.auth.callback.CallbackHandler;
import java.util.Map;

/**
 * Contains all the parameters used to define a SASL server implementation.
 */
public class TSaslServerDefinition {
  public final String mechanism;
  public final String protocol;
  public final String serverName;
  public final Map<String, String> props;
  public final CallbackHandler cbh;

  public TSaslServerDefinition(String mechanism, String protocol, String serverName,
                               Map<String, String> props, CallbackHandler cbh) {
    this.mechanism = mechanism;
    this.protocol = protocol;
    this.serverName = serverName;
    this.props = props;
    this.cbh = cbh;
  }
}
