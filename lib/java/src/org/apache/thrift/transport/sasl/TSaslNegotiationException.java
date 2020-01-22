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
 * Exception for sasl negotiation errors.
 */
public class TSaslNegotiationException extends TTransportException {

  private final ErrorType error;

  public TSaslNegotiationException(ErrorType error, String summary) {
    super(summary);
    this.error = error;
  }

  public TSaslNegotiationException(ErrorType error, String summary, Throwable cause) {
    super(summary, cause);
    this.error = error;
  }

  public ErrorType getErrorType() {
    return error;
  }

  /**
   * @return Errory type plus the message.
   */
  public String getSummary() {
    return error.name() + ": " + getMessage();
  }

  /**
   * @return Summary and eventually the cause's message.
   */
  public String getDetails() {
    return getCause() == null ? getSummary() : getSummary() + "\nReason: " + getCause().getMessage();
  }

  public enum ErrorType {
    // Unexpected system internal error during negotiation (e.g. sasl initialization failure)
    INTERNAL_ERROR(NegotiationStatus.ERROR),
    // Cannot read correct sasl frames from the connection => Send "ERROR" status byte to peer
    PROTOCOL_ERROR(NegotiationStatus.ERROR),
    // Peer is using unsupported sasl mechanisms => Send "BAD" status byte to peer
    MECHANISME_MISMATCH(NegotiationStatus.BAD),
    // Sasl authentication failure => Send "BAD" status byte to peer
    AUTHENTICATION_FAILURE(NegotiationStatus.BAD),
    ;

    public final NegotiationStatus code;

    ErrorType(NegotiationStatus code) {
      this.code = code;
    }
  }
}
