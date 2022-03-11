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

import org.apache.thrift.transport.TEOFException;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

import java.nio.ByteBuffer;

/**
 * Read frames from a transport. Each frame has a header and a payload. A header will indicate
 * the size of the payload and other informations about how to decode payload.
 * Implementations should subclass it by providing a header reader implementation.
 *
 * @param <T> Header type.
 */
public abstract class FrameReader<T extends FrameHeaderReader> {
  private final T header;
  private ByteBuffer payload;

  protected FrameReader(T header) {
    this.header = header;
  }

  /**
   * (Nonblocking) Read available bytes out of the transport without blocking to wait for incoming
   * data.
   *
   * @param transport TTransport
   * @return true if current frame is complete after read.
   * @throws TSaslNegotiationException if fail to read back a valid sasl negotiation message.
   * @throws TTransportException if io error.
   */
  public boolean read(TTransport transport) throws TSaslNegotiationException, TTransportException {
    if (!header.isComplete()) {
      if (readHeader(transport)) {
        payload = ByteBuffer.allocate(header.payloadSize());
      } else {
        return false;
      }
    }
    if (header.payloadSize() == 0) {
      return true;
    }
    return readPayload(transport);
  }

  /**
   * (Nonblocking) Try to read available header bytes from transport.
   *
   * @return true if header is complete after read.
   * @throws TSaslNegotiationException if fail to read back a validd sasl negotiation header.
   * @throws TTransportException if io error.
   */
  private boolean readHeader(TTransport transport) throws TSaslNegotiationException, TTransportException {
    return header.read(transport);
  }

  /**
   * (Nonblocking) Try to read available
   *
   * @param transport underlying transport.
   * @return true if payload is complete after read.
   * @throws TTransportException if io error.
   */
  private boolean readPayload(TTransport transport) throws TTransportException {
    readAvailable(transport, payload);
    return payload.hasRemaining();
  }

  /**
   *
   * @return header of the frame
   */
  public T getHeader() {
    return header;
  }

  /**
   *
   * @return number of bytes of the header
   */
  public int getHeaderSize() {
    return header.toBytes().length;
  }

  /**
   *
   * @return byte array of the payload
   */
  public byte[] getPayload() {
    return payload.array();
  }

  /**
   *
   * @return size of the payload
   */
  public int getPayloadSize() {
    return header.payloadSize();
  }

  /**
   *
   * @return true if the reader has fully read a frame
   */
  public boolean isComplete() {
    return !(payload == null || payload.hasRemaining());
  }

  /**
   * Reset the state of the reader so that it can be reused to read a new frame.
   */
  public void clear() {
    header.clear();
    payload = null;
  }

  /**
   * Read immediately available bytes from the transport into the byte buffer.
   *
   * @param transport TTransport
   * @param recipient ByteBuffer
   * @return number of bytes read out of the transport
   * @throws TTransportException if io error
   */
  static int readAvailable(TTransport transport, ByteBuffer recipient) throws TTransportException {
    if (!recipient.hasRemaining()) {
      throw new IllegalStateException("Trying to fill a full recipient with " + recipient.limit()
          + " bytes");
    }
    int currentPosition = recipient.position();
    byte[] bytes = recipient.array();
    int offset = recipient.arrayOffset() + currentPosition;
    int expectedLength = recipient.remaining();
    int got = transport.read(bytes, offset, expectedLength);
    if (got < 0) {
      throw new TEOFException("Transport is closed, while trying to read " + expectedLength +
          " bytes");
    }
    recipient.position(currentPosition + got);
    return got;
  }
}
