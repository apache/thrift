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

package org.apache.thrift.protocol;

import java.util.UUID;
import org.apache.thrift.TException;

/**
 * The TLegacyUuidProtocolDecorator that decorates an existing TProtocol to
 * provide backwards compatibility with the old UUID format that the Java
 * library used on the wire.
 *
 * The initial UUID implementation in Java was not according to the protocol
 * specification. This was fixed in THRIFT-5925 and as a result would break
 * backwards compatibility with existing implementations that depends on that
 * format.
 *
 * This decorator is especially useful where migration is not needed and
 * interop between other languages, that follows the specification, are not
 * needed.
 *
 * For usage see the TestTLegacyUuidProtocolDecorator tests.
 */
public class TLegacyUuidProtocolDecorator extends TProtocolDecorator {

  public TLegacyUuidProtocolDecorator(TProtocol protocol) {
    super(protocol);
  }

  @Override
  public void writeUuid(UUID uuid) throws TException {
    byte[] buf = new byte[16];

    long lsb = uuid.getLeastSignificantBits();
    buf[0] = (byte) (0xff & (lsb >> 56));
    buf[1] = (byte) (0xff & (lsb >> 48));
    buf[2] = (byte) (0xff & (lsb >> 40));
    buf[3] = (byte) (0xff & (lsb >> 32));
    buf[4] = (byte) (0xff & (lsb >> 24));
    buf[5] = (byte) (0xff & (lsb >> 16));
    buf[6] = (byte) (0xff & (lsb >> 8));
    buf[7] = (byte) (0xff & (lsb));

    long msb = uuid.getMostSignificantBits();
    buf[8] = (byte) (0xff & (msb >> 56));
    buf[9] = (byte) (0xff & (msb >> 48));
    buf[10] = (byte) (0xff & (msb >> 40));
    buf[11] = (byte) (0xff & (msb >> 32));
    buf[12] = (byte) (0xff & (msb >> 24));
    buf[13] = (byte) (0xff & (msb >> 16));
    buf[14] = (byte) (0xff & (msb >> 8));
    buf[15] = (byte) (0xff & (msb));

    getTransport().write(buf, 0, 16);
  }

  @Override
  public UUID readUuid() throws TException {
    byte[] buf = new byte[16];
    getTransport().readAll(buf, 0, 16);

    long lsb = ((long) (buf[0] & 0xff) << 56)
        | ((long) (buf[1] & 0xff) << 48)
        | ((long) (buf[2] & 0xff) << 40)
        | ((long) (buf[3] & 0xff) << 32)
        | ((long) (buf[4] & 0xff) << 24)
        | ((long) (buf[5] & 0xff) << 16)
        | ((long) (buf[6] & 0xff) << 8)
        | ((long) (buf[7] & 0xff));

    long msb = ((long) (buf[8] & 0xff) << 56)
        | ((long) (buf[9] & 0xff) << 48)
        | ((long) (buf[10] & 0xff) << 40)
        | ((long) (buf[11] & 0xff) << 32)
        | ((long) (buf[12] & 0xff) << 24)
        | ((long) (buf[13] & 0xff) << 16)
        | ((long) (buf[14] & 0xff) << 8)
        | ((long) (buf[15] & 0xff));

    return new UUID(msb, lsb);
  }
}
