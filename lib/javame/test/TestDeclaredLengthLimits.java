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

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

/**
 * A frame length and a string length are numbers the peer chose, and
 * new byte[n] commits all of n before readAll blocks for the bytes. On a
 * CLDC heap measured in megabytes there is no slack for getting that wrong,
 * and this profile has no TConfiguration to account against, so both are held
 * to a plain ceiling.
 *
 * A counting transport stands in for the peer, so the tests can tell "refused
 * the declared length" apart from "tried to read it and ran out" -- which is
 * the only difference that matters, and the one a bare "did it throw?" check
 * misses.
 *
 * JavaME has no build harness; from the lib/javame directory run:
 *
 *   javac -d /tmp/jme $(find src -name '*.java' ! -name 'THttpClient.java') \
 *       test/TestDeclaredLengthLimits.java
 *   java -cp /tmp/jme TestDeclaredLengthLimits
 *
 * (THttpClient.java is excluded because it needs the javax.microedition API.)
 */
public class TestDeclaredLengthLimits {

  private static int checks = 0;
  private static int failures = 0;

  private static void check(boolean ok, String what) {
    checks++;
    if (ok) {
      System.out.println("ok   - " + what);
    } else {
      failures++;
      System.out.println("FAIL - " + what);
    }
  }

  /** Serves a fixed buffer and records how much was asked of it. */
  private static class CountingTransport extends TTransport {
    private final byte[] data;
    private int pos = 0;
    int bytesRequested = 0;

    CountingTransport(byte[] data) {
      this.data = data;
    }

    public boolean isOpen() {
      return true;
    }

    public void open() {}

    public void close() {}

    public int read(byte[] buf, int off, int len) throws TTransportException {
      bytesRequested += len;
      int give = Math.min(len, data.length - pos);
      if (give <= 0) {
        throw new TTransportException(TTransportException.END_OF_FILE);
      }
      System.arraycopy(data, pos, buf, off, give);
      pos += give;
      return give;
    }

    public void write(byte[] buf, int off, int len) {}
  }

  private static byte[] i32(int value) {
    return new byte[] {
      (byte) ((value >> 24) & 0xff),
      (byte) ((value >> 16) & 0xff),
      (byte) ((value >> 8) & 0xff),
      (byte) (value & 0xff)
    };
  }

  private static byte[] concat(byte[] a, byte[] b) {
    byte[] out = new byte[a.length + b.length];
    System.arraycopy(a, 0, out, 0, a.length);
    System.arraycopy(b, 0, out, a.length, b.length);
    return out;
  }

  private static void frameSizeTests() throws Exception {
    {
      CountingTransport inner = new CountingTransport(i32(0x7FFFFFFF));
      TFramedTransport framed = new TFramedTransport(inner);
      boolean threw = false;
      try {
        framed.read(new byte[1], 0, 1);
      } catch (TTransportException e) {
        threw = true;
      }
      check(threw, "a frame declaring 2 GB is refused");
      check(inner.bytesRequested == 4,
          "the declared frame size is never asked of the transport (asked for "
              + inner.bytesRequested + ")");
    }

    {
      CountingTransport inner = new CountingTransport(i32(-1));
      TFramedTransport framed = new TFramedTransport(inner);
      boolean threwTransport = false;
      boolean threwNegativeArray = false;
      try {
        framed.read(new byte[1], 0, 1);
      } catch (TTransportException e) {
        threwTransport = true;
      } catch (NegativeArraySizeException e) {
        threwNegativeArray = true;
      }
      check(threwTransport && !threwNegativeArray,
          "a negative frame size raises TTransportException, not "
              + "NegativeArraySizeException");
    }

    {
      byte[] payload = new byte[33];
      CountingTransport inner =
          new CountingTransport(concat(i32(payload.length), payload));
      TFramedTransport framed = new TFramedTransport(inner, 32);
      boolean threw = false;
      try {
        framed.read(new byte[1], 0, 1);
      } catch (TTransportException e) {
        threw = true;
      }
      check(threw, "a frame over a lowered maximum is refused");
    }

    {
      byte[] payload = "hallo world".getBytes("UTF-8");
      CountingTransport inner =
          new CountingTransport(concat(i32(payload.length), payload));
      TFramedTransport framed = new TFramedTransport(inner);
      byte[] out = new byte[payload.length];
      framed.readAll(out, 0, out.length);
      check(new String(out, "UTF-8").equals("hallo world"),
          "a frame within the maximum still reads");
    }
  }

  private static void stringLengthTests() throws Exception {
    {
      CountingTransport inner = new CountingTransport(i32(0x7FFFFFFF));
      TBinaryProtocol proto = new TBinaryProtocol(inner);
      boolean sizeLimit = false;
      try {
        proto.readBinary();
      } catch (TProtocolException e) {
        sizeLimit = e.getType() == TProtocolException.SIZE_LIMIT;
      } catch (Exception e) {
        // anything else leaves sizeLimit false
      }
      check(sizeLimit, "a binary field declaring 2 GB is refused with SIZE_LIMIT");
      check(inner.bytesRequested == 4,
          "the declared field length is never asked of the transport (asked for "
              + inner.bytesRequested + ")");
    }

    {
      byte[] payload = "a readable string".getBytes("UTF-8");
      CountingTransport inner =
          new CountingTransport(concat(i32(payload.length), payload));
      TBinaryProtocol proto = new TBinaryProtocol(inner);
      check(proto.readString().equals("a readable string"),
          "a string within the maximum still reads");
    }

    {
      byte[] payload = new byte[64];
      CountingTransport inner =
          new CountingTransport(concat(i32(payload.length), payload));
      TBinaryProtocol proto = new TBinaryProtocol(inner);
      proto.setMaxStringLength(32);
      boolean sizeLimit = false;
      try {
        proto.readBinary();
      } catch (TProtocolException e) {
        sizeLimit = e.getType() == TProtocolException.SIZE_LIMIT;
      }
      check(sizeLimit, "a field over a lowered maximum is refused");
    }
  }

  public static void main(String[] args) throws Exception {
    frameSizeTests();
    stringLengthTests();
    System.out.println("\n" + checks + " checks, " + failures + " failures");
    if (failures > 0) {
      System.exit(1);
    }
  }
}
