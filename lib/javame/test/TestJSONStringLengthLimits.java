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
import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TMessage;
import org.apache.thrift.protocol.TMessageType;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

/**
 * A JSON string or number is delimited rather than length-prefixed, so its size
 * is whatever the peer sends: there is no declared length to reject up front,
 * and every value, including the method name of a message, is held to the same
 * maximum TBinaryProtocol applies as it is read. On a CLDC heap measured in
 * megabytes there is no slack for reading a value of any length, and this
 * profile has no TConfiguration to account against, so the maximum is a plain
 * ceiling of its own.
 *
 * A counting transport stands in for the peer, so the tests can tell "stopped
 * at the maximum" apart from "read the whole value and returned it" -- which is
 * the only difference that matters, and the one a bare "did it throw?" check
 * misses. The payloads are plain ASCII, so one transport byte is read per
 * accumulated byte and the read stops within a few bytes of the maximum.
 *
 * JavaME has no build harness; from the lib/javame directory run:
 *
 *   javac -d /tmp/jme $(find src -name '*.java' ! -name 'THttpClient.java') \
 *       test/TestJSONStringLengthLimits.java
 *   java -cp /tmp/jme TestJSONStringLengthLimits
 *
 * (THttpClient.java is excluded because it needs the javax.microedition API.)
 */
public class TestJSONStringLengthLimits {

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

  private static String rep(String s, int n) {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < n; i++) {
      b.append(s);
    }
    return b.toString();
  }

  private static byte[] ascii(String s) throws Exception {
    return s.getBytes("UTF-8");
  }

  private static boolean isSizeLimit(TProtocolException e) {
    return e.getType() == TProtocolException.SIZE_LIMIT;
  }

  private static void stringTests() throws Exception {
    {
      CountingTransport inner = new CountingTransport(ascii("\"" + rep("a", 64) + "\""));
      TJSONProtocol proto = new TJSONProtocol(inner);
      proto.setMaxStringLength(32);
      boolean sizeLimit = false;
      try {
        proto.readString();
      } catch (TProtocolException e) {
        sizeLimit = isSizeLimit(e);
      } catch (Exception e) {
        // anything else leaves sizeLimit false
      }
      check(sizeLimit && inner.bytesRequested <= 32 + 8,
          "a JSON string over the maximum is refused near it (SIZE_LIMIT="
              + sizeLimit + ", bytesRequested=" + inner.bytesRequested + ")");
    }

    {
      CountingTransport inner = new CountingTransport(ascii("\"" + rep("a", 32) + "\""));
      TJSONProtocol proto = new TJSONProtocol(inner);
      proto.setMaxStringLength(32);
      check(proto.readString().equals(rep("a", 32)),
          "a JSON string at the maximum still reads");
    }

    {
      CountingTransport inner = new CountingTransport(ascii("\"" + rep("a", 33) + "\""));
      TJSONProtocol proto = new TJSONProtocol(inner);
      proto.setMaxStringLength(32);
      boolean sizeLimit = false;
      try {
        proto.readString();
      } catch (TProtocolException e) {
        sizeLimit = isSizeLimit(e);
      }
      check(sizeLimit, "a JSON string one byte over the maximum is refused");
    }
  }

  private static void numberTests() throws Exception {
    {
      CountingTransport inner = new CountingTransport(ascii(rep("1", 64) + " "));
      TJSONProtocol proto = new TJSONProtocol(inner);
      proto.setMaxStringLength(32);
      boolean sizeLimit = false;
      try {
        proto.readI64();
      } catch (TProtocolException e) {
        sizeLimit = isSizeLimit(e);
      } catch (Exception e) {
        // anything else leaves sizeLimit false
      }
      check(sizeLimit && inner.bytesRequested <= 32 + 8,
          "a JSON number over the maximum is refused near it (SIZE_LIMIT="
              + sizeLimit + ", bytesRequested=" + inner.bytesRequested + ")");
    }

    {
      // 32 numeric characters, with leading zeros so the value stays small.
      CountingTransport inner = new CountingTransport(ascii(rep("0", 30) + "42 "));
      TJSONProtocol proto = new TJSONProtocol(inner);
      proto.setMaxStringLength(32);
      check(proto.readI64() == 42L, "a JSON number at the maximum still reads");
    }
  }

  private static void base64Tests() throws Exception {
    {
      CountingTransport inner = new CountingTransport(ascii("\"" + rep("QUJD", 16) + "\""));
      TJSONProtocol proto = new TJSONProtocol(inner);
      proto.setMaxStringLength(32);
      boolean sizeLimit = false;
      try {
        proto.readBinary();
      } catch (TProtocolException e) {
        sizeLimit = isSizeLimit(e);
      } catch (Exception e) {
        // anything else leaves sizeLimit false
      }
      check(sizeLimit && inner.bytesRequested <= 32 + 8,
          "a JSON base64 value over the maximum is refused near it (SIZE_LIMIT="
              + sizeLimit + ", bytesRequested=" + inner.bytesRequested + ")");
    }

    {
      CountingTransport inner = new CountingTransport(ascii("\"" + rep("QUJD", 8) + "\""));
      TJSONProtocol proto = new TJSONProtocol(inner);
      proto.setMaxStringLength(32);
      byte[] got = proto.readBinary();
      check(new String(got, "UTF-8").equals(rep("ABC", 8)),
          "a JSON base64 value at the maximum still reads");
    }
  }

  private static void messageNameTests() throws Exception {
    {
      CountingTransport inner =
          new CountingTransport(ascii("[1,\"" + rep("a", 64) + "\",1,0]"));
      TJSONProtocol proto = new TJSONProtocol(inner);
      proto.setMaxStringLength(32);
      boolean sizeLimit = false;
      try {
        proto.readMessageBegin();
      } catch (TProtocolException e) {
        sizeLimit = isSizeLimit(e);
      } catch (Exception e) {
        // anything else leaves sizeLimit false
      }
      check(sizeLimit && inner.bytesRequested <= 32 + 12,
          "a JSON message name over the maximum is refused near it (SIZE_LIMIT="
              + sizeLimit + ", bytesRequested=" + inner.bytesRequested + ")");
    }
  }

  private static void defaultAndRoundTrip() throws Exception {
    {
      TJSONProtocol proto = new TJSONProtocol(new CountingTransport(new byte[0]));
      check(proto.getMaxStringLength() == TBinaryProtocol.DEFAULT_MAX_STRING_LENGTH,
          "the maximum has the same default as the binary protocol");
    }

    {
      // A message written with TJSONProtocol reads back the same at the default.
      TMemoryBuffer buffer = new TMemoryBuffer(1024);
      TJSONProtocol out = new TJSONProtocol(buffer);
      out.writeMessageBegin(new TMessage("someMethod", TMessageType.CALL, 7));
      out.writeString("plain \" and \\ and \n text");
      out.writeBinary(ascii("a\0z"));
      out.writeI64(-123456L);
      out.writeMessageEnd();

      TJSONProtocol in = new TJSONProtocol(buffer);
      TMessage msg = in.readMessageBegin();
      boolean ok = msg.name.equals("someMethod")
          && msg.type == TMessageType.CALL
          && msg.seqid == 7;
      ok = ok && in.readString().equals("plain \" and \\ and \n text");
      ok = ok && new String(in.readBinary(), "UTF-8").equals("a\0z");
      ok = ok && in.readI64() == -123456L;
      in.readMessageEnd();
      check(ok, "a message round-trips through the JSON protocol unchanged");
    }
  }

  public static void main(String[] args) throws Exception {
    stringTests();
    numberTests();
    base64Tests();
    messageNameTests();
    defaultAndRoundTrip();
    System.out.println("\n" + checks + " checks, " + failures + " failures");
    if (failures > 0) {
      System.exit(1);
    }
  }
}
