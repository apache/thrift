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
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.protocol.TProtocolUtil;
import org.apache.thrift.protocol.TType;
import org.apache.thrift.transport.TMemoryBuffer;

/**
 * TProtocolUtil.skip() is the path an unknown field takes, so its nesting is
 * chosen by the peer rather than by the IDL. It threads a maxDepth and
 * decrements it per level, which reads as bounded -- but it was seeded with
 * Integer.MAX_VALUE, so the guard could not fire and the only thing ending the
 * recursion was the stack running out. That arrives as StackOverflowError,
 * an Error rather than a TException, and escapes every handler expecting one.
 *
 * TProtocol already carried DEFAULT_RECURSION_DEPTH for the generated
 * read/write path (THRIFT-6055); skipping descends the same way reading does,
 * so it draws on the same budget.
 *
 * JavaME has no build harness; from the lib/javame directory run:
 *
 *   javac -d /tmp/jme $(find src -name '*.java' ! -name 'THttpClient.java') \
 *       test/TestSkipDepth.java
 *   java -cp /tmp/jme TestSkipDepth
 *
 * (THttpClient.java is excluded because it needs the javax.microedition API.)
 */
public class TestSkipDepth {

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

  /**
   * Writes `depth` nested structs, each holding one field of type STRUCT, then
   * closes them all. Every level costs the peer two bytes.
   */
  private static TMemoryBuffer nested(int depth) throws Exception {
    TMemoryBuffer buffer = new TMemoryBuffer(64 + depth * 4);
    TProtocol proto = new TBinaryProtocol(buffer);
    for (int i = 0; i < depth; i++) {
      proto.writeByte(TType.STRUCT);
      proto.writeI16((short) 1);
    }
    // One STOP ends the innermost struct's field list, then one more for each
    // struct on the way back out.
    for (int i = 0; i <= depth; i++) {
      proto.writeByte(TType.STOP);
    }
    return buffer;
  }

  private static void skipNested(int depth) throws Exception {
    TProtocol proto = new TBinaryProtocol(nested(depth));
    TProtocolUtil.skip(proto, TType.STRUCT);
  }

  public static void main(String[] args) throws Exception {
    check(TProtocol.DEFAULT_RECURSION_DEPTH == 64,
        "the shared recursion limit is 64");

    {
      boolean depthLimit = false;
      boolean stackOverflow = false;
      try {
        // Two bytes per level, so this is about 200 KB on the wire.
        skipNested(100000);
      } catch (TProtocolException e) {
        depthLimit = e.getType() == TProtocolException.DEPTH_LIMIT;
      } catch (StackOverflowError e) {
        stackOverflow = true;
      }
      check(depthLimit,
          "deep nesting is refused with DEPTH_LIMIT");
      check(!stackOverflow,
          "deep nesting does not reach StackOverflowError");
    }

    {
      boolean ok = true;
      try {
        skipNested(TProtocol.DEFAULT_RECURSION_DEPTH - 1);
      } catch (Throwable t) {
        ok = false;
        System.out.println("      (" + t + ")");
      }
      check(ok, "nesting within the limit is still skipped");
    }

    {
      boolean depthLimit = false;
      try {
        skipNested(TProtocol.DEFAULT_RECURSION_DEPTH + 2);
      } catch (TProtocolException e) {
        depthLimit = e.getType() == TProtocolException.DEPTH_LIMIT;
      }
      check(depthLimit, "nesting just past the limit is refused");
    }

    System.out.println("\n" + checks + " checks, " + failures + " failures");
    if (failures > 0) {
      System.exit(1);
    }
  }
}
