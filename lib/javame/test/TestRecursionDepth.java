/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import java.util.Vector;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.protocol.TStruct;
import org.apache.thrift.protocol.TType;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TTransport;

/**
 * Generated-code round-trip test for the recursion-depth limit in the JavaME
 * library (THRIFT-6055).  It drives the real generated struct read/write path
 * (TBase.read / TBase.write) rather than calling
 * TProtocol.incrementRecursionDepth / decrementRecursionDepth in isolation.
 *
 * The recursive IDL types CoRec / CoRec2 / RecTree (structs) and RecError
 * (exception) come from test/RecursionDepth.thrift (generated into the default
 * package).  That is a JavaME-local copy rather than the shared
 * test/Recursive.thrift: the shared file also defines a recursive union and a
 * service, and JavaME has no TUnion runtime class, so a generated union does
 * not compile.  Structs and exceptions are all JavaME can express, and the
 * guard reaches all of them through the same generated read/write path.  The
 * limit is hard-coded at 64 in TProtocol, so the boundary is: a chain of 64
 * structs round-trips, 65 is rejected with DEPTH_LIMIT.
 *
 * JavaME has no build harness; from the lib/javame directory run the following
 * (the `thrift` compiler must be one built from this branch, so the generated
 * code carries the recursion guard):
 *
 *   thrift --gen javame -o . test/RecursionDepth.thrift
 *   javac -d /tmp/jme -cp . \
 *       $(find src -name '*.java' ! -name 'THttpClient.java') \
 *       gen-javame/*.java test/TestRecursionDepth.java
 *   java -cp /tmp/jme TestRecursionDepth
 *
 * (THttpClient.java is excluded because it needs the javax.microedition API.)
 */
public class TestRecursionDepth {

  private static final int LIMIT = 64; // TProtocol.DEFAULT_RECURSION_DEPTH

  private static final int BINARY = 0;
  private static final int JSON = 1;

  private static int checks = 0;
  private static int failures = 0;

  // ---- helpers --------------------------------------------------------------

  private static TProtocol proto(int kind, TTransport t) {
    return (kind == JSON) ? (TProtocol) new TJSONProtocol(t)
                          : (TProtocol) new TBinaryProtocol(t);
  }

  private static String kindName(int kind) {
    return (kind == JSON) ? "json" : "binary";
  }

  // Build a CoRec/CoRec2 chain that is exactly 'depth' structs deep.
  private static CoRec makeNestedRecs(int depth) {
    if (depth <= 0) {
      return null;
    }
    CoRec c = new CoRec();
    c.setOther(makeNestedCoRec2(depth - 1));
    return c;
  }

  private static CoRec2 makeNestedCoRec2(int depth) {
    if (depth <= 0) {
      return null;
    }
    CoRec2 c = new CoRec2();
    c.setOther(makeNestedRecs(depth - 1));
    return c;
  }

  // Build a self-recursive RecError exception chain that is exactly 'depth' deep.
  private static RecError makeNestedError(int depth) {
    if (depth <= 0) {
      return null;
    }
    RecError e = new RecError();
    e.setOther(makeNestedError(depth - 1));
    return e;
  }

  // Craft a 'depth'-deep nested-struct payload with raw protocol primitives,
  // bypassing the recursion counter (which lives in the generated write()).
  // This is the only way to obtain an over-limit payload, since a normal
  // write() of such a chain would itself be rejected at the limit.
  private static void emit(TProtocol p, String name, int depth) throws Exception {
    p.writeStructBegin(new TStruct(name));
    if (depth > 1) {
      p.writeFieldBegin(new TField("other", TType.STRUCT, (short) 1));
      emit(p, name, depth - 1);
      p.writeFieldEnd();
    }
    p.writeFieldStop();
    p.writeStructEnd();
  }

  private static boolean isDepthLimit(Throwable e) {
    return (e instanceof TProtocolException)
        && ((TProtocolException) e).getType() == TProtocolException.DEPTH_LIMIT;
  }

  private static void check(boolean ok, String msg) {
    checks++;
    if (ok) {
      System.out.println("  pass: " + msg);
    } else {
      failures++;
      System.out.println("  FAIL: " + msg);
    }
  }

  // ---- test cases, run for each protocol ------------------------------------

  private static void roundTripAtAndBelowLimit(int kind) {
    try {
      TMemoryBuffer buf = new TMemoryBuffer(1024);
      makeNestedRecs(LIMIT - 1).write(proto(kind, buf));
      new CoRec().read(proto(kind, buf));
      check(true, kindName(kind) + ": chain one below limit round-trips");
    } catch (Throwable e) {
      check(false, kindName(kind) + ": chain one below limit round-trips (threw " + e + ")");
    }
    try {
      TMemoryBuffer buf = new TMemoryBuffer(1024);
      makeNestedRecs(LIMIT).write(proto(kind, buf));
      new CoRec().read(proto(kind, buf));
      check(true, kindName(kind) + ": chain exactly at limit round-trips");
    } catch (Throwable e) {
      check(false, kindName(kind) + ": chain exactly at limit round-trips (threw " + e + ")");
    }
  }

  private static void writeOverLimitRejected(int kind) {
    try {
      TMemoryBuffer buf = new TMemoryBuffer(1024);
      makeNestedRecs(LIMIT + 1).write(proto(kind, buf));
      check(false, kindName(kind) + ": write one over limit must throw DEPTH_LIMIT");
    } catch (Throwable e) {
      check(isDepthLimit(e), kindName(kind) + ": write one over limit throws DEPTH_LIMIT (got " + e + ")");
    }
  }

  private static void readOverLimitRejected(int kind) {
    try {
      TMemoryBuffer buf = new TMemoryBuffer(1024);
      emit(proto(kind, buf), "CoRec", LIMIT + 1); // craft a 65-deep payload
      new CoRec().read(proto(kind, buf));
      check(false, kindName(kind) + ": read one over limit must throw DEPTH_LIMIT");
    } catch (Throwable e) {
      check(isDepthLimit(e), kindName(kind) + ": read one over limit throws DEPTH_LIMIT (got " + e + ")");
    }
  }

  private static void wideStructureRoundTrips(int kind) {
    try {
      RecTree tree = new RecTree();
      tree.setItem((short) 0);
      Vector kids = new Vector();
      for (int i = 0; i < LIMIT * 3; i++) {
        RecTree leaf = new RecTree();
        leaf.setItem((short) i);
        leaf.setChildren(new Vector());
        kids.addElement(leaf);
      }
      tree.setChildren(kids);

      TMemoryBuffer buf = new TMemoryBuffer(4096);
      tree.write(proto(kind, buf));
      new RecTree().read(proto(kind, buf));
      check(true, kindName(kind) + ": wide shallow tree round-trips (decrement unwinds each sibling)");
    } catch (Throwable e) {
      check(false, kindName(kind) + ": wide shallow tree round-trips (threw " + e + ")");
    }
  }

  private static void cyclicGraphRejected(int kind) {
    try {
      CoRec data = makeNestedRecs(2); // CoRec -> CoRec2 -> null
      data.getOther().setOther(data); // close the loop: CoRec2.other -> CoRec
      TMemoryBuffer buf = new TMemoryBuffer(1024);
      data.write(proto(kind, buf));
      check(false, kindName(kind) + ": cyclic graph must throw DEPTH_LIMIT");
    } catch (Throwable e) {
      check(isDepthLimit(e), kindName(kind) + ": cyclic graph throws DEPTH_LIMIT (got " + e + ")");
    }
  }

  // The same bound must apply to recursive exceptions, which are generated
  // through the same read/write path as structs.
  private static void exceptionRoundTripAtLimit(int kind) {
    try {
      TMemoryBuffer buf = new TMemoryBuffer(1024);
      makeNestedError(LIMIT).write(proto(kind, buf));
      new RecError().read(proto(kind, buf));
      check(true, kindName(kind) + ": exception chain at limit round-trips");
    } catch (Throwable e) {
      check(false, kindName(kind) + ": exception chain at limit round-trips (threw " + e + ")");
    }
  }

  private static void exceptionWriteOverLimitRejected(int kind) {
    try {
      TMemoryBuffer buf = new TMemoryBuffer(1024);
      makeNestedError(LIMIT + 1).write(proto(kind, buf));
      check(false, kindName(kind) + ": exception write one over limit must throw DEPTH_LIMIT");
    } catch (Throwable e) {
      check(isDepthLimit(e), kindName(kind) + ": exception write one over limit throws DEPTH_LIMIT (got " + e + ")");
    }
  }

  private static void exceptionReadOverLimitRejected(int kind) {
    try {
      TMemoryBuffer buf = new TMemoryBuffer(1024);
      emit(proto(kind, buf), "RecError", LIMIT + 1); // craft a 65-deep payload
      new RecError().read(proto(kind, buf));
      check(false, kindName(kind) + ": exception read one over limit must throw DEPTH_LIMIT");
    } catch (Throwable e) {
      check(isDepthLimit(e), kindName(kind) + ": exception read one over limit throws DEPTH_LIMIT (got " + e + ")");
    }
  }

  public static void main(String[] args) {
    int[] kinds = {BINARY, JSON};
    for (int k = 0; k < kinds.length; k++) {
      int kind = kinds[k];
      System.out.println(kindName(kind) + " protocol:");
      roundTripAtAndBelowLimit(kind);
      writeOverLimitRejected(kind);
      readOverLimitRejected(kind);
      wideStructureRoundTrips(kind);
      cyclicGraphRejected(kind);
      exceptionRoundTripAtLimit(kind);
      exceptionWriteOverLimitRejected(kind);
      exceptionReadOverLimitRejected(kind);
    }
    System.out.println(checks + " checks, " + failures + " failure(s)");
    if (failures != 0) {
      throw new RuntimeException(failures + " recursion-depth check(s) failed");
    }
  }
}
