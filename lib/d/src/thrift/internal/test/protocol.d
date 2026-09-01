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
module thrift.internal.test.protocol;

import std.exception;
import thrift.transport.memory;
import thrift.protocol.base;

version (unittest):

void testContainerSizeLimit(Protocol)() if (isTProtocol!Protocol) {
  auto buffer = new TMemoryBuffer;
  auto prot = new Protocol(buffer);

  // Make sure reading fails if a container larger than the size limit is read.
  prot.containerSizeLimit = 3;

  {
    prot.writeListBegin(TList(TType.I32, 4));
    prot.writeI32(0); // Make sure size can be read e.g. for JSON protocol.
    prot.reset();

    auto e = cast(TProtocolException)collectException(prot.readListBegin());
    enforce(e && e.type == TProtocolException.Type.SIZE_LIMIT);
    prot.reset();
    buffer.reset();
  }

  {
    prot.writeMapBegin(TMap(TType.I32, TType.I32, 4));
    prot.writeI32(0); // Make sure size can be read e.g. for JSON protocol.
    prot.reset();

    auto e = cast(TProtocolException)collectException(prot.readMapBegin());
    enforce(e && e.type == TProtocolException.Type.SIZE_LIMIT);
    prot.reset();
    buffer.reset();
  }

  {
    prot.writeSetBegin(TSet(TType.I32, 4));
    prot.writeI32(0); // Make sure size can be read e.g. for JSON protocol.
    prot.reset();

    auto e = cast(TProtocolException)collectException(prot.readSetBegin());
    enforce(e && e.type == TProtocolException.Type.SIZE_LIMIT);
    prot.reset();
    buffer.reset();
  }

  // Make sure reading works if the containers are smaller than the limit or
  // no limit is set.
  foreach (limit; [3, 0, -1]) {
    prot.containerSizeLimit = limit;

    {
      prot.writeListBegin(TList(TType.I32, 2));
      prot.writeI32(0);
      prot.writeI32(1);
      prot.writeListEnd();
      prot.reset();

      auto list = prot.readListBegin();
      enforce(list.elemType == TType.I32);
      enforce(list.size == 2);
      enforce(prot.readI32() == 0);
      enforce(prot.readI32() == 1);
      prot.readListEnd();

      prot.reset();
      buffer.reset();
    }

    {
      prot.writeMapBegin(TMap(TType.I32, TType.I32, 2));
      prot.writeI32(0);
      prot.writeI32(1);
      prot.writeI32(2);
      prot.writeI32(3);
      prot.writeMapEnd();
      prot.reset();

      auto map = prot.readMapBegin();
      enforce(map.keyType == TType.I32);
      enforce(map.valueType == TType.I32);
      enforce(map.size == 2);
      enforce(prot.readI32() == 0);
      enforce(prot.readI32() == 1);
      enforce(prot.readI32() == 2);
      enforce(prot.readI32() == 3);
      prot.readMapEnd();

      prot.reset();
      buffer.reset();
    }

    {
      prot.writeSetBegin(TSet(TType.I32, 2));
      prot.writeI32(0);
      prot.writeI32(1);
      prot.writeSetEnd();
      prot.reset();

      auto set = prot.readSetBegin();
      enforce(set.elemType == TType.I32);
      enforce(set.size == 2);
      enforce(prot.readI32() == 0);
      enforce(prot.readI32() == 1);
      prot.readSetEnd();

      prot.reset();
      buffer.reset();
    }
  }
}

/*
 * A protocol built with its default arguments must carry a usable limit. They
 * used to default to zero, which readSize() reads as "no limit", so the checks
 * above them could not fire unless a caller had set one -- and the size went
 * straight into uninitializedArray.
 *
 * Only the defaults are checked here; that the checks themselves work is
 * testStringSizeLimit/testContainerSizeLimit's job, and they are wire-format
 * agnostic in a way a raw declared length cannot be.
 */
void testSizeLimitDefaults(Protocol)() if (isTProtocol!Protocol) {
  auto buffer = new TMemoryBuffer;
  auto prot = new Protocol(buffer);

  enforce(prot.stringSizeLimit > 0,
    "string size limit should have a usable default");
  enforce(prot.containerSizeLimit > 0,
    "container size limit should have a usable default");

  // An ordinary payload still reads with the defaults in force.
  prot.writeString("still fine");
  prot.reset();
  enforce(prot.readString() == "still fine");
  prot.reset();
  buffer.reset();
}

void testStringSizeLimit(Protocol)() if (isTProtocol!Protocol) {
  auto buffer = new TMemoryBuffer;
  auto prot = new Protocol(buffer);

  // Make sure reading fails if a string larger than the size limit is read.
  prot.stringSizeLimit = 3;

  {
    prot.writeString("asdf");
    prot.reset();

    auto e = cast(TProtocolException)collectException(prot.readString());
    enforce(e && e.type == TProtocolException.Type.SIZE_LIMIT);
    prot.reset();
    buffer.reset();
  }

  {
    prot.writeBinary([1, 2, 3, 4]);
    prot.reset();

    auto e = cast(TProtocolException)collectException(prot.readBinary());
    enforce(e && e.type == TProtocolException.Type.SIZE_LIMIT);
    prot.reset();
    buffer.reset();
  }

  // Make sure reading works if the containers are smaller than the limit or
  // no limit is set.
  foreach (limit; [3, 0, -1]) {
    prot.containerSizeLimit = limit;

    {
      prot.writeString("as");
      prot.reset();

      enforce(prot.readString() == "as");
      prot.reset();
      buffer.reset();
    }

    {
      prot.writeBinary([1, 2]);
      prot.reset();

      enforce(prot.readBinary() == [1, 2]);
      prot.reset();
      buffer.reset();
    }
  }
}

/*
 * skip() descends through nested structs and containers exactly the way
 * reading them does, so it has to draw on the same recursion budget: a payload
 * nested one level past the limit must be rejected instead of running the call
 * stack down.
 *
 * The payloads are written with the raw protocol primitives, which carry no
 * guard of their own -- writeStruct() would refuse to emit an over-deep chain
 * in the first place.
 */
void testSkipDepthLimit(Protocol)() if (isTProtocol!Protocol) {
  // Keep the test hermetic with respect to the thread-local counter.
  uint savedDepth = currentRecursionDepth_;
  scope(exit) currentRecursionDepth_ = savedDepth;
  currentRecursionDepth_ = 0;

  // Writes a chain of `depth` nested structs, each holding the next as field 1.
  static void writeStructChain(Protocol prot, uint depth) {
    prot.writeStructBegin(TStruct("Chain"));
    if (depth > 1) {
      prot.writeFieldBegin(TField("next", TType.STRUCT, 1));
      writeStructChain(prot, depth - 1);
      prot.writeFieldEnd();
    }
    prot.writeFieldStop();
    prot.writeStructEnd();
  }

  // Writes a chain of `depth` nested one-element lists. The innermost one is
  // empty so that the chain costs exactly `depth` levels and not one more.
  static void writeListChain(Protocol prot, uint depth) {
    if (depth > 1) {
      prot.writeListBegin(TList(TType.LIST, 1));
      writeListChain(prot, depth - 1);
      prot.writeListEnd();
    } else {
      prot.writeListBegin(TList(TType.I32, 0));
      prot.writeListEnd();
    }
  }

  // Writes a payload into a fresh buffer, then skips it back off as `type`.
  static void skipPayload(scope void delegate(Protocol) write, TType type) {
    auto buffer = new TMemoryBuffer;
    auto prot = new Protocol(buffer);
    write(prot);
    prot.reset();
    skip(prot, type);
  }

  // A chain exactly at the limit is skipped (off-by-one guard), one level
  // deeper is rejected with DEPTH_LIMIT. Both must leave the counter unwound.
  {
    skipPayload((Protocol p) { writeStructChain(p, DEFAULT_MAX_RECURSION_DEPTH); },
      TType.STRUCT);
    enforce(currentRecursionDepth_ == 0, "counter must unwind to 0");

    auto e = cast(TProtocolException)collectException(
      skipPayload((Protocol p) { writeStructChain(p, DEFAULT_MAX_RECURSION_DEPTH + 1); },
        TType.STRUCT));
    enforce(e && e.type == TProtocolException.Type.DEPTH_LIMIT,
      "skipping a struct chain past the limit must throw DEPTH_LIMIT");
    enforce(currentRecursionDepth_ == 0, "counter must unwind after a throw");
  }

  // Containers recurse through the same skip(), so they are bounded alike.
  {
    skipPayload((Protocol p) { writeListChain(p, DEFAULT_MAX_RECURSION_DEPTH); },
      TType.LIST);
    enforce(currentRecursionDepth_ == 0, "counter must unwind to 0");

    auto e = cast(TProtocolException)collectException(
      skipPayload((Protocol p) { writeListChain(p, DEFAULT_MAX_RECURSION_DEPTH + 1); },
        TType.LIST));
    enforce(e && e.type == TProtocolException.Type.DEPTH_LIMIT,
      "skipping a list chain past the limit must throw DEPTH_LIMIT");
    enforce(currentRecursionDepth_ == 0, "counter must unwind after a throw");
  }

  // Decrement regression guard: a wide, shallow struct holding far more fields
  // than the limit is only one level deep and must still skip cleanly. This
  // holds just if the counter is decremented as each field unwinds.
  skipPayload((Protocol prot) {
    prot.writeStructBegin(TStruct("Wide"));
    foreach (i; 1 .. DEFAULT_MAX_RECURSION_DEPTH * 3) {
      prot.writeFieldBegin(TField("f", TType.I32, cast(short)i));
      prot.writeI32(cast(int)i);
      prot.writeFieldEnd();
    }
    prot.writeFieldStop();
    prot.writeStructEnd();
  }, TType.STRUCT);
  enforce(currentRecursionDepth_ == 0, "counter must unwind to 0");
}
