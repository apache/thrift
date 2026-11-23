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

/**
 * An implementation of the mini serialization benchmark also available for
 * C++ and Java.
 *
 * For meaningful results, you might want to make sure that
 * the Thrift library is compiled with release build flags,
 * e.g. by including the source files with the build instead
 * of linking libthriftd:
 *
   dmd -w -O -release -inline -I../src -Igen-d -ofserialization_benchmark \
   $(find ../src/thrift -name '*.d' -not -name index.d) \
   gen-d/DebugProtoTest_types.d serialization_benchmark.d
 */
module serialization_benchmark;

import std.datetime.stopwatch : AutoStart, StopWatch;
import std.math : PI;
import std.stdio;
import thrift.protocol.binary;
import thrift.transport.memory;
import thrift.transport.range;
import DebugProtoTest_types;

void main() {
  auto buf = new TMemoryBuffer;
  enum ITERATIONS = 10_000_000;

  {
    auto ooe = OneOfEach();
    ooe.im_true   = true;
    ooe.im_false  = false;
    ooe.a_bite    = 0x7f;
    ooe.integer16 = 27_000;
    ooe.integer32 = 1 << 24;
    ooe.integer64 = 6_000_000_000;
    ooe.double_precision = PI;
    ooe.some_characters = "JSON THIS! \"\1";
    ooe.zomg_unicode = "\xd7\n\a\t";
    ooe.base64 = "\1\2\3\255";

    auto prot = tBinaryProtocol(buf);
    auto sw = StopWatch(AutoStart.yes);
    foreach (i; 0 .. ITERATIONS) {
      buf.reset(120);
      ooe.write(prot);
    }
    sw.stop();

    auto msecs = sw.peek().total!"msecs";
    writefln("Write: %s ms (%s kHz)", msecs, ITERATIONS / msecs);
  }

  auto data = buf.getContents().dup;

  {
    auto readBuf = tInputRangeTransport(data);
    auto prot = tBinaryProtocol(readBuf);
    auto ooe = OneOfEach();

    auto sw = StopWatch(AutoStart.yes);
    foreach (i; 0 .. ITERATIONS) {
      readBuf.reset(data);
      ooe.read(prot);
    }
    sw.stop();

    auto msecs = sw.peek().total!"msecs";
    writefln(" Read: %s ms (%s kHz)", msecs, ITERATIONS / msecs);
  }
}
