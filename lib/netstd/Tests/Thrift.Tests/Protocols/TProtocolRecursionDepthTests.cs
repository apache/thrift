// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Protocols
{
    // Exercises the recursion-depth limit through the *generated* struct read/write
    // path (TBase.WriteAsync / TBase.ReadAsync), i.e. the real code path a malicious
    // payload would hit -- not the protocol's Write/ReadStructBegin methods in isolation.
    //
    // The recursive IDL types (CoRec / CoRec2 / RecTree) come from test/Recursive.thrift,
    // generated into the Thrift.Compile.net10 test assembly. CoRec <-> CoRec2 form a
    // mutually recursive chain; RecTree is a wide tree of nested structs.
    [TestClass]
    public class TProtocolRecursionDepthTests
    {
        private const int TestRecursionLimit = 5;

        public enum ProtocolKind { Binary, Compact, Json }

        private static TProtocol MakeProtocol(ProtocolKind kind, TTransport transport) => kind switch
        {
            ProtocolKind.Binary => new TBinaryProtocol(transport),
            ProtocolKind.Compact => new TCompactProtocol(transport),
            ProtocolKind.Json => new TJsonProtocol(transport),
            _ => throw new ArgumentOutOfRangeException(nameof(kind)),
        };

        // Build a CoRec/CoRec2 chain that is exactly 'depth' structs deep.
        private static CoRec? MakeNestedRecs(int depth)
        {
            return (depth > 0)
                ? new CoRec { Other = MakeNestedCoRec2(depth - 1) }
                : null;
        }

        private static CoRec2? MakeNestedCoRec2(int depth)
        {
            return (depth > 0)
                ? new CoRec2 { Other = MakeNestedRecs(depth - 1) }
                : null;
        }

        // Serialize 'data' into a fresh buffer using a protocol configured with the given
        // recursion limit, and return the rewound buffer ready for reading.
        private static async Task<MemoryStream> WriteToBuffer(TBase data, ProtocolKind kind, int recursionLimit)
        {
            var stream = new MemoryStream();
            var config = new TConfiguration { RecursionLimit = recursionLimit };
            var trans = new TStreamTransport(null, stream, config);
            var proto = MakeProtocol(kind, trans);
            await data.WriteAsync(proto, default);
            await trans.FlushAsync(default);
            stream.Position = 0;
            return stream;
        }

        private static async Task ReadFromBuffer(TBase into, MemoryStream buffer, ProtocolKind kind, int recursionLimit)
        {
            buffer.Position = 0;
            var config = new TConfiguration { RecursionLimit = recursionLimit };
            var trans = new TStreamTransport(buffer, null, config);
            var proto = MakeProtocol(kind, trans);
            await into.ReadAsync(proto, default);
        }

        private static async Task AssertDepthLimitAsync(Func<Task> action)
        {
            try
            {
                await action();
                Assert.Fail("Expected TProtocolException(DEPTH_LIMIT) was not thrown");
            }
            catch (TProtocolException ex)
            {
                Assert.AreEqual(TProtocolException.DEPTH_LIMIT, ex.GetExceptionType());
            }
        }

        // A chain one level below the limit must round-trip cleanly.
        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task RoundTrip_BelowLimit_Succeeds(ProtocolKind kind)
        {
            var data = MakeNestedRecs(TestRecursionLimit - 1)!;
            using var buffer = await WriteToBuffer(data, kind, TestRecursionLimit);
            await ReadFromBuffer(new CoRec(), buffer, kind, TestRecursionLimit);
        }

        // A chain exactly at the limit must still round-trip (off-by-one guard).
        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task RoundTrip_AtLimit_Succeeds(ProtocolKind kind)
        {
            var data = MakeNestedRecs(TestRecursionLimit)!;
            using var buffer = await WriteToBuffer(data, kind, TestRecursionLimit);
            await ReadFromBuffer(new CoRec(), buffer, kind, TestRecursionLimit);
        }

        // Writing a chain one level over the limit must be rejected.
        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task Write_AboveLimit_Throws(ProtocolKind kind)
        {
            var data = MakeNestedRecs(TestRecursionLimit + 1)!;
            await AssertDepthLimitAsync(() => WriteToBuffer(data, kind, TestRecursionLimit));
        }

        // Reading a too-deep payload must be rejected. The payload is produced with a
        // higher limit so that valid bytes exist on the wire, then read back with the
        // real limit -- mimicking a hostile message from the network.
        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task Read_AboveLimit_Throws(ProtocolKind kind)
        {
            var data = MakeNestedRecs(TestRecursionLimit + 1)!;
            using var buffer = await WriteToBuffer(data, kind, TestRecursionLimit + 1);
            await AssertDepthLimitAsync(() => ReadFromBuffer(new CoRec(), buffer, kind, TestRecursionLimit));
        }

        // Decrement regression guard: a *wide* (shallow) tree whose total number of
        // struct-begins far exceeds the limit must still round-trip. This only holds
        // if DecrementRecursionDepth correctly unwinds each sibling back to depth 1.
        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task RoundTrip_WideStructure_Succeeds(ProtocolKind kind)
        {
            var tree = new RecTree { Item = 0, Children = new List<RecTree>() };
            for (var i = 0; i < (TestRecursionLimit * 3); i++)
                tree.Children.Add(new RecTree { Item = (short)i, Children = new List<RecTree>() });

            using var buffer = await WriteToBuffer(tree, kind, TestRecursionLimit);
            await ReadFromBuffer(new RecTree(), buffer, kind, TestRecursionLimit);
        }

        // A cyclic object graph would recurse forever without the limit; it must instead
        // fail with DEPTH_LIMIT.
        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task CyclicGraph_Throws(ProtocolKind kind)
        {
            var data = MakeNestedRecs(2)!;   // CoRec -> CoRec2 -> null
            data.Other!.Other = data;        // close the loop: CoRec2.Other -> CoRec
            await AssertDepthLimitAsync(() => WriteToBuffer(data, kind, TestRecursionLimit));
        }
    }
}
