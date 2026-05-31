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
using Thrift.Protocol.Entities;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Protocols
{
    // Exercises the recursion-depth limit through the *generated* struct read/write
    // path (TBase.WriteAsync / TBase.ReadAsync), i.e. the real code path a malicious
    // payload would hit -- not the protocol's Write/ReadStructBegin methods in isolation.
    //
    // The recursive IDL types come from test/Recursive.thrift, generated into the
    // Thrift.Compile.net10 test assembly, and cover all three struct-like kinds:
    //   * struct    -- CoRec <-> CoRec2 (mutually recursive), RecTree (wide tree)
    //   * exception -- CoError <-> CoError2 (mutually recursive)
    //   * union     -- CoUnion <-> CoUnion2 (mutually recursive)
    // The generator emits the same IncrementRecursionDepth/DecrementRecursionDepth
    // guard for all three, so the limit must hold regardless of kind.
    [TestClass]
    public class TProtocolRecursionDepthTests
    {
        private const int TestRecursionLimit = 5;

        public enum ProtocolKind { Binary, Compact, Json }

        // The round-trippable kinds. Unions are excluded here and tested separately
        // (see the Union_* methods): a CoUnion/CoUnion2 has no scalar leaf -- its only
        // field is the recursive one -- so every terminal is the "unset" state, which
        // throws on write. No finite union value can be serialized, hence there is no
        // at-limit round-trip; only the over-limit rejection is observable.
        public enum ChainKind { Struct, Exception }

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

        // CoError <-> CoError2 mirrors CoRec exactly: 'other' is optional, so the
        // chain terminates with a null leaf.
        private static CoError? MakeNestedErrors(int depth)
        {
            return (depth > 0)
                ? new CoError { Other = MakeNestedCoError2(depth - 1) }
                : null;
        }

        private static CoError2? MakeNestedCoError2(int depth)
        {
            return (depth > 0)
                ? new CoError2 { Other = MakeNestedErrors(depth - 1) }
                : null;
        }

        // Build a 'depth'-deep chain of the requested round-trippable kind.
        private static TBase MakeChain(ChainKind chain, int depth) => chain switch
        {
            ChainKind.Struct => MakeNestedRecs(depth)!,
            ChainKind.Exception => MakeNestedErrors(depth)!,
            _ => throw new ArgumentOutOfRangeException(nameof(chain)),
        };

        // A fresh, empty instance to read into.
        private static TBase NewEmpty(ChainKind chain) => chain switch
        {
            ChainKind.Struct => new CoRec(),
            ChainKind.Exception => new CoError(),
            _ => throw new ArgumentOutOfRangeException(nameof(chain)),
        };

        // A two-node chain closed into a cycle: node -> other -> node -> ...
        private static TBase MakeCyclic(ChainKind chain)
        {
            switch (chain)
            {
                case ChainKind.Struct:
                    var rec = MakeNestedRecs(2)!;
                    rec.Other!.Other = rec;
                    return rec;
                case ChainKind.Exception:
                    var err = MakeNestedErrors(2)!;
                    err.Other!.Other = err;
                    return err;
                default:
                    throw new ArgumentOutOfRangeException(nameof(chain));
            }
        }

        // CoUnion <-> CoUnion2: every terminal is the unset state, which is unwritable
        // (see ChainKind). Only used to drive over-limit writes, where the guard fires
        // before the terminal is ever reached.
        private static CoUnion MakeNestedUnion(int depth)
            => depth > 0 ? new CoUnion.@other(MakeNestedCoUnion2(depth - 1)) : new CoUnion.___undefined();

        private static CoUnion2 MakeNestedCoUnion2(int depth)
            => depth > 0 ? new CoUnion2.@other(MakeNestedUnion(depth - 1)) : new CoUnion2.___undefined();

        // Serialize 'data' into a fresh buffer using a protocol configured with the given
        // recursion limit, and return the rewound buffer ready for reading. Takes the
        // TUnionBase write interface so structs, exceptions and unions all flow through.
        private static async Task<MemoryStream> WriteToBuffer(TUnionBase data, ProtocolKind kind, int recursionLimit)
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
        [DataRow(ProtocolKind.Binary, ChainKind.Struct)]
        [DataRow(ProtocolKind.Compact, ChainKind.Struct)]
        [DataRow(ProtocolKind.Json, ChainKind.Struct)]
        [DataRow(ProtocolKind.Binary, ChainKind.Exception)]
        [DataRow(ProtocolKind.Compact, ChainKind.Exception)]
        [DataRow(ProtocolKind.Json, ChainKind.Exception)]
        public async Task RoundTrip_BelowLimit_Succeeds(ProtocolKind kind, ChainKind chain)
        {
            var data = MakeChain(chain, TestRecursionLimit - 1);
            using var buffer = await WriteToBuffer(data, kind, TestRecursionLimit);
            await ReadFromBuffer(NewEmpty(chain), buffer, kind, TestRecursionLimit);
        }

        // A chain exactly at the limit must still round-trip (off-by-one guard, and
        // proves the guard does not double-count).
        [TestMethod]
        [DataRow(ProtocolKind.Binary, ChainKind.Struct)]
        [DataRow(ProtocolKind.Compact, ChainKind.Struct)]
        [DataRow(ProtocolKind.Json, ChainKind.Struct)]
        [DataRow(ProtocolKind.Binary, ChainKind.Exception)]
        [DataRow(ProtocolKind.Compact, ChainKind.Exception)]
        [DataRow(ProtocolKind.Json, ChainKind.Exception)]
        public async Task RoundTrip_AtLimit_Succeeds(ProtocolKind kind, ChainKind chain)
        {
            var data = MakeChain(chain, TestRecursionLimit);
            using var buffer = await WriteToBuffer(data, kind, TestRecursionLimit);
            await ReadFromBuffer(NewEmpty(chain), buffer, kind, TestRecursionLimit);
        }

        // Writing a chain one level over the limit must be rejected.
        [TestMethod]
        [DataRow(ProtocolKind.Binary, ChainKind.Struct)]
        [DataRow(ProtocolKind.Compact, ChainKind.Struct)]
        [DataRow(ProtocolKind.Json, ChainKind.Struct)]
        [DataRow(ProtocolKind.Binary, ChainKind.Exception)]
        [DataRow(ProtocolKind.Compact, ChainKind.Exception)]
        [DataRow(ProtocolKind.Json, ChainKind.Exception)]
        public async Task Write_AboveLimit_Throws(ProtocolKind kind, ChainKind chain)
        {
            var data = MakeChain(chain, TestRecursionLimit + 1);
            await AssertDepthLimitAsync(() => WriteToBuffer(data, kind, TestRecursionLimit));
        }

        // Reading a too-deep payload must be rejected. The payload is produced with a
        // higher limit so that valid bytes exist on the wire, then read back with the
        // real limit -- mimicking a hostile message from the network.
        [TestMethod]
        [DataRow(ProtocolKind.Binary, ChainKind.Struct)]
        [DataRow(ProtocolKind.Compact, ChainKind.Struct)]
        [DataRow(ProtocolKind.Json, ChainKind.Struct)]
        [DataRow(ProtocolKind.Binary, ChainKind.Exception)]
        [DataRow(ProtocolKind.Compact, ChainKind.Exception)]
        [DataRow(ProtocolKind.Json, ChainKind.Exception)]
        public async Task Read_AboveLimit_Throws(ProtocolKind kind, ChainKind chain)
        {
            var data = MakeChain(chain, TestRecursionLimit + 1);
            using var buffer = await WriteToBuffer(data, kind, TestRecursionLimit + 1);
            await AssertDepthLimitAsync(() => ReadFromBuffer(NewEmpty(chain), buffer, kind, TestRecursionLimit));
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
        [DataRow(ProtocolKind.Binary, ChainKind.Struct)]
        [DataRow(ProtocolKind.Compact, ChainKind.Struct)]
        [DataRow(ProtocolKind.Json, ChainKind.Struct)]
        [DataRow(ProtocolKind.Binary, ChainKind.Exception)]
        [DataRow(ProtocolKind.Compact, ChainKind.Exception)]
        [DataRow(ProtocolKind.Json, ChainKind.Exception)]
        public async Task CyclicGraph_Throws(ProtocolKind kind, ChainKind chain)
        {
            var data = MakeCyclic(chain);
            await AssertDepthLimitAsync(() => WriteToBuffer(data, kind, TestRecursionLimit));
        }

        // Writing a union chain past the limit is rejected before the (unwritable)
        // unset terminal is ever reached.
        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task Union_Write_AboveLimit_Throws(ProtocolKind kind)
        {
            var data = MakeNestedUnion(TestRecursionLimit + 5);
            await AssertDepthLimitAsync(() => WriteToBuffer(data, kind, TestRecursionLimit));
        }

        // Reading a too-deep union payload must be rejected. Craft it with raw protocol
        // primitives -- a direct STRUCT field (id 1 = 'other'), NOT a list -- so the
        // reader recurses through the guarded CoUnion.ReadAsync and not the unbounded
        // TProtocolUtil.SkipAsync branch a non-Struct field type would take.
        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task Union_Read_AboveLimit_Throws(ProtocolKind kind)
        {
            var config = new TConfiguration { RecursionLimit = TestRecursionLimit };
            using var stream = new MemoryStream();

            var writeTrans = new TStreamTransport(null, stream, config);
            var writeProto = MakeProtocol(kind, writeTrans);
            await WriteDeepUnion(writeProto, TestRecursionLimit + 5);
            await writeTrans.FlushAsync(default);
            stream.Position = 0;

            var readTrans = new TStreamTransport(stream, null, config);
            var readProto = MakeProtocol(kind, readTrans);
            await AssertDepthLimitAsync(() => CoUnion.ReadAsync(readProto, default));
        }

        // Emit 'depth' nested CoUnion structs by hand (field id 1 / TType.Struct), with
        // no recursion guard on the write side, so the reader is what trips the limit.
        private static async Task WriteDeepUnion(TProtocol proto, int depth)
        {
            await proto.WriteStructBeginAsync(new TStruct("CoUnion"), default);
            if (depth > 1)
            {
                await proto.WriteFieldBeginAsync(new TField { Name = "other", Type = TType.Struct, ID = 1 }, default);
                await WriteDeepUnion(proto, depth - 1);
                await proto.WriteFieldEndAsync(default);
            }
            await proto.WriteFieldStopAsync(default);
            await proto.WriteStructEndAsync(default);
        }
    }
}
