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
    // A container header declares an element count that the reader multiplies by
    // the per-element minimum serialized size and checks against the message-size
    // budget. The product is computed in 64 bits so a large count cannot reduce it
    // (mod 2^32) to a small value that would pass the budget check.
    [TestClass]
    public class TProtocolContainerSizeTests
    {
        public enum ProtocolKind { Binary, Compact, Json }

        // 0x20000000 * 8 (min serialized size of a DOUBLE) == 1<<32, i.e. 0 when
        // taken as a signed 32-bit product; the map product (x16) is likewise.
        private const int OverflowCount = 0x20000000;

        private static TProtocol MakeProtocol(ProtocolKind kind, TTransport transport) => kind switch
        {
            ProtocolKind.Binary => new TBinaryProtocol(transport),
            ProtocolKind.Compact => new TCompactProtocol(transport),
            ProtocolKind.Json => new TJsonProtocol(transport),
            _ => throw new ArgumentOutOfRangeException(nameof(kind)),
        };

        private static async Task<MemoryStream> WriteHeaderAsync(ProtocolKind kind, Func<TProtocol, Task> write)
        {
            var stream = new MemoryStream();
            var trans = new TStreamTransport(null, stream, new TConfiguration());
            await write(MakeProtocol(kind, trans));
            await trans.FlushAsync(default);
            stream.Position = 0;
            return stream;
        }

        private static async Task AssertOversizedRejectedAsync(MemoryStream buffer, ProtocolKind kind, Func<TProtocol, Task> read)
        {
            buffer.Position = 0;
            var proto = MakeProtocol(kind, new TStreamTransport(buffer, null, new TConfiguration()));
            try
            {
                await read(proto);
                Assert.Fail("Expected the oversized container header to be rejected, but it was accepted");
            }
            catch (TTransportException ex)
            {
                Assert.AreEqual(TTransportException.ExceptionType.MessageSizeLimit, ex.Type);
            }
        }

        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task ReadListBegin_OversizedCount_Rejected(ProtocolKind kind)
        {
            using var buffer = await WriteHeaderAsync(kind, p =>
                p.WriteListBeginAsync(new TList(TType.Double, OverflowCount), default));
            await AssertOversizedRejectedAsync(buffer, kind, async p => await p.ReadListBeginAsync(default));
        }

        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task ReadSetBegin_OversizedCount_Rejected(ProtocolKind kind)
        {
            using var buffer = await WriteHeaderAsync(kind, p =>
                p.WriteSetBeginAsync(new TSet(TType.Double, OverflowCount), default));
            await AssertOversizedRejectedAsync(buffer, kind, async p => await p.ReadSetBeginAsync(default));
        }

        [TestMethod]
        [DataRow(ProtocolKind.Binary)]
        [DataRow(ProtocolKind.Compact)]
        [DataRow(ProtocolKind.Json)]
        public async Task ReadMapBegin_OversizedCount_Rejected(ProtocolKind kind)
        {
            using var buffer = await WriteHeaderAsync(kind, p =>
                p.WriteMapBeginAsync(new TMap(TType.Double, TType.Double, OverflowCount), default));
            await AssertOversizedRejectedAsync(buffer, kind, async p => await p.ReadMapBeginAsync(default));
        }
    }
}
