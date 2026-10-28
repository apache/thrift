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
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift;
using Thrift.Protocol;
using Thrift.Protocol.Entities;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Protocols
{
    // A length or element count read from the wire is passed to the transport's
    // CheckReadBytesAvailable() before the protocol allocates or reads anything for it,
    // and the endpoint transport rejects a negative size there. TBufferedTransport and
    // TFramedTransport sit between the two and must pass a negative size on unchanged,
    // also while they still hold unread bytes. The string readers reject a negative
    // length themselves, with TProtocolException.NEGATIVE_SIZE.
    [TestClass]
    public class TProtocolNegativeSizeTests
    {
        public enum StackKind { Endpoint, Buffered, Framed }

        // Bytes that follow the payload. On the buffered and framed stacks they are
        // still held unread by the layered transport when the size is checked.
        private const int TrailingBytes = 8;

        // -1 as a binary protocol length or element count (big-endian i32)
        private static readonly byte[] BinaryMinusOne = { 0xFF, 0xFF, 0xFF, 0xFF };

        // -1 as a compact protocol length or element count (a plain varint32, not zigzag)
        private static readonly byte[] CompactMinusOne = { 0xFF, 0xFF, 0xFF, 0xFF, 0x0F };

        // compact list or set header: element count in a following varint (0xF), element type i32 (0x5)
        private const byte CompactI32ListHeader = 0xF5;

        // compact map key and value types, both i32 (0x5)
        private const byte CompactI32MapTypes = 0x55;

        public static IEnumerable<object[]> Stacks => new[]
        {
            new object[] { StackKind.Endpoint, 0 },
            new object[] { StackKind.Buffered, 0 },
            new object[] { StackKind.Buffered, TrailingBytes },
            new object[] { StackKind.Framed, 0 },
            new object[] { StackKind.Framed, TrailingBytes },
        };

        private static byte[] Concat(params byte[][] parts)
        {
            var length = 0;
            foreach (var part in parts)
                length += part.Length;

            var result = new byte[length];
            var offset = 0;
            foreach (var part in parts)
            {
                part.CopyTo(result, offset);
                offset += part.Length;
            }
            return result;
        }

        // The payload and 'trailing' filler bytes over an in-memory endpoint transport.
        // On the framed stack the frame covers the filler bytes too, so that they end up
        // in the framed transport's read buffer.
        private static TTransport MakeTransport(StackKind stack, byte[] payload, int trailing)
        {
            var data = Concat(payload, new byte[trailing]);

            if (stack == StackKind.Framed)
            {
                var frameSize = new byte[4];
                BinaryPrimitives.WriteInt32BigEndian(frameSize, data.Length);
                data = Concat(frameSize, data);
            }

            var endpoint = new TMemoryBufferTransport(data, new TConfiguration());
            return stack switch
            {
                StackKind.Endpoint => endpoint,
                StackKind.Buffered => new TBufferedTransport(endpoint),
                StackKind.Framed => new TFramedTransport(endpoint),
                _ => throw new ArgumentOutOfRangeException(nameof(stack)),
            };
        }

        // The size reaches the endpoint transport, which rejects it.
        private static async Task AssertRejectedByTransportAsync(Func<Task> read)
        {
            try
            {
                await read();
                Assert.Fail("Expected the negative size to be rejected, but it was accepted");
            }
            catch (TTransportException ex)
            {
                Assert.AreEqual(TTransportException.ExceptionType.MessageSizeLimit, ex.Type);
            }
        }

        // The string reader rejects the length itself.
        private static async Task AssertRejectedByProtocolAsync(Func<Task> read)
        {
            try
            {
                await read();
                Assert.Fail("Expected the negative length to be rejected, but it was accepted");
            }
            catch (TProtocolException ex)
            {
                Assert.AreEqual(TProtocolException.NEGATIVE_SIZE, ex.GetExceptionType());
            }
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task BinaryProtocol_ReadBinary_NegativeLength_Rejected(StackKind stack, int trailing)
        {
            var proto = new TBinaryProtocol(MakeTransport(stack, BinaryMinusOne, trailing));
            await AssertRejectedByTransportAsync(async () => await proto.ReadBinaryAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task BinaryProtocol_ReadListBegin_NegativeCount_Rejected(StackKind stack, int trailing)
        {
            var payload = Concat(new[] { (byte)TType.I32 }, BinaryMinusOne);
            var proto = new TBinaryProtocol(MakeTransport(stack, payload, trailing));
            await AssertRejectedByTransportAsync(async () => await proto.ReadListBeginAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task BinaryProtocol_ReadSetBegin_NegativeCount_Rejected(StackKind stack, int trailing)
        {
            var payload = Concat(new[] { (byte)TType.I32 }, BinaryMinusOne);
            var proto = new TBinaryProtocol(MakeTransport(stack, payload, trailing));
            await AssertRejectedByTransportAsync(async () => await proto.ReadSetBeginAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task BinaryProtocol_ReadMapBegin_NegativeCount_Rejected(StackKind stack, int trailing)
        {
            var payload = Concat(new[] { (byte)TType.I32, (byte)TType.I32 }, BinaryMinusOne);
            var proto = new TBinaryProtocol(MakeTransport(stack, payload, trailing));
            await AssertRejectedByTransportAsync(async () => await proto.ReadMapBeginAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task BinaryProtocol_ReadString_NegativeLength_Rejected(StackKind stack, int trailing)
        {
            var proto = new TBinaryProtocol(MakeTransport(stack, BinaryMinusOne, trailing));
            await AssertRejectedByProtocolAsync(async () => await proto.ReadStringAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task CompactProtocol_ReadBinary_NegativeLength_Rejected(StackKind stack, int trailing)
        {
            var proto = new TCompactProtocol(MakeTransport(stack, CompactMinusOne, trailing));
            await AssertRejectedByTransportAsync(async () => await proto.ReadBinaryAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task CompactProtocol_ReadListBegin_NegativeCount_Rejected(StackKind stack, int trailing)
        {
            var payload = Concat(new[] { CompactI32ListHeader }, CompactMinusOne);
            var proto = new TCompactProtocol(MakeTransport(stack, payload, trailing));
            await AssertRejectedByTransportAsync(async () => await proto.ReadListBeginAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task CompactProtocol_ReadSetBegin_NegativeCount_Rejected(StackKind stack, int trailing)
        {
            var payload = Concat(new[] { CompactI32ListHeader }, CompactMinusOne);
            var proto = new TCompactProtocol(MakeTransport(stack, payload, trailing));
            await AssertRejectedByTransportAsync(async () => await proto.ReadSetBeginAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task CompactProtocol_ReadMapBegin_NegativeCount_Rejected(StackKind stack, int trailing)
        {
            var payload = Concat(CompactMinusOne, new[] { CompactI32MapTypes });
            var proto = new TCompactProtocol(MakeTransport(stack, payload, trailing));
            await AssertRejectedByTransportAsync(async () => await proto.ReadMapBeginAsync(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task CompactProtocol_ReadString_NegativeLength_Rejected(StackKind stack, int trailing)
        {
            var proto = new TCompactProtocol(MakeTransport(stack, CompactMinusOne, trailing));
            await AssertRejectedByProtocolAsync(async () => await proto.ReadStringAsync(default));
        }

        // Valid sizes on the same stacks: a string, a binary and a list header with its element.
        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task BinaryProtocol_ValidSizes_Accepted(StackKind stack, int trailing)
        {
            var payload = new byte[]
            {
                0x00, 0x00, 0x00, 0x03, (byte)'a', (byte)'b', (byte)'c',  // string "abc"
                0x00, 0x00, 0x00, 0x02, 0x01, 0x02,                       // binary { 1, 2 }
                (byte)TType.I32, 0x00, 0x00, 0x00, 0x01,                  // list<i32>, one element
                0x00, 0x00, 0x00, 0x2A,                                   // 42
            };
            var proto = new TBinaryProtocol(MakeTransport(stack, payload, trailing));
            Assert.AreEqual("abc", await proto.ReadStringAsync(default));
            CollectionAssert.AreEqual(new byte[] { 0x01, 0x02 }, await proto.ReadBinaryAsync(default));
            Assert.AreEqual(1, (await proto.ReadListBeginAsync(default)).Count);
            Assert.AreEqual(42, await proto.ReadI32Async(default));
        }

        [TestMethod]
        [DynamicData(nameof(Stacks))]
        public async Task CompactProtocol_ValidSizes_Accepted(StackKind stack, int trailing)
        {
            var payload = new byte[]
            {
                0x03, (byte)'a', (byte)'b', (byte)'c',  // string "abc"
                0x02, 0x01, 0x02,                       // binary { 1, 2 }
                0x15,                                   // list<i32>, one element
                0x54,                                   // 42 (zigzag)
            };
            var proto = new TCompactProtocol(MakeTransport(stack, payload, trailing));
            Assert.AreEqual("abc", await proto.ReadStringAsync(default));
            CollectionAssert.AreEqual(new byte[] { 0x01, 0x02 }, await proto.ReadBinaryAsync(default));
            Assert.AreEqual(1, (await proto.ReadListBeginAsync(default)).Count);
            Assert.AreEqual(42, await proto.ReadI32Async(default));
        }
    }
}
