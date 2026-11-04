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
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift;
using Thrift.Protocol;
using Thrift.Protocol.Entities;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Protocols
{
    // TConfiguration.MaxContainerSize limits the number of elements a list, set or map may declare
    // when it is read. Every container here is written complete, with elements of type byte, and is
    // read back with MaxMessageSize set to its exact length. So every declared element really is on
    // the wire, and the byte budget that CheckReadBytesAvailable() holds the count to accepts it:
    // only MaxContainerSize can refuse it.
    //
    // A count above the limit must be refused by Read*BeginAsync(), with a SIZE_LIMIT
    // TProtocolException. A count at the limit must read back in full, and so must the count above
    // it when the limit is 0, -1 or left at its default, all of which switch it off.
    [TestClass]
    public class TProtocolMaxContainerSizeTests
    {
        public enum ProtocolKind { Binary, Compact, Json }

        public enum ContainerKind { List, Set, Map }

        private const int Limit = 20;
        private const int MapValueBase = 100;  // map values are 100 + key, all below 128

        public static IEnumerable<object[]> Containers
        {
            get
            {
                foreach (ProtocolKind kind in Enum.GetValues(typeof(ProtocolKind)))
                    foreach (ContainerKind container in Enum.GetValues(typeof(ContainerKind)))
                        yield return new object[] { kind, container };
            }
        }

        // null leaves MaxContainerSize at its default
        public static IEnumerable<object?[]> ContainersWithLimitOff
        {
            get
            {
                foreach (var container in Containers)
                    foreach (var off in new int?[] { 0, -1, null })
                        yield return new object?[] { container[0], container[1], off };
            }
        }

        private static TProtocol MakeProtocol(ProtocolKind kind, TTransport transport) => kind switch
        {
            ProtocolKind.Binary => new TBinaryProtocol(transport),
            ProtocolKind.Compact => new TCompactProtocol(transport),
            ProtocolKind.Json => new TJsonProtocol(transport),
            _ => throw new ArgumentOutOfRangeException(nameof(kind)),
        };

        // A complete container of count byte elements (count byte pairs for a map)
        private static async Task<byte[]> WrittenAsync(ProtocolKind kind, ContainerKind container, int count)
        {
            var trans = new TMemoryBufferTransport(new TConfiguration());
            var prot = MakeProtocol(kind, trans);
            switch (container)
            {
                case ContainerKind.List:
                    await prot.WriteListBeginAsync(new TList(TType.Byte, count), default);
                    break;
                case ContainerKind.Set:
                    await prot.WriteSetBeginAsync(new TSet(TType.Byte, count), default);
                    break;
                case ContainerKind.Map:
                    await prot.WriteMapBeginAsync(new TMap(TType.Byte, TType.Byte, count), default);
                    break;
            }
            for (var i = 0; i < count; ++i)
            {
                await prot.WriteByteAsync((sbyte)i, default);
                if (container == ContainerKind.Map)
                {
                    await prot.WriteByteAsync((sbyte)(MapValueBase + i), default);
                }
            }
            switch (container)
            {
                case ContainerKind.List:
                    await prot.WriteListEndAsync(default);
                    break;
                case ContainerKind.Set:
                    await prot.WriteSetEndAsync(default);
                    break;
                case ContainerKind.Map:
                    await prot.WriteMapEndAsync(default);
                    break;
            }
            await trans.FlushAsync(default);
            return trans.GetBuffer();
        }

        // null leaves MaxContainerSize at its default
        private static TProtocol Reader(ProtocolKind kind, byte[] data, int? limit)
        {
            var config = new TConfiguration { MaxMessageSize = data.Length };
            if (limit.HasValue)
            {
                config.MaxContainerSize = limit.Value;
            }
            return MakeProtocol(kind, new TMemoryBufferTransport(data, config));
        }

        // Returns the element count the container declares
        private static async Task<int> ReadBeginAsync(TProtocol prot, ContainerKind container)
        {
            switch (container)
            {
                case ContainerKind.List:
                    return (await prot.ReadListBeginAsync(default)).Count;
                case ContainerKind.Set:
                    return (await prot.ReadSetBeginAsync(default)).Count;
                case ContainerKind.Map:
                    return (await prot.ReadMapBeginAsync(default)).Count;
                default:
                    throw new ArgumentOutOfRangeException(nameof(container));
            }
        }

        // Reads the elements and the end of the container, and checks they are the ones written
        private static async Task ReadRestAsync(TProtocol prot, ContainerKind container, int count)
        {
            for (var i = 0; i < count; ++i)
            {
                Assert.AreEqual((sbyte)i, await prot.ReadByteAsync(default));
                if (container == ContainerKind.Map)
                {
                    Assert.AreEqual((sbyte)(MapValueBase + i), await prot.ReadByteAsync(default));
                }
            }
            switch (container)
            {
                case ContainerKind.List:
                    await prot.ReadListEndAsync(default);
                    break;
                case ContainerKind.Set:
                    await prot.ReadSetEndAsync(default);
                    break;
                case ContainerKind.Map:
                    await prot.ReadMapEndAsync(default);
                    break;
            }
        }

        private static async Task AssertReadBackInFullAsync(ProtocolKind kind, ContainerKind container, int count, int? limit)
        {
            var prot = Reader(kind, await WrittenAsync(kind, container, count), limit);
            Assert.AreEqual(count, await ReadBeginAsync(prot, container));
            await ReadRestAsync(prot, container, count);
        }

        private static async Task AssertRefusedByReadBeginAsync(ProtocolKind kind, ContainerKind container, int count, int limit)
        {
            var prot = Reader(kind, await WrittenAsync(kind, container, count), limit);
            int declared;
            try
            {
                declared = await ReadBeginAsync(prot, container);
            }
            catch (TProtocolException ex)
            {
                Assert.AreEqual(TProtocolException.SIZE_LIMIT, ex.GetExceptionType());
                Assert.AreEqual($"Container of {count} elements exceeds the limit of {limit}", ex.Message);
                return;
            }

            await ReadRestAsync(prot, container, declared);
            Assert.Fail($"A {container} of {declared} elements was read back in full with MaxContainerSize {limit}");
        }

        [TestMethod]
        [DynamicData(nameof(Containers))]
        public async Task ContainerAboveLimit_RefusedByReadBegin(ProtocolKind kind, ContainerKind container)
        {
            await AssertRefusedByReadBeginAsync(kind, container, Limit + 1, Limit);
        }

        [TestMethod]
        [DynamicData(nameof(Containers))]
        public async Task ContainerAtLimit_ReadBackInFull(ProtocolKind kind, ContainerKind container)
        {
            await AssertReadBackInFullAsync(kind, container, Limit, Limit);
        }

        [TestMethod]
        [DynamicData(nameof(ContainersWithLimitOff))]
        public async Task ContainerAboveLimit_ReadBackInFull_WhenLimitIsOff(ProtocolKind kind, ContainerKind container, int? off)
        {
            await AssertReadBackInFullAsync(kind, container, Limit + 1, off);
        }
    }
}
