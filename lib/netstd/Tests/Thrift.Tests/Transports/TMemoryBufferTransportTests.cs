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

using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Transport.Client;

namespace Thrift.Tests.Transports
{
    // Capacity is the size of the internal buffer, Length the number of bytes the buffer holds,
    // and Position the read/write position. Setting Capacity resizes the buffer only: growing it
    // keeps Length, Position and the content, and shrinking it keeps them too, except for what no
    // longer fits, which is cut off.
    [TestClass]
    public class TMemoryBufferTransportTests
    {
        private static readonly byte[] Content = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

        private static async Task<TMemoryBufferTransport> WrittenAsync(int initialCapacity)
        {
            var trans = new TMemoryBufferTransport(new TConfiguration(), initialCapacity);
            await trans.WriteAsync(Content, default);
            return trans;
        }

        private static async Task<byte[]> ReadFromStartAsync(TMemoryBufferTransport trans)
        {
            trans.Seek(0, SeekOrigin.Begin);
            var buffer = new byte[trans.Capacity + 1];
            var count = await trans.ReadAsync(buffer, 0, buffer.Length, default);
            return buffer.Take(count).ToArray();
        }

        [TestMethod]
        public async Task Capacity_GrowEmptyBuffer_KeepsItEmpty()
        {
            // the free space of a buffer is Capacity - Length
            var trans = new TMemoryBufferTransport(new TConfiguration(), 1024);
            trans.Capacity = 4096;

            Assert.AreEqual(4096, trans.Capacity);
            Assert.AreEqual(0, trans.Length);
            Assert.AreEqual(0, trans.Position);
            Assert.AreEqual(4096, trans.Capacity - trans.Length);
            CollectionAssert.AreEqual(new byte[0], trans.GetBuffer());

            await trans.WriteAsync(Content, default);
            CollectionAssert.AreEqual(Content, trans.GetBuffer());
        }

        [TestMethod]
        public async Task Capacity_ShrinkEmptyBuffer_KeepsItEmpty()
        {
            var trans = new TMemoryBufferTransport(new TConfiguration(), 1024);
            trans.Capacity = 100;

            Assert.AreEqual(100, trans.Capacity);
            Assert.AreEqual(0, trans.Length);
            Assert.AreEqual(0, trans.Position);
            CollectionAssert.AreEqual(new byte[0], trans.GetBuffer());

            await trans.WriteAsync(Content, default);
            CollectionAssert.AreEqual(Content, trans.GetBuffer());
        }

        [TestMethod]
        public async Task Capacity_Grow_KeepsLengthPositionAndContent()
        {
            var trans = await WrittenAsync(16);
            trans.Capacity = 4096;

            Assert.AreEqual(4096, trans.Capacity);
            Assert.AreEqual(Content.Length, trans.Length);
            Assert.AreEqual(Content.Length, trans.Position);
            CollectionAssert.AreEqual(Content, trans.GetBuffer());

            // the next write appends to the content
            await trans.WriteAsync(Content, default);
            CollectionAssert.AreEqual(Content.Concat(Content).ToArray(), trans.GetBuffer());
            CollectionAssert.AreEqual(Content.Concat(Content).ToArray(), await ReadFromStartAsync(trans));
        }

        [TestMethod]
        public async Task Capacity_ShrinkToLength_KeepsLengthPositionAndContent()
        {
            var trans = await WrittenAsync(2048);
            trans.Capacity = Content.Length;

            Assert.AreEqual(Content.Length, trans.Capacity);
            Assert.AreEqual(Content.Length, trans.Length);
            Assert.AreEqual(Content.Length, trans.Position);
            CollectionAssert.AreEqual(Content, trans.GetBuffer());
            CollectionAssert.AreEqual(Content, await ReadFromStartAsync(trans));
        }

        [TestMethod]
        public async Task Capacity_ShrinkAboveLength_KeepsLengthPositionAndContent()
        {
            var trans = await WrittenAsync(2048);
            trans.Capacity = 16;

            Assert.AreEqual(16, trans.Capacity);
            Assert.AreEqual(Content.Length, trans.Length);
            Assert.AreEqual(Content.Length, trans.Position);
            CollectionAssert.AreEqual(Content, trans.GetBuffer());
            CollectionAssert.AreEqual(Content, await ReadFromStartAsync(trans));
        }

        [TestMethod]
        public async Task Capacity_ShrinkBelowLength_CutsOffLengthAndPosition()
        {
            var trans = await WrittenAsync(2048);
            trans.Capacity = 4;

            Assert.AreEqual(4, trans.Capacity);
            Assert.AreEqual(4, trans.Length);
            Assert.AreEqual(4, trans.Position);
            CollectionAssert.AreEqual(Content.Take(4).ToArray(), trans.GetBuffer());

            // the next write appends to what is left
            await trans.WriteAsync(Content, default);
            CollectionAssert.AreEqual(Content.Take(4).Concat(Content).ToArray(), trans.GetBuffer());
        }

        [TestMethod]
        public async Task Capacity_ShrinkBelowLength_KeepsPositionThatStillFits()
        {
            var trans = await WrittenAsync(2048);
            trans.Seek(2, SeekOrigin.Begin);
            trans.Capacity = 6;

            Assert.AreEqual(6, trans.Capacity);
            Assert.AreEqual(6, trans.Length);
            Assert.AreEqual(2, trans.Position);

            // reading continues at the position, up to the new length
            var buffer = new byte[Content.Length];
            var count = await trans.ReadAsync(buffer, 0, buffer.Length, default);
            CollectionAssert.AreEqual(Content.Skip(2).Take(4).ToArray(), buffer.Take(count).ToArray());
        }
    }
}
