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
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Transports
{
    [TestClass]
    public class TCallIsolatedLayerTests
    {
        [TestMethod]
        public async Task MemoryBuffer_Views_Do_Not_Share_Contents()
        {
            using var shared = new TMemoryBufferTransport(null);
            using var a = shared.CreateCallTransport();
            using var b = shared.CreateCallTransport();

            await a.WriteAsync(new byte[] { 1, 2, 3 }, CancellationToken.None);
            await b.WriteAsync(new byte[] { 9 }, CancellationToken.None);

            Assert.AreEqual(3, ((TMemoryBufferTransport)a).Length);
            Assert.AreEqual(1, ((TMemoryBufferTransport)b).Length);
            CollectionAssert.AreEqual(new byte[] { 1, 2, 3 }, ((TMemoryBufferTransport)a).GetBuffer());
            CollectionAssert.AreEqual(new byte[] { 9 }, ((TMemoryBufferTransport)b).GetBuffer());
            Assert.AreEqual(0, shared.Length, "the shared transport itself must stay untouched");
        }

        [TestMethod]
        public void Buffered_Layer_Creates_Distinct_Layered_Views_Over_Distinct_Inner_Views()
        {
            using var layer = new TIsolatedBufferedTransport(new TMemoryBufferTransport(null));
            using var a = (TLayeredTransport)layer.CreateCallTransport();
            using var b = (TLayeredTransport)layer.CreateCallTransport();

            Assert.IsInstanceOfType<TBufferedTransport>(a);
            Assert.AreNotSame(a, b);
            Assert.AreNotSame(a.InnerTransport, b.InnerTransport);
        }

        [TestMethod]
        public async Task Framed_Layer_Views_Exchange_Independent_Data()
        {
            using var layer = new TIsolatedFramedTransport(new TMemoryBufferTransport(null));
            using var a = layer.CreateCallTransport();
            using var b = layer.CreateCallTransport();
            Assert.IsInstanceOfType<TFramedTransport>(a);

            await a.WriteAsync(new byte[] { 1, 2 }, CancellationToken.None);
            await b.WriteAsync(new byte[] { 7, 8, 9 }, CancellationToken.None);
            await a.FlushAsync(CancellationToken.None);
            await b.FlushAsync(CancellationToken.None);

            var innerA = (TMemoryBufferTransport)((TLayeredTransport)a).InnerTransport;
            var innerB = (TMemoryBufferTransport)((TLayeredTransport)b).InnerTransport;
            Assert.AreEqual(4 + 2, innerA.Length);
            Assert.AreEqual(4 + 3, innerB.Length);
        }

        [TestMethod]
        public void Disposing_A_Layered_View_Disposes_Only_Its_Own_Inner_View()
        {
            using var layer = new TIsolatedBufferedTransport(new TMemoryBufferTransport(null));
            var a = layer.CreateCallTransport();
            using var b = layer.CreateCallTransport();
            a.Dispose();
            Assert.IsTrue(b.IsOpen);
        }

        [TestMethod]
        public void Isolation_Layers_Only_Accept_CallIsolated_Transports()
        {
            foreach (var type in new[] { typeof(TIsolatedBufferedTransport), typeof(TIsolatedFramedTransport) })
            {
                foreach (var ctor in type.GetConstructors())
                {
                    Assert.IsTrue(typeof(ICallIsolatedTransport).IsAssignableFrom(ctor.GetParameters()[0].ParameterType),
                        $"{type.Name} constructor must require ICallIsolatedTransport");
                }
            }
        }

        [TestMethod]
        public async Task Disposed_Memory_Call_View_Reports_NotOpen_For_Every_Operation()
        {
            using var shared = new TMemoryBufferTransport(null);
            var view = shared.CreateCallTransport();
            view.Dispose();

            Assert.IsFalse(view.IsOpen);
            foreach (var operation in new Func<Task>[]
                     {
                         () => view.WriteAsync(new byte[] { 1 }, 0, 1, CancellationToken.None),
                         () => view.FlushAsync(CancellationToken.None),
                         async () => await view.ReadAsync(new byte[1], 0, 1, CancellationToken.None),
                     })
            {
                var ex = await Assert.ThrowsExactlyAsync<TTransportException>(operation);
                Assert.AreEqual(TTransportException.ExceptionType.NotOpen, ex.Type);
            }

            // the shared memory transport, and the legacy use of the class, are not affected
            Assert.IsTrue(shared.IsOpen);
        }

        private sealed class NoViewTransport : IAsyncCallIsolatedTransport
        {
            public TTransport CreateCallTransport() => null!;
            public Task<TTransport> CreateCallTransportAsync(CancellationToken cancellationToken) => Task.FromResult<TTransport>(null!);
            public void Dispose() { }
        }

        [TestMethod]
        public async Task Layer_Reports_The_Real_Cause_When_The_Inner_Transport_Returns_No_View()
        {
            using var layer = new TIsolatedBufferedTransport(new NoViewTransport());

            var sync = Assert.ThrowsExactly<InvalidOperationException>(() => layer.CreateCallTransport());
            StringAssert.Contains(sync.Message, "no transport view");

            var asyncEx = await Assert.ThrowsExactlyAsync<InvalidOperationException>(
                () => ((IAsyncCallIsolatedTransport)layer).CreateCallTransportAsync(CancellationToken.None));
            StringAssert.Contains(asyncEx.Message, "no transport view");
        }

        [TestMethod]
        public void Legacy_Layers_Still_Accept_Any_Transport()
        {
            using var t = new TBufferedTransport(new TMemoryBufferTransport(null));
            using var f = new TFramedTransport(new TMemoryBufferTransport(null));
            Assert.IsNotNull(t.UnderlyingTransport);
            Assert.IsNotNull(f);
        }
    }
}
