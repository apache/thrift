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
using System.Collections.Concurrent;
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Transports
{
    [TestClass]
    public class TCallTimeoutAndAsyncCreationTests
    {
        private sealed class TestClient : TBaseClient, IDisposable
        {
            public TestClient(ICallIsolatedTransport transport) : base(transport, new TBinaryProtocol.Factory()) { }
        }

        /// <summary>Async-capable transport that declares a timeout and records the views it created.</summary>
        private sealed class AsyncTimeoutTransport : IAsyncCallIsolatedTransport, IPerCallTimeoutTransport
        {
            public readonly ConcurrentBag<TMemoryBufferTransport> Views = new();
            public TimeSpan PerCallTimeout { get; set; } = Timeout.InfiniteTimeSpan;
            public Func<CancellationToken, Task>? Setup;

            public TTransport CreateCallTransport()
            {
                var v = new TMemoryBufferTransport(null);
                Views.Add(v);
                return v;
            }

            public async Task<TTransport> CreateCallTransportAsync(CancellationToken cancellationToken)
            {
                cancellationToken.ThrowIfCancellationRequested();
                if (Setup != null)
                {
                    await Setup(cancellationToken);
                }
                return CreateCallTransport();
            }

            public void Dispose() { }
        }

        private static async Task WaitUntilCancelled(CancellationToken token)
        {
            try { await Task.Delay(Timeout.Infinite, token); }
            catch (OperationCanceledException) { }
        }

        // ---- asynchronous creation -------------------------------------------------

        [TestMethod]
        public async Task Async_Creation_Returns_A_View_Owned_By_The_Caller()
        {
            var transport = new AsyncTimeoutTransport { Setup = async token => await Task.Delay(10, token) };
            using var client = new TestClient(transport);

            using var ctx = await client.CreateCallContextAsync();
            Assert.AreEqual(1, transport.Views.Count);
            Assert.IsTrue(ctx.Protocol.Transport.IsOpen);
        }

        [TestMethod]
        public async Task Async_Creation_Falls_Back_To_Sync_Creation_For_Transports_Without_Async_Setup()
        {
            var transport = new TMemoryBufferTransport(null);
            using var client = new TestClient(transport);
            using var ctx = await client.CreateCallContextAsync();
            Assert.IsNotNull(ctx.Protocol);
        }

        [TestMethod]
        public async Task Cancelling_During_Async_Creation_Leaves_No_View()
        {
            var transport = new AsyncTimeoutTransport();
            using var cts = new CancellationTokenSource();
            cts.Cancel();
            using var client = new TestClient(transport);

            await Assert.ThrowsAsync<OperationCanceledException>(() => client.CreateCallContextAsync(cts.Token));
            Assert.AreEqual(0, transport.Views.Count);
        }

        [TestMethod]
        public async Task Layers_Forward_Async_Creation_And_Dispose_The_Inner_View_On_Failure()
        {
            var inner = new AsyncTimeoutTransport();
            using var layer = new TIsolatedFramedTransport(inner);

            using var view = await ((IAsyncCallIsolatedTransport)layer).CreateCallTransportAsync(CancellationToken.None);
            Assert.IsInstanceOfType<TFramedTransport>(view);
            Assert.AreEqual(1, inner.Views.Count);

            using var cts = new CancellationTokenSource();
            cts.Cancel();
            await Assert.ThrowsAsync<OperationCanceledException>(
                () => ((IAsyncCallIsolatedTransport)layer).CreateCallTransportAsync(cts.Token));
            Assert.AreEqual(1, inner.Views.Count, "no view may be created for a cancelled request");
        }

        [TestMethod]
        public async Task Http_And_Memory_Transports_Support_Async_Creation()
        {
            using var http = new THttpTransport(new HttpClient(), null, new Uri("http://localhost/x"));
            using var viaHttp = await ((IAsyncCallIsolatedTransport)http).CreateCallTransportAsync(CancellationToken.None);
            Assert.IsNotNull(viaHttp);

            using var memory = new TMemoryBufferTransport(null);
            using var viaMemory = await ((IAsyncCallIsolatedTransport)memory).CreateCallTransportAsync(CancellationToken.None);
            Assert.IsNotNull(viaMemory);
        }

        // ---- per-call timeout ---------------------------------------------------------

        [TestMethod]
        public void Transports_Are_Unbounded_Unless_They_Declare_A_Timeout()
        {
            using var http = new THttpTransport(new HttpClient(), null, new Uri("http://localhost/x"));
            Assert.AreEqual(Timeout.InfiniteTimeSpan, http.PerCallTimeout);
            http.PerCallTimeout = TimeSpan.FromSeconds(5);
            Assert.AreEqual(TimeSpan.FromSeconds(5), http.PerCallTimeout);

            using var layer = new TIsolatedBufferedTransport(http);
            Assert.AreEqual(TimeSpan.FromSeconds(5), ((IPerCallTimeoutTransport)layer).PerCallTimeout, "layers report the inner timeout");
        }

        [TestMethod]
        public async Task Per_Call_Timeout_Cancels_Only_Its_Own_Call()
        {
            var transport = new AsyncTimeoutTransport();
            using var client = new TestClient(transport);
            using var slow = await client.CreateCallContextAsync(TimeSpan.FromMilliseconds(50));
            using var other = await client.CreateCallContextAsync();

            await WaitUntilCancelled(slow.CancellationToken);

            Assert.IsTrue(slow.IsTimedOut);
            Assert.IsFalse(other.CancellationToken.IsCancellationRequested);
            Assert.IsFalse(other.IsTimedOut);
        }

        [TestMethod]
        public async Task Transport_Timeout_Applies_To_Every_Call()
        {
            var transport = new AsyncTimeoutTransport { PerCallTimeout = TimeSpan.FromMilliseconds(50) };
            using var client = new TestClient(transport);
            using var ctx = await client.CreateCallContextAsync();

            await WaitUntilCancelled(ctx.CancellationToken);
            Assert.IsTrue(ctx.IsTimedOut);
        }

        [TestMethod]
        public async Task Per_Call_Timeout_Can_Tighten_But_Not_Loosen_The_Transport_Timeout()
        {
            var transport = new AsyncTimeoutTransport { PerCallTimeout = TimeSpan.FromMilliseconds(400) };
            using var client = new TestClient(transport);

            using var tighter = await client.CreateCallContextAsync(TimeSpan.FromMilliseconds(30));
            await WaitUntilCancelled(tighter.CancellationToken);
            Assert.IsTrue(tighter.IsTimedOut);

            using var looser = await client.CreateCallContextAsync(TimeSpan.FromMinutes(10));
            var sw = System.Diagnostics.Stopwatch.StartNew();
            await WaitUntilCancelled(looser.CancellationToken);
            Assert.IsTrue(looser.IsTimedOut);
            Assert.IsTrue(sw.Elapsed < TimeSpan.FromSeconds(5), "the transport's timeout must still apply");
        }

        [TestMethod]
        public async Task Caller_Cancellation_Is_Not_Reported_As_A_Timeout()
        {
            var transport = new AsyncTimeoutTransport { PerCallTimeout = TimeSpan.FromMinutes(10) };
            using var client = new TestClient(transport);
            using var cts = new CancellationTokenSource();
            using var ctx = await client.CreateCallContextAsync(cts.Token);

            cts.Cancel();
            Assert.IsTrue(ctx.CancellationToken.IsCancellationRequested);
            Assert.IsFalse(ctx.IsTimedOut);
            Assert.IsFalse(ctx.IsTimeoutFailure(new OperationCanceledException()));
        }

        [TestMethod]
        public async Task Timeout_Failures_Are_Recognized_And_Converted()
        {
            var transport = new AsyncTimeoutTransport { PerCallTimeout = TimeSpan.FromMilliseconds(30) };
            using var client = new TestClient(transport);
            using var ctx = await client.CreateCallContextAsync();
            await WaitUntilCancelled(ctx.CancellationToken);

            Assert.IsTrue(ctx.IsTimeoutFailure(new OperationCanceledException()));
            Assert.IsTrue(ctx.IsTimeoutFailure(new TTransportException(TTransportException.ExceptionType.Interrupted, "x")));
            Assert.IsFalse(ctx.IsTimeoutFailure(new InvalidOperationException()), "unrelated failures must pass through");

            var ex = ctx.CreateTimeoutException(new OperationCanceledException());
            Assert.AreEqual(TTransportException.ExceptionType.TimedOut, ex.Type);
        }

        [TestMethod]
        public async Task Without_Timeouts_The_Context_Uses_The_Callers_Token_Directly()
        {
            using var client = new TestClient(new AsyncTimeoutTransport());
            using var cts = new CancellationTokenSource();
            using var ctx = await client.CreateCallContextAsync(cts.Token);
            Assert.AreEqual(cts.Token, ctx.CancellationToken, "no linked source is allocated when no timeout is configured");
        }
    }
}
