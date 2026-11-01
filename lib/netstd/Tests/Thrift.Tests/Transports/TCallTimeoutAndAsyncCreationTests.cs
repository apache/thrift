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
using System.Linq;
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
                // a call view of a memory buffer reports that it is not open once it was disposed
                var v = (TMemoryBufferTransport)new TMemoryBufferTransport(null).CreateCallTransport();
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
        public async Task Cancellation_That_Arrives_During_Creation_Disposes_The_View_Even_When_The_Creator_Ignores_It()
        {
            // the creator takes 200 ms and does not look at its token; the token is cancelled after 50 ms
            var transport = new AsyncTimeoutTransport { Setup = _ => Task.Delay(200) };
            using var layer = new TIsolatedFramedTransport(transport);
            using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(50));

            await Assert.ThrowsAsync<OperationCanceledException>(
                () => ((IAsyncCallIsolatedTransport)layer).CreateCallTransportAsync(cts.Token));

            Assert.AreEqual(1, transport.Views.Count, "the creator did create a view");
            Assert.IsTrue(transport.Views.All(v => !v.IsOpen), "the view created in the meantime must be disposed");
        }

        [TestMethod]
        public async Task Client_Does_Not_Return_A_Context_When_Cancellation_Arrives_During_Creation()
        {
            var transport = new AsyncTimeoutTransport { Setup = _ => Task.Delay(200) };
            using var client = new TestClient(transport);
            using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(50));

            await Assert.ThrowsAsync<OperationCanceledException>(() => client.CreateCallContextAsync(cts.Token));

            Assert.AreEqual(1, transport.Views.Count);
            Assert.IsTrue(transport.Views.All(v => !v.IsOpen), "the view created in the meantime must be disposed");
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

        // ---- synchronous creation and cancellation ---------------------------------------

        [TestMethod]
        public void Sync_Creation_Does_Not_Create_A_View_For_A_Cancelled_Token()
        {
            var transport = new AsyncTimeoutTransport();
            using var client = new TestClient(transport);
            using var cts = new CancellationTokenSource();
            cts.Cancel();

            Assert.Throws<OperationCanceledException>(() => client.CreateCallContext(cts.Token));
            Assert.AreEqual(0, transport.Views.Count, "no view may be created for a cancelled request");
        }

        [TestMethod]
        public void Sync_Creation_Disposes_The_View_When_Cancellation_Arrives_During_Creation()
        {
            using var cts = new CancellationTokenSource();
            var transport = new AsyncTimeoutTransport { Setup = _ => Task.CompletedTask };
            var cancelling = new CancellingTransport(transport, cts);
            using var client = new TestClient(cancelling);

            Assert.Throws<OperationCanceledException>(() => client.CreateCallContext(cts.Token));
            Assert.AreEqual(1, transport.Views.Count, "the creator did create a view");
            Assert.IsTrue(transport.Views.All(v => !v.IsOpen), "the view created in the meantime must be disposed");
        }

        /// <summary>Cancels the caller's token while the view is being created, and does not look at it.</summary>
        private sealed class CancellingTransport : ICallIsolatedTransport
        {
            private readonly ICallIsolatedTransport _inner;
            private readonly CancellationTokenSource _source;

            public CancellingTransport(ICallIsolatedTransport inner, CancellationTokenSource source)
            {
                _inner = inner;
                _source = source;
            }

            public TTransport CreateCallTransport()
            {
                var view = _inner.CreateCallTransport();
                _source.Cancel();
                return view;
            }

            public void Dispose() { }
        }

        // ---- ownership when the context cannot be built -----------------------------------

        private sealed class RecordingProtocol : TBinaryProtocol
        {
            public bool IsProtocolDisposed;

            public RecordingProtocol(TTransport transport) : base(transport) { }

            protected override void Dispose(bool disposing)
            {
                IsProtocolDisposed = true;
                base.Dispose(disposing);
            }
        }

        private sealed class RecordingFactory : TProtocolFactory
        {
            public RecordingProtocol? Created;

            public override TProtocol GetProtocol(TTransport trans) => Created = new RecordingProtocol(trans);
        }

        [TestMethod]
        public void A_Context_That_Cannot_Be_Built_Disposes_The_Protocol_And_The_View()
        {
            var transport = new AsyncTimeoutTransport();
            var factory = new RecordingFactory();
            using var client = new TBaseClientWithFactory(transport, factory);

            Assert.Throws<ArgumentOutOfRangeException>(() => client.CreateCallContext(TimeSpan.FromSeconds(-1)));

            Assert.IsTrue(factory.Created!.IsProtocolDisposed, "the protocol may hold resources of its own");
            Assert.IsTrue(transport.Views.All(v => !v.IsOpen), "the view must be disposed as well");
        }

        private sealed class TBaseClientWithFactory : TBaseClient, IDisposable
        {
            public TBaseClientWithFactory(ICallIsolatedTransport transport, TProtocolFactory factory) : base(transport, factory) { }
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
