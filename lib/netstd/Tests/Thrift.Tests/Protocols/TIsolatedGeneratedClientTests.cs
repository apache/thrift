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
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using IsolatedCall;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Processor;
using Thrift.Protocol.Entities;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Protocols
{
    /// <summary>Exercises the code generated with <c>netstd:isolated</c> end to end.</summary>
    [TestClass]
    public class TIsolatedGeneratedClientTests
    {
        private sealed class Handler : IsolatedService.IAsync
        {
            public readonly ConcurrentBag<string> Fired = new();

            public Task<int> ping(int value, CancellationToken cancellationToken = default) => Task.FromResult(value + 1000);
            public Task nothing(CancellationToken cancellationToken = default) => Task.CompletedTask;

            public Task<string> echo(string? text, CancellationToken cancellationToken = default)
            {
                if (text == "bad")
                {
                    throw new Oops { Why = "because" };
                }
                return Task.FromResult(text ?? "");
            }

            public Task<int> add(int a, int b, CancellationToken cancellationToken = default) => Task.FromResult(a + b);
            public Task<int> collide(int callContext, int cancellationToken_, CancellationToken cancellationToken = default) => Task.FromResult(callContext * 100 + cancellationToken_);

            public Task fire(string? text, CancellationToken cancellationToken = default)
            {
                Fired.Add(text ?? "");
                return Task.CompletedTask;
            }
        }

        /// <summary>Call-isolated transport whose views run each request through the generated processor.</summary>
        private sealed class LoopbackTransport : ICallIsolatedTransport, IPerCallTimeoutTransport
        {
            private readonly ITAsyncProcessor _processor;
            public readonly ConcurrentBag<View> Views = new();
            public bool IsDisposed;
            public int ReplySeqIdOffset;
            public TimeSpan PerCallTimeout { get; set; } = Timeout.InfiniteTimeSpan;

            /// <summary>Reading a reply waits for this task, to simulate a slow server.</summary>
            public Task ReplyGate = Task.CompletedTask;

            public LoopbackTransport(IsolatedService.IAsync handler) : this(new IsolatedService.AsyncProcessor(handler)) { }

            public LoopbackTransport(ITAsyncProcessor processor) => _processor = processor;

            public TTransport CreateCallTransport()
            {
                var v = new View(this);
                Views.Add(v);
                return v;
            }

            public void Dispose() => IsDisposed = true;

            public sealed class View : TEndpointTransport
            {
                private readonly LoopbackTransport _owner;
                private readonly MemoryStream _request = new();
                private TMemoryBufferTransport? _reply;
                public bool Disposed;

                public View(LoopbackTransport owner) : base(null) => _owner = owner;

                public override bool IsOpen => !Disposed;
                public override Task OpenAsync(CancellationToken cancellationToken) => Task.CompletedTask;
                public override void Close() { }

                public override Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
                {
                    _request.Write(buffer, offset, length);
                    return Task.CompletedTask;
                }

                public override async Task FlushAsync(CancellationToken cancellationToken)
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    var input = new TBinaryProtocol(new TMemoryBufferTransport(_request.ToArray(), null));
                    var output = new TBinaryProtocol(new TMemoryBufferTransport(null));
                    _request.SetLength(0);
                    await _owner._processor.ProcessAsync(input, output, cancellationToken);
                    var bytes = ((TMemoryBufferTransport)output.Transport).GetBuffer();
                    if (_owner.ReplySeqIdOffset != 0 && bytes.Length > 0)
                    {
                        bytes = Rewrite(bytes, _owner.ReplySeqIdOffset);
                    }
                    _reply = new TMemoryBufferTransport(bytes, null);
                }

                // strict binary: version(4) name-len(4) name(n) seqid(4)
                private static byte[] Rewrite(byte[] bytes, int offset)
                {
                    var nameLen = (bytes[4] << 24) | (bytes[5] << 16) | (bytes[6] << 8) | bytes[7];
                    var pos = 8 + nameLen;
                    var seq = (bytes[pos] << 24) | (bytes[pos + 1] << 16) | (bytes[pos + 2] << 8) | bytes[pos + 3];
                    seq += offset;
                    bytes[pos] = (byte)(seq >> 24); bytes[pos + 1] = (byte)(seq >> 16); bytes[pos + 2] = (byte)(seq >> 8); bytes[pos + 3] = (byte)seq;
                    return bytes;
                }

                public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
                {
                    if (_reply == null)
                    {
                        throw new TTransportException(TTransportException.ExceptionType.NotOpen, "No request has been sent");
                    }
                    await _owner.ReplyGate.WaitAsync(cancellationToken);
                    return await _reply.ReadAsync(buffer, offset, length, cancellationToken);
                }

                protected override void Dispose(bool disposing) => Disposed = true;
            }
        }

        private static (IsolatedService.Client client, LoopbackTransport transport, Handler handler) Create()
        {
            var handler = new Handler();
            var transport = new LoopbackTransport(handler);
            return (new IsolatedService.Client(transport, new TBinaryProtocol.Factory()), transport, handler);
        }

        [TestMethod]
        public async Task High_Level_Call_Returns_Result_And_Disposes_Its_View()
        {
            var (client, transport, _) = Create();
            Assert.AreEqual(5, await client.add(2, 3));
            await client.nothing();
            Assert.AreEqual(2, transport.Views.Count);
            Assert.IsTrue(transport.Views.All(v => v.Disposed));
            Assert.IsFalse(transport.IsDisposed);
        }

        [TestMethod]
        public async Task Inherited_Service_Method_Uses_Isolated_Path()
        {
            var (client, transport, _) = Create();
            Assert.AreEqual(1007, await client.ping(7));
            Assert.IsTrue(transport.Views.All(v => v.Disposed));
        }

        [TestMethod]
        public async Task Colliding_Argument_Names_Are_Renamed()
        {
            var (client, _, _) = Create();
            Assert.AreEqual(305, await client.collide(3, 5));
        }

        [TestMethod]
        public async Task Declared_Exception_Propagates_And_View_Is_Disposed()
        {
            var (client, transport, _) = Create();
            var ex = await Assert.ThrowsExactlyAsync<Oops>(() => client.echo("bad"));
            Assert.AreEqual("because", ex.Why);
            Assert.IsTrue(transport.Views.All(v => v.Disposed));
        }

        [TestMethod]
        public async Task Oneway_Call_Sends_And_Releases_Its_Context()
        {
            var (client, transport, handler) = Create();
            await client.fire("go");
            CollectionAssert.AreEqual(new[] { "go" }, handler.Fired.ToArray());
            Assert.IsTrue(transport.Views.All(v => v.Disposed));
        }

        [TestMethod]
        public async Task Replies_Can_Be_Received_Out_Of_Order_With_Explicit_Contexts()
        {
            var (client, transport, _) = Create();
            using var first = client.CreateCallContext();
            using var second = client.CreateCallContext();

            await client.send_add(first, 1, 2);
            await client.send_add(second, 10, 20);

            Assert.AreEqual(30, await client.recv_add(second));
            Assert.AreEqual(3, await client.recv_add(first));
            Assert.IsFalse(transport.Views.Any(v => v.Disposed), "explicit contexts are owned by the caller");
        }

        [TestMethod]
        public async Task Concurrent_Calls_Do_Not_Interfere()
        {
            var (client, _, _) = Create();
            var results = await Task.WhenAll(Enumerable.Range(0, 100).Select(i => Task.Run(() => client.add(i, i))));
            CollectionAssert.AreEqual(Enumerable.Range(0, 100).Select(i => 2 * i).ToArray(), results);
        }

        [TestMethod]
        public async Task Mismatched_Reply_Sequence_Id_Is_Rejected()
        {
            var (client, transport, _) = Create();
            transport.ReplySeqIdOffset = 7;
            var ex = await Assert.ThrowsExactlyAsync<TApplicationException>(() => client.add(1, 1));
            Assert.AreEqual(TApplicationException.ExceptionType.BadSequenceId, ex.Type);
            Assert.IsTrue(transport.Views.All(v => v.Disposed));
        }

        // ---- dual mode: the isolated-generated client keeps its legacy constructors -------------

        [TestMethod]
        public async Task Legacy_Constructed_Client_Uses_The_Shared_Protocols()
        {
            var transport = new LoopbackTransport(new Handler());
            using var shared = transport.CreateCallTransport();
            var client = new IsolatedService.Client(new TBinaryProtocol(shared));

            Assert.IsFalse(client.IsCallIsolated);
            Assert.AreEqual(5, await client.add(2, 3));
            Assert.AreEqual(9, await client.add(4, 5));

            // the split operations without a context use the shared protocol too
            await client.send_add(1, 1);
            Assert.AreEqual(2, await client.recv_add());

            Assert.AreEqual(1, transport.Views.Count, "a legacy client must not create call views");
        }

        [TestMethod]
        public void Isolated_Constructed_Client_Reports_Its_Mode_And_Has_No_Shared_Protocols()
        {
            var (client, _, _) = Create();
            Assert.IsTrue(client.IsCallIsolated);
            Assert.ThrowsExactly<InvalidOperationException>(() => _ = client.InputProtocol);
            Assert.ThrowsExactly<InvalidOperationException>(() => _ = client.OutputProtocol);
        }

        // ---- multiplexed protocol ------------------------------------------------------------

        private sealed class MultiplexedFactory : TProtocolFactory
        {
            public override TProtocol GetProtocol(TTransport trans) =>
                new TMultiplexedProtocol(new TBinaryProtocol(trans), "IsolatedService");
        }

        [TestMethod]
        public async Task Protocol_Factory_Can_Return_A_Multiplexed_Protocol()
        {
            var multiplexer = new TMultiplexedProcessor();
            multiplexer.RegisterProcessor("IsolatedService", new IsolatedService.AsyncProcessor(new Handler()));
            var transport = new LoopbackTransport(multiplexer);
            var client = new IsolatedService.Client(transport, new MultiplexedFactory());

            // overlapping calls, each with its own multiplexed protocol
            var sums = await Task.WhenAll(client.add(1, 2), client.add(3, 4));
            CollectionAssert.AreEqual(new[] { 3, 7 }, sums);
            Assert.IsTrue(transport.Views.All(v => v.Disposed));
        }

        // ---- per-call timeout through the generated client ----------------------------------

        [TestMethod]
        public async Task Transport_Timeout_Fails_A_Slow_High_Level_Call_As_TimedOut()
        {
            var (client, transport, _) = Create();
            transport.PerCallTimeout = TimeSpan.FromMilliseconds(50);
            transport.ReplyGate = new TaskCompletionSource().Task; // the reply never arrives

            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => client.add(1, 1));
            Assert.AreEqual(TTransportException.ExceptionType.TimedOut, ex.Type);
            Assert.IsTrue(transport.Views.All(v => v.Disposed));
        }

        [TestMethod]
        public async Task Per_Call_Timeout_Spans_Split_Send_And_Receive()
        {
            var (client, transport, _) = Create();
            transport.ReplyGate = new TaskCompletionSource().Task;
            using var ctx = client.CreateCallContext(TimeSpan.FromMilliseconds(50));

            await client.send_add(ctx, 1, 1);
            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => client.recv_add(ctx));
            Assert.AreEqual(TTransportException.ExceptionType.TimedOut, ex.Type);
        }

        [TestMethod]
        public async Task Caller_Cancellation_Of_A_Slow_Call_Is_Not_Reported_As_A_Timeout()
        {
            var (client, transport, _) = Create();
            transport.PerCallTimeout = TimeSpan.FromMinutes(10);
            transport.ReplyGate = new TaskCompletionSource().Task;
            using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(50));

            await Assert.ThrowsAsync<OperationCanceledException>(() => client.add(1, 1, cts.Token));
            Assert.IsTrue(transport.Views.All(v => v.Disposed));
        }

        [TestMethod]
        public async Task Cancelled_Call_Disposes_Its_View_And_Leaves_Others_Usable()
        {
            var (client, transport, _) = Create();
            using var cts = new CancellationTokenSource();
            cts.Cancel();

            // a call that is cancelled before it starts creates no view at all, so nothing can leak
            await Assert.ThrowsAsync<OperationCanceledException>(() => client.add(1, 1, cts.Token));
            Assert.AreEqual(0, transport.Views.Count);

            Assert.AreEqual(4, await client.add(2, 2));
        }
    }
}
