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
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Protocol.Entities;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Protocols
{
    [TestClass]
    public class TCallContextClientTests
    {
        private sealed class TestClient : TBaseClient, IDisposable
        {
            public TestClient(ICallIsolatedTransport transport)
                : base(transport, new TBinaryProtocol.Factory()) { }
        }

        /// <summary>Hands out memory views and remembers them.</summary>
        private sealed class TrackingTransport : ICallIsolatedTransport
        {
            public readonly ConcurrentBag<TMemoryBufferTransport> Views = new();
            public bool IsDisposed;

            public TTransport CreateCallTransport()
            {
                var v = new TMemoryBufferTransport(null);
                Views.Add(v);
                return v;
            }

            public void Dispose() => IsDisposed = true;
        }

        [TestMethod]
        public void Contexts_Get_Distinct_Protocols_And_Transports()
        {
            var transport = new TrackingTransport();
            using var client = new TestClient(transport);
            using var a = client.CreateCallContext();
            using var b = client.CreateCallContext();

            Assert.AreNotSame(a.Protocol, b.Protocol);
            Assert.AreNotSame(a.Protocol.Transport, b.Protocol.Transport);
            Assert.AreNotEqual(a.SeqId, b.SeqId);
        }

        [TestMethod]
        public async Task Concurrent_Contexts_Get_Unique_Sequence_Ids()
        {
            using var client = new TestClient(new TrackingTransport());
            var ids = await Task.WhenAll(Enumerable.Range(0, 500).Select(_ => Task.Run(() =>
            {
                using var ctx = client.CreateCallContext();
                return ctx.SeqId;
            })));
            Assert.AreEqual(ids.Length, ids.Distinct().Count());
        }

        [TestMethod]
        public void Matching_Reply_Is_Accepted_And_Mismatch_Is_Rejected()
        {
            using var client = new TestClient(new TrackingTransport());
            using var ctx = client.CreateCallContext();

            ctx.ValidateReply(new TMessage("m", TMessageType.Reply, ctx.SeqId));

            var ex = Assert.ThrowsExactly<TApplicationException>(
                () => ctx.ValidateReply(new TMessage("m", TMessageType.Reply, ctx.SeqId + 1)));
            Assert.AreEqual(TApplicationException.ExceptionType.BadSequenceId, ex.Type);
        }

        [TestMethod]
        public void Disposing_A_Context_Disposes_Its_Transport_But_Not_Siblings()
        {
            var transport = new TrackingTransport();
            using var client = new TestClient(transport);
            var a = client.CreateCallContext();
            using var b = client.CreateCallContext();

            a.Dispose();
            a.Dispose(); // idempotent

            Assert.IsFalse(transport.IsDisposed, "the shared transport belongs to the client");
            Assert.IsTrue(b.Protocol.Transport.IsOpen);
        }

        [TestMethod]
        public void Context_Carries_Its_Own_CancellationToken()
        {
            using var client = new TestClient(new TrackingTransport());
            using var cts = new CancellationTokenSource();
            using var a = client.CreateCallContext(cts.Token);
            using var b = client.CreateCallContext();

            cts.Cancel();
            Assert.IsTrue(a.CancellationToken.IsCancellationRequested);
            Assert.IsFalse(b.CancellationToken.IsCancellationRequested);
        }

        [TestMethod]
        public void Disposing_Client_Disposes_Shared_Transport_And_Rejects_New_Calls()
        {
            var transport = new TrackingTransport();
            var client = new TestClient(transport);
            client.Dispose();

            Assert.IsTrue(transport.IsDisposed);
            Assert.ThrowsExactly<ObjectDisposedException>(() => client.CreateCallContext());
        }

        [TestMethod]
        public void Legacy_Client_Is_Unchanged()
        {
            var t = new TMemoryBufferTransport(null);
            var p = new TBinaryProtocol(t);
            var legacy = new LegacyClient(p);
            Assert.AreSame(p, legacy.InputProtocol);
            Assert.AreEqual(1, legacy.SeqId);
            Assert.AreEqual(2, legacy.SeqId);
            Assert.ThrowsExactly<InvalidOperationException>(() => legacy.CreateCallContext());
        }

        [TestMethod]
        public void Disposing_The_Client_Before_A_Context_Is_Safe_For_The_Caller_Owned_Context()
        {
            // the shared client may be disposed while a caller still holds a context; releasing the
            // context afterwards must neither throw nor touch the already released transport twice
            var transport = new TrackingTransport();
            var client = new TestClient(transport);
            var ctx = client.CreateCallContext();

            client.Dispose();
            ctx.Dispose();
            ctx.Dispose();

            Assert.IsTrue(transport.IsDisposed);
            Assert.ThrowsExactly<ObjectDisposedException>(() => _ = ctx.Protocol);
        }

        private sealed class LegacyClient : TBaseClient
        {
            public LegacyClient(TProtocol p) : base(p, p) { }
        }
    }
}
