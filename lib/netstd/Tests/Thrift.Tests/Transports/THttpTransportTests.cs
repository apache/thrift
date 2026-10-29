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
using System.Net;
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Transports
{
    [TestClass]
    public class THttpTransportTests
    {
        private static readonly Uri TestUri = new Uri("http://localhost/thrift");

        /// <summary>Echoes each request body back, prefixed with "re:", after the optional gate completes.</summary>
        private sealed class EchoHandler : HttpMessageHandler
        {
            public readonly List<TrackedContent> Responses = new();
            public Func<string, Task>? Gate;

            protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                var body = await request.Content!.ReadAsStringAsync(cancellationToken);
                if (Gate != null)
                {
                    await Gate(body).WaitAsync(cancellationToken);
                }

                var content = new TrackedContent("re:" + body);
                lock (Responses)
                {
                    Responses.Add(content);
                }
                return new HttpResponseMessage(HttpStatusCode.OK) { Content = content };
            }
        }

        private sealed class TrackedContent : StringContent
        {
            public bool IsDisposed;
            public TrackedContent(string s) : base(s) { }
            protected override void Dispose(bool disposing)
            {
                IsDisposed = true;
                base.Dispose(disposing);
            }
        }

        private static async Task WriteRequestAsync(TTransport t, string text, CancellationToken ct = default)
        {
            var bytes = System.Text.Encoding.UTF8.GetBytes(text);
            await t.WriteAsync(bytes, 0, bytes.Length, ct);
            await t.FlushAsync(ct);
        }

        private static async Task<string> ReadResponseAsync(TTransport t, int len, CancellationToken ct = default)
        {
            var buf = new byte[len];
            var n = await t.ReadAllAsync(buf, 0, len, ct);
            return System.Text.Encoding.UTF8.GetString(buf, 0, n);
        }

        [TestMethod]
        public void THttpTransport_Uses_Configured_ConnectionTimeout_Test()
        {
            var client = new HttpClient();
            var httpClientTransport = new THttpTransport(client, null)
            {
                ConnectTimeout = 5000
            };

            Assert.IsTrue(client.Timeout.TotalMilliseconds == 5000);
            Assert.IsTrue(httpClientTransport.ConnectTimeout == 5000);
        }

        [TestMethod]
        public async Task Overlapping_Calls_Keep_Their_Own_Request_And_Response()
        {
            var handler = new EchoHandler();
            var release = new TaskCompletionSource();
            var arrived = new TaskCompletionSource();
            handler.Gate = async body =>
            {
                if (body == "A")
                {
                    arrived.TrySetResult();
                    await release.Task;   // A's response is held until B has completed
                }
            };

            using var shared = new THttpTransport(new HttpClient(handler), null, TestUri);
            using var callA = shared.CreateCallTransport();
            using var callB = shared.CreateCallTransport();

            var flushA = WriteRequestAsync(callA, "A");
            await arrived.Task;
            await WriteRequestAsync(callB, "B");
            Assert.AreEqual("re:B", await ReadResponseAsync(callB, 4));

            release.SetResult();
            await flushA;
            Assert.AreEqual("re:A", await ReadResponseAsync(callA, 4));
        }

        [TestMethod]
        public async Task Disposing_One_Call_Leaves_Sibling_Usable()
        {
            var handler = new EchoHandler();
            using var shared = new THttpTransport(new HttpClient(handler), null, TestUri);
            var callA = shared.CreateCallTransport();
            using var callB = shared.CreateCallTransport();

            await WriteRequestAsync(callA, "A");
            await WriteRequestAsync(callB, "B");
            Assert.AreEqual("re:A", await ReadResponseAsync(callA, 4));
            callA.Dispose();

            Assert.IsTrue(handler.Responses[0].IsDisposed, "A's response must be released with its view");
            Assert.IsFalse(handler.Responses[1].IsDisposed, "B's response must stay alive");
            Assert.AreEqual("re:B", await ReadResponseAsync(callB, 4));
        }

        [TestMethod]
        public async Task Cancelling_One_Call_Does_Not_Cancel_Sibling()
        {
            var handler = new EchoHandler();
            var releaseB = new TaskCompletionSource();
            var arrivedA = new TaskCompletionSource();
            handler.Gate = async body =>
            {
                if (body == "A")
                {
                    arrivedA.TrySetResult();
                    await Task.Delay(Timeout.Infinite);
                }
                else
                {
                    await releaseB.Task;
                }
            };

            using var shared = new THttpTransport(new HttpClient(handler), null, TestUri);
            using var callA = shared.CreateCallTransport();
            using var callB = shared.CreateCallTransport();
            using var cts = new CancellationTokenSource();

            var flushA = WriteRequestAsync(callA, "A", cts.Token);
            await arrivedA.Task;
            var flushB = WriteRequestAsync(callB, "B");

            cts.Cancel();
            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => flushA);
            Assert.AreEqual(TTransportException.ExceptionType.Interrupted, ex.Type);

            releaseB.SetResult();
            await flushB;
            Assert.AreEqual("re:B", await ReadResponseAsync(callB, 4));
        }

        [TestMethod]
        public async Task Failed_Call_Releases_Its_Resources_And_New_Calls_Are_Rejected_After_Dispose()
        {
            var handler = new EchoHandler();
            var shared = new THttpTransport(new HttpClient(handler), null, TestUri);
            var call = shared.CreateCallTransport();
            call.Dispose();
            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => WriteRequestAsync(call, "x"));
            Assert.AreEqual(TTransportException.ExceptionType.NotOpen, ex.Type);

            shared.Dispose();
            Assert.ThrowsExactly<ObjectDisposedException>(() => shared.CreateCallTransport());
        }

        [TestMethod]
        public async Task Disposed_Http_Call_View_Reports_NotOpen_For_Every_Operation()
        {
            using var shared = new THttpTransport(new HttpClient(new EchoHandler()), null, TestUri);
            var call = shared.CreateCallTransport();
            call.Dispose();

            Assert.IsFalse(call.IsOpen);
            foreach (var operation in new Func<Task>[]
                     {
                         () => call.WriteAsync(new byte[] { 1 }, 0, 1, CancellationToken.None),
                         () => call.FlushAsync(CancellationToken.None),
                         async () => await call.ReadAsync(new byte[1], 0, 1, CancellationToken.None),
                     })
            {
                var ex = await Assert.ThrowsExactlyAsync<TTransportException>(operation);
                Assert.AreEqual(TTransportException.ExceptionType.NotOpen, ex.Type);
            }
        }

        [TestMethod]
        public async Task Read_Before_Flush_Reports_NotOpen()
        {
            using var shared = new THttpTransport(new HttpClient(new EchoHandler()), null, TestUri);
            using var call = shared.CreateCallTransport();
            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => ReadResponseAsync(call, 1));
            Assert.AreEqual(TTransportException.ExceptionType.NotOpen, ex.Type);
        }
    }
}
