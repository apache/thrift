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
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
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
        // Records the Accept-Encoding values of each request and answers with an empty 200 response.
        // Like the handler of a plain HttpClient, it decompresses nothing.
        private sealed class RecordingHandler : HttpMessageHandler
        {
            public List<List<string>> AcceptEncodings { get; } = new List<List<string>>();

            protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                AcceptEncodings.Add(request.Headers.AcceptEncoding.Select(value => value.ToString()).ToList());
                return Task.FromResult(new HttpResponseMessage(HttpStatusCode.OK)
                {
                    Content = new ByteArrayContent(Array.Empty<byte>()),
                    RequestMessage = request
                });
            }
        }

        private static async Task<List<string>> SendRequestAsync(THttpTransport transport, RecordingHandler handler)
        {
            await transport.WriteAsync(new byte[] { 0 }, 0, 1, CancellationToken.None);
            await transport.FlushAsync(CancellationToken.None);

            Assert.HasCount(1, handler.AcceptEncodings);
            return handler.AcceptEncodings[0];
        }

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
        public void THttpTransport_Own_Handler_Advertises_Deflate_And_Gzip_Test()
        {
            // the transport creates the handler of this client, and that handler decompresses both
            using var transport = new THttpTransport(new Uri("http://localhost/"), new TConfiguration());

            var advertised = transport.RequestHeaders.AcceptEncoding.Select(value => value.ToString()).ToList();
            CollectionAssert.AreEqual(new[] { "deflate", "gzip" }, advertised);
        }

        [TestMethod]
        public async Task THttpTransport_Injected_Client_Sends_No_Accept_Encoding_Test()
        {
            // the handler of a caller-supplied client is configured by the caller, and here it decompresses nothing
            var handler = new RecordingHandler();
            using var transport = new THttpTransport(new HttpClient(handler), new TConfiguration(), new Uri("http://localhost/"));

            var sent = await SendRequestAsync(transport, handler);
            CollectionAssert.AreEqual(Array.Empty<string>(), sent, "Accept-Encoding sent: " + string.Join(", ", sent));
        }

        [TestMethod]
        public async Task THttpTransport_Injected_Client_Keeps_Callers_Accept_Encoding_Test()
        {
            var handler = new RecordingHandler();
            var client = new HttpClient(handler);
            client.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("gzip"));
            using var transport = new THttpTransport(client, new TConfiguration(), new Uri("http://localhost/"));

            var sent = await SendRequestAsync(transport, handler);
            CollectionAssert.AreEqual(new[] { "gzip" }, sent, "Accept-Encoding sent: " + string.Join(", ", sent));
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

        // ---- the timeout of the HttpClient also bounds every request of a call view -------------

        /// <summary>A server that never answers; only cancellation ends the request.</summary>
        private sealed class StalledServerHandler : HttpMessageHandler
        {
            protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                await Task.Delay(Timeout.Infinite, cancellationToken);
                return new HttpResponseMessage(HttpStatusCode.OK);
            }
        }

        private sealed class TestClient : TBaseClient, IDisposable
        {
            public TestClient(ICallIsolatedTransport transport) : base(transport, new Thrift.Protocol.TBinaryProtocol.Factory()) { }
        }

        [TestMethod]
        public async Task Http_Client_Timeout_Is_Reported_As_TimedOut()
        {
            using var shared = new THttpTransport(new HttpClient(new StalledServerHandler()), null, TestUri) { ConnectTimeout = 100 };
            using var call = shared.CreateCallTransport();

            // nobody cancels: only the timeout of the HttpClient can end the request
            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => WriteRequestAsync(call, "x"));
            Assert.AreEqual(TTransportException.ExceptionType.TimedOut, ex.Type);
        }

        [TestMethod]
        public async Task Caller_Cancellation_Is_Still_Reported_As_Interrupted()
        {
            using var shared = new THttpTransport(new HttpClient(new StalledServerHandler()), null, TestUri) { ConnectTimeout = 60000 };
            using var call = shared.CreateCallTransport();
            using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(100));

            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => WriteRequestAsync(call, "x", cts.Token));
            Assert.AreEqual(TTransportException.ExceptionType.Interrupted, ex.Type);
        }

        [TestMethod]
        public async Task Longer_Per_Call_Timeout_Does_Not_Extend_The_Http_Client_Timeout()
        {
            using var shared = new THttpTransport(new HttpClient(new StalledServerHandler()), null, TestUri)
            {
                ConnectTimeout = 100,
                PerCallTimeout = TimeSpan.FromMinutes(10),
            };
            using var client = new TestClient(shared);
            using var context = await client.CreateCallContextAsync();

            // the call's own token (linked to the per-call timeout) is not cancelled; the HttpClient timeout is
            var started = DateTime.UtcNow;
            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => WriteRequestAsync(context.Protocol.Transport, "x", context.CancellationToken));
            Assert.AreEqual(TTransportException.ExceptionType.TimedOut, ex.Type);
            Assert.IsTrue(DateTime.UtcNow - started < TimeSpan.FromSeconds(30), "the per-call timeout must not have extended the HttpClient timeout");
        }

        [TestMethod]
        public async Task Late_Response_Is_Released_When_The_View_Was_Disposed_While_The_Request_Was_Pending()
        {
            var handler = new EchoHandler();
            var release = new TaskCompletionSource();
            var arrived = new TaskCompletionSource();
            handler.Gate = async _ =>
            {
                arrived.TrySetResult();
                await release.Task;
            };

            using var shared = new THttpTransport(new HttpClient(handler), null, TestUri);
            var call = shared.CreateCallTransport();

            var flush = WriteRequestAsync(call, "A");
            await arrived.Task;
            call.Dispose();          // the caller gave up while the request is on its way
            release.SetResult();     // and the server answers afterwards

            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => flush);
            Assert.AreEqual(TTransportException.ExceptionType.NotOpen, ex.Type);
            Assert.HasCount(1, handler.Responses);
            Assert.IsTrue(handler.Responses[0].IsDisposed, "the late response must be released with the view");
        }

        [TestMethod]
        public async Task Small_Reply_Is_Read_Through_A_Buffered_Layer_When_The_Size_Limit_Is_Below_The_Buffer_Size()
        {
            // the buffered layer asks the call view for a full buffer (1024 bytes), the reply is 4 bytes
            using var shared = new THttpTransport(new HttpClient(new EchoHandler()), new TConfiguration { MaxMessageSize = 100 }, TestUri);
            using var layer = new TIsolatedBufferedTransport(shared);
            using var view = layer.CreateCallTransport();

            await WriteRequestAsync(view, "A");
            Assert.AreEqual("re:A", await ReadResponseAsync(view, 4));
        }

        [TestMethod]
        public async Task Reply_Above_The_Size_Limit_Is_Still_Rejected_Through_A_Buffered_Layer()
        {
            using var shared = new THttpTransport(new HttpClient(new EchoHandler()), new TConfiguration { MaxMessageSize = 100 }, TestUri);
            using var layer = new TIsolatedBufferedTransport(shared);
            using var view = layer.CreateCallTransport();

            var request = new string('x', 200);                 // the echo is 203 bytes
            await WriteRequestAsync(view, request);
            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => ReadResponseAsync(view, 203));
            Assert.AreEqual(TTransportException.ExceptionType.MessageSizeLimit, ex.Type);
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
