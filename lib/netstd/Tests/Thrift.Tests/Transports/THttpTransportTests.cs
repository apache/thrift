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
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Protocol.Entities;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Transports
{
    [TestClass]
    public class THttpTransportTests
    {
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
        public void ClosedTHttpTransportReportsNotOpen()
        {
            using var transport = CreateTransport(new TrackingHandler(_ => CreateResponse("ok")));

            transport.Close();

            Assert.IsFalse(transport.IsOpen);
        }

        [TestMethod]
        public async Task ClosedTHttpTransportRejectsWrites()
        {
            using var transport = CreateTransport(new TrackingHandler(_ => CreateResponse("ok")));

            transport.Close();

            await Assert.ThrowsAsync<TTransportException>(
                () => transport.WriteAsync(new byte[] { 1 }, CancellationToken.None));
        }

        [TestMethod]
        public async Task ClosedTransportsRejectOpenAsync()
        {
            using var parent = CreateTransport(new TrackingHandler(_ => CreateResponse("ok")));
            var call = await parent.CreatePerCallTransportAsync();
            call.Dispose();
            parent.Close();

            await Assert.ThrowsAsync<TTransportException>(
                () => parent.OpenAsync(CancellationToken.None));
            await Assert.ThrowsAsync<TTransportException>(
                () => call.OpenAsync(CancellationToken.None));
        }

        [TestMethod]
        public async Task ConnectTimeout_StopsBlockedResponseBodyRead()
        {
            using var parent = CreateTransport(new BlockingResponseHandler());
            parent.ConnectTimeout = 50;
            await parent.WriteAsync(new[] { (byte)1 }, CancellationToken.None);
            await parent.FlushAsync(CancellationToken.None);

            var buffer = new byte[1];
            var ex = await Assert.ThrowsAsync<TTransportException>(() => parent.ReadAsync(buffer, 0, 1, CancellationToken.None).AsTask());
            Assert.AreEqual(TTransportException.ExceptionType.Interrupted, ex.Type);
        }

        [TestMethod]
        public async Task DisposedHttpClientDuringFlushIsReportedAsNotOpen()
        {
            using var parent = CreateTransport(new DisposedHttpClientHandler());
            await parent.WriteAsync(new byte[] { 1 }, CancellationToken.None);

            var ex = await Assert.ThrowsAsync<TTransportException>(
                () => parent.FlushAsync(CancellationToken.None));

            Assert.AreEqual(TTransportException.ExceptionType.NotOpen, ex.Type);
        }

        [TestMethod]
        public async Task PerCallTransport_ConnectTimeout_StopsBlockedResponseBodyRead()
        {
            using var parent = CreateTransport(new BlockingResponseHandler());
            parent.ConnectTimeout = 50;
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new[] { (byte)1 }, CancellationToken.None);
            await call.FlushAsync(CancellationToken.None);

            var buffer = new byte[1];
            var ex = await Assert.ThrowsAsync<TTransportException>(() => call.ReadAsync(buffer, 0, 1, CancellationToken.None).AsTask());
            Assert.AreEqual(TTransportException.ExceptionType.Interrupted, ex.Type);
            call.Dispose();
        }

        [TestMethod]
        public async Task CallerCancellationDuringResponseReadIsNotTranslatedToTransportTimeout()
        {
            using (var parent = CreateTransport(new BlockingResponseHandler()))
            {
                await parent.WriteAsync(new byte[] { 1 }, CancellationToken.None);
                await parent.FlushAsync(CancellationToken.None);

                using var cancellation = new CancellationTokenSource();
                var readTask = parent.ReadAsync(new byte[1], 0, 1, cancellation.Token).AsTask();
                cancellation.Cancel();
                await Assert.ThrowsAsync<OperationCanceledException>(() => readTask);
            }

            using (var parent = CreateTransport(new BlockingResponseHandler()))
            {
                var call = await parent.CreatePerCallTransportAsync();
                await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);
                await call.FlushAsync(CancellationToken.None);

                using var cancellation = new CancellationTokenSource();
                var readTask = call.ReadAsync(new byte[1], 0, 1, cancellation.Token).AsTask();
                cancellation.Cancel();
                await Assert.ThrowsAsync<OperationCanceledException>(() => readTask);
                call.Dispose();
            }
        }

        [TestMethod]
        public async Task ClosingTransportCancelsActiveResponseReads()
        {
            using (var parent = CreateTransport(new BlockingResponseHandler()))
            {
                await parent.WriteAsync(new byte[] { 1 }, CancellationToken.None);
                await parent.FlushAsync(CancellationToken.None);
                var readTask = parent.ReadAsync(new byte[1], 0, 1, CancellationToken.None).AsTask();

                parent.Close();

                var ex = await Assert.ThrowsAsync<TTransportException>(() => readTask);
                Assert.AreEqual(TTransportException.ExceptionType.Interrupted, ex.Type);
            }

            using (var parent = CreateTransport(new BlockingResponseHandler()))
            {
                var call = await parent.CreatePerCallTransportAsync();
                await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);
                await call.FlushAsync(CancellationToken.None);
                var readTask = call.ReadAsync(new byte[1], 0, 1, CancellationToken.None).AsTask();

                call.Dispose();

                var ex = await Assert.ThrowsAsync<TTransportException>(() => readTask);
                Assert.AreEqual(TTransportException.ExceptionType.Interrupted, ex.Type);
            }
        }

        [TestMethod]
        public async Task CreatePerCallTransportAsync_CreatesDistinctTransports()
        {
            using var parent = CreateTransport(new TrackingHandler(_ => CreateResponse("ok")));
            var first = await parent.CreatePerCallTransportAsync();
            var second = await parent.CreatePerCallTransportAsync();

            Assert.AreNotSame(first, second);
            first.Dispose();
            second.Dispose();
        }

        [TestMethod]
        public async Task PerCallTransport_SendsReadsAndDisposesResponseAndRequestResources()
        {
            var responseContent = new TrackingContent("response");
            Stream? requestStream = null;
            var handler = new TrackingHandler(req =>
            {
                // Capture the request stream while the HttpRequestMessage/StreamContent are
                // still alive, i.e. during the send itself.
                requestStream = req.Content!.ReadAsStreamAsync().GetAwaiter().GetResult();
                return new HttpResponseMessage(HttpStatusCode.OK) { Content = responseContent };
            });
            using var parent = CreateTransport(handler);
            var call = await parent.CreatePerCallTransportAsync();

            await call.WriteAsync(new byte[] { 1, 2, 3 }, CancellationToken.None);
            await call.FlushAsync(CancellationToken.None);

            // The HttpRequestMessage/StreamContent used to send the request are disposed
            // deterministically as soon as the request has been sent, rather than being left
            // for the GC.
            await Assert.ThrowsAsync<ObjectDisposedException>(() => handler.Requests[0].Content!.ReadAsStreamAsync());

            // The underlying request stream is owned by the per-call transport (not the
            // now-disposed HttpRequestMessage/StreamContent) and remains usable until the call
            // itself is disposed.
            Assert.AreEqual(1, requestStream!.ReadByte());

            var buffer = new byte[8];
            var count = await call.ReadAsync(buffer, 0, buffer.Length, CancellationToken.None);
            Assert.AreEqual(8, count);
            Assert.AreEqual("response", System.Text.Encoding.UTF8.GetString(buffer));

            call.Dispose();

            Assert.IsTrue(responseContent.IsDisposed);
            Assert.ThrowsExactly<ObjectDisposedException>(() => requestStream!.ReadByte());
        }

        [TestMethod]
        public async Task PerCallTransport_FailedResponse_DisposesResponse()
        {
            var responseContent = new TrackingContent("failure");
            using var parent = CreateTransport(new TrackingHandler(_ => new HttpResponseMessage(HttpStatusCode.InternalServerError) { Content = responseContent }));
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);

            await Assert.ThrowsAsync<TTransportException>(() => call.FlushAsync(CancellationToken.None));
            Assert.IsTrue(responseContent.IsDisposed);
            call.Dispose();
        }

        [TestMethod]
        public async Task PerCallTransport_RewindsSeekableResponseStream()
        {
            var responseStream = new MemoryStream(System.Text.Encoding.UTF8.GetBytes("response"), writable: false);
            responseStream.Position = 2;
            var responseContent = new TrackingContent(responseStream);
            using var parent = CreateTransport(new TrackingHandler(_ =>
                new HttpResponseMessage(HttpStatusCode.OK) { Content = responseContent }));
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);
            await call.FlushAsync(CancellationToken.None);

            var buffer = new byte[8];
            var count = await call.ReadAsync(buffer, 0, buffer.Length, CancellationToken.None);

            Assert.AreEqual(8, count);
            Assert.AreEqual("response", System.Text.Encoding.UTF8.GetString(buffer));
            call.Dispose();
        }

        [TestMethod]
        public async Task PerCallTransport_Cancellation_DoesNotLeakResponse()
        {
            var handler = new TrackingHandler(_ => CreateResponse("response"));
            using var parent = CreateTransport(handler);
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);
            using var cancellation = new CancellationTokenSource();
            cancellation.Cancel();

            await Assert.ThrowsAsync<TTransportException>(() => call.FlushAsync(cancellation.Token));
            Assert.AreEqual(0, handler.Requests.Count);
            call.Dispose();
        }

        [TestMethod]
        public async Task DisposingPerCallTransport_DoesNotCloseParentHttpClient()
        {
            var handler = new TrackingHandler(_ => CreateResponse("ok"));
            using var parent = CreateTransport(handler);
            var first = await parent.CreatePerCallTransportAsync();
            await first.WriteAsync(new byte[] { 1 }, CancellationToken.None);
            await first.FlushAsync(CancellationToken.None);
            first.Dispose();

            var second = await parent.CreatePerCallTransportAsync();
            await second.WriteAsync(new byte[] { 2 }, CancellationToken.None);
            await second.FlushAsync(CancellationToken.None);
            second.Dispose();

            Assert.AreEqual(2, handler.Requests.Count);
            Assert.IsTrue(parent.IsOpen);
        }

        [TestMethod]
        public async Task SharedTransport_RemainsUsableAcrossMultipleCalls()
        {
            var handler = new TrackingHandler(_ => CreateResponse("ok"));
            using var parent = CreateTransport(handler);
            await parent.WriteAsync(new byte[] { 1 }, CancellationToken.None);
            await parent.FlushAsync(CancellationToken.None);
            await parent.WriteAsync(new byte[] { 2 }, CancellationToken.None);
            await parent.FlushAsync(CancellationToken.None);
            Assert.AreEqual(2, handler.Requests.Count);
            Assert.IsTrue(parent.IsOpen);
        }

        [TestMethod]
        public async Task PerCallTransport_Disposal_DoesNotDisposeSharedTransport()
        {
            var handler = new TrackingHandler(_ => CreateResponse("ok"));
            var parent = CreateTransport(handler);
            var call = await parent.CreatePerCallTransportAsync();

            call.Dispose();

            Assert.IsFalse(handler.IsDisposed);
            Assert.IsTrue(parent.IsOpen);
            parent.Dispose();
            Assert.IsTrue(handler.IsDisposed);
        }

        [TestMethod]
        public async Task PerCallTransport_ReadFailure_IsDisposedByCallOwner()
        {
            var responseContent = new TrackingContent(new ThrowingReadStream());
            using var parent = CreateTransport(new TrackingHandler(_ =>
                new HttpResponseMessage(HttpStatusCode.OK) { Content = responseContent }));
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);
            await call.FlushAsync(CancellationToken.None);
            var ex = await Assert.ThrowsAsync<TTransportException>(() => call.ReadAsync(new byte[1], 0, 1, CancellationToken.None).AsTask());
            Assert.AreEqual(TTransportException.ExceptionType.Unknown, ex.Type);
            Assert.IsInstanceOfType<IOException>(ex.InnerException);
            call.Dispose();
            Assert.IsFalse(call.IsOpen);
            Assert.IsTrue(responseContent.IsDisposed);
            Assert.IsTrue(parent.IsOpen);
        }

        [TestMethod]
        public async Task PerCallTransport_ReadFailureAfterFlush_DisposesOwnedResponse()
        {
            var responseContent = new TrackingContent(new ThrowingReadStream());
            using var parent = CreateTransport(new TrackingHandler(_ =>
                new HttpResponseMessage(HttpStatusCode.OK) { Content = responseContent }));
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);

            await call.FlushAsync(CancellationToken.None);
            var ex = await Assert.ThrowsAsync<TTransportException>(() => call.ReadAsync(new byte[1], 0, 1, CancellationToken.None).AsTask());
            Assert.AreEqual(TTransportException.ExceptionType.Unknown, ex.Type);
            Assert.IsInstanceOfType<IOException>(ex.InnerException);
            call.Dispose();

            Assert.IsFalse(call.IsOpen);
            Assert.IsTrue(responseContent.IsDisposed);
        }

        [TestMethod]
        public async Task PerCallTransport_StreamInitializationFailure_PreventsRetryOnSameCall()
        {
            var failingContent = new ThrowingStreamInitContent();
            var handler = new TrackingHandler(_ =>
                new HttpResponseMessage(HttpStatusCode.OK) { Content = failingContent });
            using var parent = CreateTransport(handler);
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);

            var ex = await Assert.ThrowsAsync<TTransportException>(() => call.FlushAsync(CancellationToken.None));
            Assert.AreEqual(TTransportException.ExceptionType.Unknown, ex.Type);
            Assert.IsInstanceOfType<IOException>(ex.InnerException);
            Assert.IsTrue(failingContent.IsDisposed);

            await Assert.ThrowsAsync<InvalidOperationException>(() => call.FlushAsync(CancellationToken.None));
            Assert.AreEqual(1, handler.Requests.Count);
            call.Dispose();
        }

        [TestMethod]
        public async Task ConcurrentPerCallTransports_CompleteIndependently()
        {
            var handler = new IndependentResponseHandler();
            using var parent = CreateTransport(handler);
            var first = await parent.CreatePerCallTransportAsync();
            var second = await parent.CreatePerCallTransportAsync();
            var firstTask = SendAndReadAsync(first, 1);
            var secondTask = SendAndReadAsync(second, 2);
            await handler.RequestsStarted.Task;
            handler.Release(0);
            Assert.AreEqual("response-0", await firstTask);
            Assert.IsFalse(secondTask.IsCompleted);
            handler.Release(1);
            Assert.AreEqual("response-1", await secondTask);
            first.Dispose();
            second.Dispose();
        }

        [TestMethod]
        public async Task GeneratedClientUsesIndependentPerCallTransports()
        {
            var handler = new GeneratedClientHandler();
            using var transport = CreateTransport(handler);
            var protocolFactory = new TrackingBinaryProtocolFactory();
            using var client = new ThriftTest.ThriftTest.Client(transport, protocolFactory);

            var firstCall = client.testString("first", CancellationToken.None);
            var secondCall = client.testString("second", CancellationToken.None);
            await handler.BothRequestsStarted.Task;
            handler.ReleaseResponses();

            var responses = await Task.WhenAll(firstCall, secondCall);
            var requestBytes = handler.Requests.ToArray();
            Assert.AreEqual(2, requestBytes.Length);
            Assert.IsFalse(requestBytes[0].SequenceEqual(requestBytes[1]));
            Assert.AreEqual(6, protocolFactory.CreatedCount);
            CollectionAssert.AreEquivalent(new[] { "response-0", "response-1" }, responses);
        }

        [TestMethod]
        public async Task ProtocolFactory_IsInvokedPerCallWithPerCallTransport()
        {
            using var parent = CreateTransport(new TrackingHandler(_ => CreateResponse("ok")));
            var factory = new TrackingProtocolFactory();
            var firstTransport = await parent.CreatePerCallTransportAsync();
            using var firstProtocol = factory.GetProtocol(firstTransport);
            var secondTransport = await parent.CreatePerCallTransportAsync();
            using var secondProtocol = factory.GetProtocol(secondTransport);
            Assert.AreEqual(2, factory.Transports.Count);
            Assert.AreSame(firstTransport, factory.Transports[0]);
            Assert.AreSame(secondTransport, factory.Transports[1]);
        }

        [TestMethod]
        public void BaseClient_DisposesInputProtocolWhenOutputFactoryFails()
        {
            var transport = new TrackingMemoryBufferTransport();

            try
            {
                new TestBaseClient(
                    transport,
                    new TrackingBinaryProtocolFactory(),
                    new ThrowingProtocolFactory());
                Assert.Fail("The output protocol factory should have failed.");
            }
            catch (InvalidOperationException)
            {
            }

            Assert.IsTrue(transport.IsDisposed);
        }

        [TestMethod]
        public async Task ClosingParentDuringPerCallSendRejectsLateResponse()
        {
            var handler = new ReleaseResponseHandler();
            using var parent = CreateTransport(handler);
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);
            var flushTask = call.FlushAsync(CancellationToken.None);
            await handler.RequestStarted.Task;

            parent.Close();
            handler.Release();

            var ex = await Assert.ThrowsAsync<TTransportException>(() => flushTask);
            Assert.AreEqual(TTransportException.ExceptionType.NotOpen, ex.Type);
            Assert.IsFalse(call.IsOpen);
        }

        [TestMethod]
        public async Task DisposingPerCallTransportCancelsInFlightSend()
        {
            var handler = new CancellationHandler();
            using var parent = CreateTransport(handler);
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);
            var flushTask = call.FlushAsync(CancellationToken.None);
            await handler.RequestStarted.Task;

            call.Dispose();

            var ex = await Assert.ThrowsAsync<TTransportException>(() => flushTask);
            Assert.AreEqual(TTransportException.ExceptionType.Interrupted, ex.Type);
            await handler.CancellationObserved.Task;
        }

        [TestMethod]
        public async Task CancellationWhileRequestIsBlocked_ReleasesResponseAndParentRemainsUsable()
        {
            var handler = new CancellationHandler();
            using var parent = CreateTransport(handler);
            var call = await parent.CreatePerCallTransportAsync();
            await call.WriteAsync(new byte[] { 1 }, CancellationToken.None);
            using var cancellation = new CancellationTokenSource();
            var flushTask = call.FlushAsync(cancellation.Token);
            await handler.RequestStarted.Task;
            cancellation.Cancel();
            await Assert.ThrowsAsync<TTransportException>(() => flushTask);
            await handler.CancellationObserved.Task;
            call.Dispose();
            Assert.IsNull(handler.BlockedResponse);
            var followUp = await parent.CreatePerCallTransportAsync();
            await followUp.WriteAsync(new byte[] { 2 }, CancellationToken.None);
            await followUp.FlushAsync(CancellationToken.None);
            followUp.Dispose();
            Assert.IsTrue(parent.IsOpen);
        }

        private static async Task<string> SendAndReadAsync(TTransport transport, byte value)
        {
            await transport.WriteAsync(new[] { value }, CancellationToken.None);
            await transport.FlushAsync(CancellationToken.None);
            var buffer = new byte[10];
            var count = await transport.ReadAsync(buffer, 0, buffer.Length, CancellationToken.None);
            return System.Text.Encoding.UTF8.GetString(buffer, 0, count);
        }

        private static THttpTransport CreateTransport(HttpMessageHandler handler)
        {
            return new THttpTransport(new HttpClient(handler), null, new Uri("http://localhost/test"));
        }

        private static HttpResponseMessage CreateResponse(string value)
        {
            return new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent(value) };
        }

        private sealed class TrackingBinaryProtocolFactory : TProtocolFactory
        {
            private int _createdCount;
            public int CreatedCount => _createdCount;

            public override TProtocol GetProtocol(TTransport trans)
            {
                Interlocked.Increment(ref _createdCount);
                return new TBinaryProtocol(trans);
            }
        }

        private sealed class ThrowingProtocolFactory : TProtocolFactory
        {
            public override TProtocol GetProtocol(TTransport trans)
            {
                throw new InvalidOperationException("Synthetic protocol factory failure.");
            }
        }

        private sealed class TestBaseClient : TBaseClient
        {
            public TestBaseClient(
                TTransport transport,
                TProtocolFactory inputProtocolFactory,
                TProtocolFactory outputProtocolFactory)
                : base(transport, inputProtocolFactory, outputProtocolFactory)
            {
            }
        }

        private sealed class TrackingMemoryBufferTransport : TMemoryBufferTransport
        {
            public TrackingMemoryBufferTransport()
                : base(new TConfiguration())
            {
            }

            public bool IsDisposed { get; private set; }

            protected override void Dispose(bool disposing)
            {
                IsDisposed = true;
                base.Dispose(disposing);
            }
        }

        private sealed class GeneratedClientHandler : HttpMessageHandler
        {
            private readonly TaskCompletionSource<bool> _release = NewSignal();
            private int _requestCount;
            public ConcurrentQueue<byte[]> Requests { get; } = new ConcurrentQueue<byte[]>();
            public TaskCompletionSource<bool> BothRequestsStarted { get; } = NewSignal();

            protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                var requestBytes = await request.Content!.ReadAsByteArrayAsync(cancellationToken);
                Requests.Enqueue(requestBytes);
                var index = Interlocked.Increment(ref _requestCount) - 1;
                if (index == 1)
                {
                    BothRequestsStarted.TrySetResult(true);
                }
                await _release.Task.WaitAsync(cancellationToken);
                return new HttpResponseMessage(HttpStatusCode.OK)
                {
                    Content = new ByteArrayContent(CreateStringReply("response-" + index))
                };
            }

            public void ReleaseResponses() => _release.TrySetResult(true);

            private static byte[] CreateStringReply(string value)
            {
                var transport = new TMemoryBufferTransport(new TConfiguration());
                var protocol = new TBinaryProtocol(transport);
                protocol.WriteMessageBeginAsync(new TMessage("testString", TMessageType.Reply, 1), CancellationToken.None).GetAwaiter().GetResult();
                protocol.WriteStructBeginAsync(new TStruct("testString_result"), CancellationToken.None).GetAwaiter().GetResult();
                protocol.WriteFieldBeginAsync(new TField("success", TType.String, 0), CancellationToken.None).GetAwaiter().GetResult();
                protocol.WriteStringAsync(value, CancellationToken.None).GetAwaiter().GetResult();
                protocol.WriteFieldEndAsync(CancellationToken.None).GetAwaiter().GetResult();
                protocol.WriteFieldStopAsync(CancellationToken.None).GetAwaiter().GetResult();
                protocol.WriteStructEndAsync(CancellationToken.None).GetAwaiter().GetResult();
                protocol.WriteMessageEndAsync(CancellationToken.None).GetAwaiter().GetResult();
                var bytes = transport.GetBuffer();
                protocol.Dispose();
                return bytes;
            }
        }

        private sealed class TrackingHandler : HttpMessageHandler
        {
            private readonly Func<HttpRequestMessage, HttpResponseMessage> _responseFactory;
            public TrackingHandler(Func<HttpRequestMessage, HttpResponseMessage> responseFactory) => _responseFactory = responseFactory;
            public List<HttpRequestMessage> Requests { get; } = new List<HttpRequestMessage>();
            public bool IsDisposed { get; private set; }
            protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                cancellationToken.ThrowIfCancellationRequested();
                Requests.Add(request);
                return Task.FromResult(_responseFactory(request));
            }
            protected override void Dispose(bool disposing)
            {
                IsDisposed = true;
                base.Dispose(disposing);
            }
        }

        private class TrackingContent : HttpContent
        {
            private readonly byte[]? _bytes;
            private readonly Stream? _stream;
            public TrackingContent(string value) => _bytes = System.Text.Encoding.UTF8.GetBytes(value);
            public TrackingContent(Stream stream) => _stream = stream;
            public bool IsDisposed { get; private set; }
            protected override Task SerializeToStreamAsync(Stream stream, TransportContext? context) => stream.WriteAsync(_bytes ?? Array.Empty<byte>(), 0, _bytes?.Length ?? 0);
            protected override bool TryComputeLength(out long length) { length = _bytes?.Length ?? 0; return true; }
            protected override Task<Stream> CreateContentReadStreamAsync() => Task.FromResult(_stream ?? (Stream)new MemoryStream(_bytes ?? Array.Empty<byte>(), writable: false));
            protected override void Dispose(bool disposing) { IsDisposed = true; _stream?.Dispose(); base.Dispose(disposing); }
        }

        private sealed class ThrowingReadStream : MemoryStream
        {
            public override Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken) =>
                Task.FromException<int>(new IOException("Synthetic response read failure."));
        }

        private sealed class ThrowingStreamInitContent : HttpContent
        {
            public bool IsDisposed { get; private set; }
            protected override Task SerializeToStreamAsync(Stream stream, TransportContext? context) => Task.CompletedTask;
            protected override bool TryComputeLength(out long length) { length = 0; return true; }
            protected override Task<Stream> CreateContentReadStreamAsync() =>
                Task.FromException<Stream>(new IOException("Synthetic stream initialization failure."));
            protected override void Dispose(bool disposing) { IsDisposed = true; base.Dispose(disposing); }
        }

        private sealed class TrackingProtocolFactory : TProtocolFactory
        {
            public List<TTransport> Transports { get; } = new List<TTransport>();
            public override TProtocol GetProtocol(TTransport trans)
            {
                Transports.Add(trans);
                return new TBinaryProtocol(trans);
            }
        }

        private sealed class IndependentResponseHandler : HttpMessageHandler
        {
            private readonly TaskCompletionSource<bool>[] _releases = { NewSignal(), NewSignal() };
            private int _requestCount;
            public TaskCompletionSource<bool> RequestsStarted { get; } = NewSignal();
            protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                var index = Interlocked.Increment(ref _requestCount) - 1;
                if (index == 1) RequestsStarted.TrySetResult(true);
                await _releases[index].Task.WaitAsync(cancellationToken);
                return CreateResponse("response-" + index);
            }
            public void Release(int index) => _releases[index].TrySetResult(true);
        }

        private sealed class BlockingResponseHandler : HttpMessageHandler
        {
            protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                return Task.FromResult(new HttpResponseMessage(HttpStatusCode.OK)
                {
                    Content = new BlockingReadContent()
                });
            }
        }

        private sealed class DisposedHttpClientHandler : HttpMessageHandler
        {
            protected override Task<HttpResponseMessage> SendAsync(
                HttpRequestMessage request,
                CancellationToken cancellationToken)
            {
                throw new ObjectDisposedException("HttpClient");
            }
        }

        private sealed class BlockingReadContent : HttpContent
        {
            protected override Task SerializeToStreamAsync(Stream stream, TransportContext? context) => Task.CompletedTask;
            protected override bool TryComputeLength(out long length)
            {
                length = 0;
                return false;
            }
            protected override Task<Stream> CreateContentReadStreamAsync() => Task.FromResult<Stream>(new BlockingReadStream());
        }

        private sealed class BlockingReadStream : Stream
        {
            public override bool CanRead => true;
            public override bool CanSeek => false;
            public override bool CanWrite => false;
            public override long Length => throw new NotSupportedException();
            public override long Position { get => throw new NotSupportedException(); set => throw new NotSupportedException(); }
            public override void Flush() { }
            public override int Read(byte[] buffer, int offset, int count) => throw new NotSupportedException();
            public override long Seek(long offset, SeekOrigin origin) => throw new NotSupportedException();
            public override void SetLength(long value) => throw new NotSupportedException();
            public override void Write(byte[] buffer, int offset, int count) => throw new NotSupportedException();
            public override async Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken)
            {
                await Task.Delay(Timeout.Infinite, cancellationToken);
                return 0;
            }
            public override ValueTask<int> ReadAsync(Memory<byte> buffer, CancellationToken cancellationToken = default)
            {
                return new ValueTask<int>(Task.Delay(Timeout.Infinite, cancellationToken).ContinueWith(_ => 0, cancellationToken, TaskContinuationOptions.None, TaskScheduler.Default));
            }
        }

        private sealed class ReleaseResponseHandler : HttpMessageHandler
        {
            private readonly TaskCompletionSource<bool> _release = NewSignal();
            public TaskCompletionSource<bool> RequestStarted { get; } = NewSignal();

            protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                RequestStarted.TrySetResult(true);
                await _release.Task;
                return CreateResponse("late");
            }

            public void Release() => _release.TrySetResult(true);
        }

        private sealed class CancellationHandler : HttpMessageHandler
        {
            private readonly TaskCompletionSource<bool> _blocked = NewSignal();
            private int _requestCount;
            public TaskCompletionSource<bool> RequestStarted { get; } = NewSignal();
            public TaskCompletionSource<bool> CancellationObserved { get; } = NewSignal();
            public HttpResponseMessage? BlockedResponse { get; private set; }
            protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                if (Interlocked.Increment(ref _requestCount) > 1)
                    return CreateResponse("follow-up");
                RequestStarted.TrySetResult(true);
                try
                {
                    await _blocked.Task.WaitAsync(cancellationToken);
                    BlockedResponse = CreateResponse("unexpected");
                    return BlockedResponse;
                }
                catch (OperationCanceledException)
                {
                    CancellationObserved.TrySetResult(true);
                    throw;
                }
            }
        }

        private static TaskCompletionSource<bool> NewSignal() =>
            new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously);
    }
}
