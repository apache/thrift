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
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Runtime.ExceptionServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NSubstitute;
using Thrift.Collections;
using Thrift.Processor;
using Thrift.Protocol;
using Thrift.Protocol.Entities;
using Thrift.Server;
using Thrift.Transport;
using Thrift.Transport.Client;
using Thrift.Transport.Server;
using ThriftTest;
using ThriftTestService = ThriftTest.ThriftTest;

namespace Thrift.Tests.Async
{
    // An application that blocks on a client call (.Result, .Wait(), GetAwaiter().GetResult()) on a
    // thread with a single-threaded SynchronizationContext, such as a UI thread, must still get its
    // result. That thread is blocked, so no await in the library or in the generated code may wait
    // for it: none of them may post its continuation to the caller's context.
    //
    // The client, processor and types are the ones generated from test/ThriftTest.thrift into
    // Thrift.Compile.net11.
    [TestClass]
    public class SynchronizationContextTests
    {
        public enum ProtocolKind { Binary, Compact, Json }

        public enum LayerKind { None, Buffered, Framed }

        public enum CallKind { String, Insanity, DeclaredException, ApplicationException }

        public enum ServerKind { Simple, ThreadPool }

        private const string Reply = "pong";
        private const int XceptionCode = 1001;
        private const string ServiceName = "ThriftTest";

        // Generous on purpose: a call takes milliseconds, the wait only matters when it hangs.
        private static readonly TimeSpan HangTimeout = TimeSpan.FromSeconds(10);

        // The stream completes every read, write and flush asynchronously, the way a socket does. Without a
        // layer, every read and write of the protocol and of the generated code reaches the stream.
        [TestMethod]
        [DataRow(ProtocolKind.Binary, LayerKind.None, CallKind.String)]
        [DataRow(ProtocolKind.Binary, LayerKind.Buffered, CallKind.String)]
        [DataRow(ProtocolKind.Binary, LayerKind.Framed, CallKind.String)]
        [DataRow(ProtocolKind.Compact, LayerKind.None, CallKind.String)]
        [DataRow(ProtocolKind.Compact, LayerKind.Buffered, CallKind.String)]
        [DataRow(ProtocolKind.Compact, LayerKind.Framed, CallKind.String)]
        [DataRow(ProtocolKind.Json, LayerKind.None, CallKind.String)]
        [DataRow(ProtocolKind.Json, LayerKind.Buffered, CallKind.String)]
        [DataRow(ProtocolKind.Json, LayerKind.Framed, CallKind.String)]
        [DataRow(ProtocolKind.Binary, LayerKind.None, CallKind.Insanity)]
        [DataRow(ProtocolKind.Compact, LayerKind.None, CallKind.Insanity)]
        [DataRow(ProtocolKind.Json, LayerKind.None, CallKind.Insanity)]
        [DataRow(ProtocolKind.Binary, LayerKind.None, CallKind.DeclaredException)]
        [DataRow(ProtocolKind.Compact, LayerKind.None, CallKind.DeclaredException)]
        [DataRow(ProtocolKind.Json, LayerKind.None, CallKind.DeclaredException)]
        [DataRow(ProtocolKind.Binary, LayerKind.None, CallKind.ApplicationException)]
        [DataRow(ProtocolKind.Compact, LayerKind.None, CallKind.ApplicationException)]
        [DataRow(ProtocolKind.Json, LayerKind.None, CallKind.ApplicationException)]
        public async Task BlockingClientCall_OnSingleThreadedContext_Completes(ProtocolKind protocol, LayerKind layer, CallKind call)
        {
            var stream = new AsyncOnlyStream(await EncodeReplyAsync(protocol, layer, call));
            var transport = MakeLayer(layer, new TStreamTransport(stream, stream, new TConfiguration()));
            using var client = new ThriftTestService.Client(MakeProtocol(protocol, transport));
            var context = new BlockedThreadContext();

            object? outcome = null;
            var completed = context.RunBlocking(() => outcome = CallBlocking(client, call), HangTimeout);

            // Without an asynchronous completion there is nothing to post, and the test would prove nothing.
            Assert.IsTrue(stream.Suspensions > 0, "the stream never completed asynchronously");
            Assert.IsTrue(completed, $"the call hung: {context.Posted} continuation(s) waited for the blocked thread");
            Assert.AreEqual(0, context.Posted, "a continuation was posted to the caller's SynchronizationContext");
            AssertOutcome(call, outcome);
        }

        // A UI thread that hosts a server and then calls it with a blocking client call must get its
        // result too. With the thread blocked, the server can only answer if none of its awaits waits for
        // that thread either, so the processor and the handler run on the thread pool.
        [TestMethod]
        [DataRow(ServerKind.Simple, false)]
        [DataRow(ServerKind.Simple, true)]
        [DataRow(ServerKind.ThreadPool, false)]
        [DataRow(ServerKind.ThreadPool, true)]
        public async Task BlockingCallToOwnServer_OnSingleThreadedContext_Completes(ServerKind kind, bool multiplexed)
        {
            var handlerCalls = 0;
            SynchronizationContext? handlerContext = null;
            var handler = Substitute.For<ThriftTestService.IAsync>();
            handler.testString(Arg.Any<string?>(), Arg.Any<CancellationToken>()).Returns(_ =>
            {
                handlerContext = SynchronizationContext.Current;
                Interlocked.Increment(ref handlerCalls);
                return Task.FromResult(Reply);
            });

            ITAsyncProcessor processor = new ThriftTestService.AsyncProcessor(handler);
            if (multiplexed)
            {
                var multiplexer = new TMultiplexedProcessor();
                multiplexer.RegisterProcessor(ServiceName, processor);
                processor = multiplexer;
            }

            var serverTransport = new TServerSocketTransport(new TcpListener(IPAddress.Loopback, 0), new TConfiguration());
            var protocolFactory = new TBinaryProtocol.Factory();
            TServer server = kind switch
            {
                ServerKind.Simple => new TSimpleAsyncServer(new TSingletonProcessorFactory(processor), serverTransport,
                    null, null, protocolFactory, protocolFactory, (ILogger?)null),
                ServerKind.ThreadPool => new TThreadPoolAsyncServer(processor, serverTransport, new TTransportFactory(), protocolFactory),
                _ => throw new ArgumentOutOfRangeException(nameof(kind)),
            };

            using var cancel = new CancellationTokenSource();
            var context = new BlockedThreadContext();
            Task? serving = null;
            string? result = null;
            var completed = context.RunBlocking(() =>
            {
                // ServeAsync() returns at the first await that has to wait, the accept.
                serving = server.ServeAsync(cancel.Token);
                TProtocol protocol = new TBinaryProtocol(
                    new TSocketTransport(IPAddress.Loopback, serverTransport.GetPort(), new TConfiguration()));
                if (multiplexed)
                    protocol = new TMultiplexedProtocol(protocol, ServiceName);
                using var client = new ThriftTestService.Client(protocol);
                client.OpenTransportAsync().GetAwaiter().GetResult();
                result = client.testString("ping").GetAwaiter().GetResult();
            }, HangTimeout);

            cancel.Cancel();
            server.Stop();
            if (serving != null)
            {
                await Task.WhenAny(serving, Task.Delay(HangTimeout));
                if (serving.IsFaulted)
                    _ = serving.Exception;  // the loop may end with the cancellation, which is fine here
            }

            Assert.AreEqual(1, handlerCalls, "the request did not reach the handler");
            Assert.IsTrue(completed, $"the call hung: {context.Posted} continuation(s) waited for the blocked thread");
            Assert.AreEqual(0, context.Posted, "a continuation was posted to the caller's SynchronizationContext");
            Assert.AreNotSame(context, handlerContext, "the handler ran on the caller's SynchronizationContext");
            Assert.AreEqual(Reply, result);
        }

        // Makes the call the way an application on a UI thread would, and returns the result or the exception.
        private static object? CallBlocking(ThriftTestService.Client client, CallKind call)
        {
            try
            {
                switch (call)
                {
                    case CallKind.Insanity:
                        return client.testInsanity(MakeInsanity()).GetAwaiter().GetResult();
                    case CallKind.DeclaredException:
                        client.testException("Xception").GetAwaiter().GetResult();
                        return null;
                    case CallKind.String:
                    case CallKind.ApplicationException:
                        return client.testString("ping").GetAwaiter().GetResult();
                    default:
                        throw new ArgumentOutOfRangeException(nameof(call));
                }
            }
            catch (Exception ex)
            {
                return ex;
            }
        }

        private static void AssertOutcome(CallKind call, object? outcome)
        {
            switch (call)
            {
                case CallKind.String:
                    Assert.AreEqual(Reply, outcome);
                    break;
                case CallKind.Insanity:
                    Assert.IsTrue(TCollections.Equals(MakeInsanityReply(), outcome as IEnumerable), $"unexpected result: {outcome}");
                    break;
                case CallKind.DeclaredException:
                    Assert.IsInstanceOfType(outcome, typeof(Xception));
                    Assert.AreEqual(XceptionCode, ((Xception)outcome).ErrorCode);
                    break;
                case CallKind.ApplicationException:
                    Assert.IsInstanceOfType(outcome, typeof(TApplicationException));
                    Assert.AreEqual(TApplicationException.ExceptionType.InternalError, ((TApplicationException)outcome).Type);
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(call));
            }
        }

        private static TProtocol MakeProtocol(ProtocolKind kind, TTransport transport) => kind switch
        {
            ProtocolKind.Binary => new TBinaryProtocol(transport),
            ProtocolKind.Compact => new TCompactProtocol(transport),
            ProtocolKind.Json => new TJsonProtocol(transport),
            _ => throw new ArgumentOutOfRangeException(nameof(kind)),
        };

        private static TTransport MakeLayer(LayerKind kind, TTransport transport) => kind switch
        {
            LayerKind.None => transport,
            LayerKind.Buffered => new TBufferedTransport(transport),
            LayerKind.Framed => new TFramedTransport(transport),
            _ => throw new ArgumentOutOfRangeException(nameof(kind)),
        };

        private static Insanity MakeInsanity() => new()
        {
            UserMap = new Dictionary<Numberz, long> { [Numberz.FIVE] = 5, [Numberz.EIGHT] = 8 },
            Xtructs = new List<Xtruct>
            {
                new() { String_thing = "Goodbye4", Byte_thing = 4, I32_thing = 4, I64_thing = 4 },
                new() { String_thing = "Hello2", Byte_thing = 2, I32_thing = 2, I64_thing = 2 },
            },
        };

        private static Dictionary<long, Dictionary<Numberz, Insanity>> MakeInsanityReply() => new()
        {
            [1] = new Dictionary<Numberz, Insanity> { [Numberz.TWO] = MakeInsanity(), [Numberz.THREE] = MakeInsanity() },
            [2] = new Dictionary<Numberz, Insanity> { [Numberz.SIX] = new Insanity() },
        };

        // The server's answer to the call, encoded with the protocol and layer the client uses.
        private static async Task<byte[]> EncodeReplyAsync(ProtocolKind protocol, LayerKind layer, CallKind call)
        {
            var ct = CancellationToken.None;
            using var buffer = new MemoryStream();
            var transport = MakeLayer(layer, new TStreamTransport(null, buffer, new TConfiguration()));
            var prot = MakeProtocol(protocol, transport);
            switch (call)
            {
                case CallKind.String:
                    await prot.WriteMessageBeginAsync(new TMessage("testString", TMessageType.Reply, 0), ct);
                    await new ThriftTestService.InternalStructs.testString_result { Success = Reply }.WriteAsync(prot, ct);
                    break;
                case CallKind.Insanity:
                    await prot.WriteMessageBeginAsync(new TMessage("testInsanity", TMessageType.Reply, 0), ct);
                    await new ThriftTestService.InternalStructs.testInsanity_result { Success = MakeInsanityReply() }.WriteAsync(prot, ct);
                    break;
                case CallKind.DeclaredException:
                    await prot.WriteMessageBeginAsync(new TMessage("testException", TMessageType.Reply, 0), ct);
                    await new ThriftTestService.InternalStructs.testException_result
                    {
                        Err1 = new Xception { ErrorCode = XceptionCode, Message = "Xception" },
                    }.WriteAsync(prot, ct);
                    break;
                case CallKind.ApplicationException:
                    await prot.WriteMessageBeginAsync(new TMessage("testString", TMessageType.Exception, 0), ct);
                    await new TApplicationException(TApplicationException.ExceptionType.InternalError, "the handler failed").WriteAsync(prot, ct);
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(call));
            }
            await prot.WriteMessageEndAsync(ct);
            await transport.FlushAsync(ct);
            return buffer.ToArray();
        }

        // Stands in for a UI thread: a context bound to one thread, which runs posted work only while
        // that thread is free. RunBlocking() keeps the thread blocked on the call, so posted work waits
        // forever. After the timeout the queued work runs on the calling thread instead, so that a hung
        // call can finish and the test does not leave a blocked thread behind.
        private sealed class BlockedThreadContext : SynchronizationContext
        {
            private readonly BlockingCollection<(SendOrPostCallback Callback, object? State)> _queue = new();
            private int _posted;

            public int Posted => Volatile.Read(ref _posted);

            public override void Post(SendOrPostCallback d, object? state)
            {
                Interlocked.Increment(ref _posted);
                _queue.Add((d, state));
            }

            public override void Send(SendOrPostCallback d, object? state)
                => throw new NotSupportedException("the thread of this context is blocked");

            public override SynchronizationContext CreateCopy() => this;

            public bool RunBlocking(Action call, TimeSpan timeout)
            {
                ExceptionDispatchInfo? error = null;
                var thread = new Thread(() =>
                {
                    SetSynchronizationContext(this);
                    try
                    {
                        call();
                    }
                    catch (Exception ex)
                    {
                        error = ExceptionDispatchInfo.Capture(ex);
                    }
                })
                { IsBackground = true };
                thread.Start();

                var completed = thread.Join(timeout);

                var deadline = DateTime.UtcNow + timeout;
                while (!thread.Join(0) && DateTime.UtcNow < deadline)
                {
                    if (_queue.TryTake(out var work, 50))
                        work.Callback(work.State);
                }

                error?.Throw();
                return completed;
            }
        }

        // Stands in for a socket: every read, write and flush completes asynchronously, on the thread
        // pool. Reads return the encoded reply, and the written request is dropped.
        private sealed class AsyncOnlyStream : Stream
        {
            private readonly MemoryStream _reply;
            private int _suspensions;

            public AsyncOnlyStream(byte[] reply)
            {
                _reply = new MemoryStream(reply, writable: false);
            }

            public int Suspensions => Volatile.Read(ref _suspensions);

            private async Task YieldToThreadPoolAsync()
            {
                Interlocked.Increment(ref _suspensions);
                await Task.CompletedTask.ConfigureAwait(ConfigureAwaitOptions.ForceYielding);
            }

            public override async ValueTask<int> ReadAsync(Memory<byte> buffer, CancellationToken cancellationToken = default)
            {
                await YieldToThreadPoolAsync().ConfigureAwait(false);
                return _reply.Read(buffer.Span);
            }

            public override Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken)
                => ReadAsync(buffer.AsMemory(offset, count), cancellationToken).AsTask();

            public override async ValueTask WriteAsync(ReadOnlyMemory<byte> buffer, CancellationToken cancellationToken = default)
                => await YieldToThreadPoolAsync().ConfigureAwait(false);

            public override Task WriteAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken)
                => YieldToThreadPoolAsync();

            public override Task FlushAsync(CancellationToken cancellationToken)
                => YieldToThreadPoolAsync();

            // TStreamTransport uses only the asynchronous members.
            public override int Read(byte[] buffer, int offset, int count) => throw new NotSupportedException();
            public override void Write(byte[] buffer, int offset, int count) => throw new NotSupportedException();
            public override void Flush() => throw new NotSupportedException();
            public override long Seek(long offset, SeekOrigin origin) => throw new NotSupportedException();
            public override void SetLength(long value) => throw new NotSupportedException();
            public override bool CanRead => true;
            public override bool CanSeek => false;
            public override bool CanWrite => true;
            public override long Length => throw new NotSupportedException();

            public override long Position
            {
                get => throw new NotSupportedException();
                set => throw new NotSupportedException();
            }
        }
    }
}
