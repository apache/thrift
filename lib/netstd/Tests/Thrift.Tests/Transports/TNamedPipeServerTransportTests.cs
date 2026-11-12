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
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NSubstitute;
using Thrift.Processor;
using Thrift.Protocol;
using Thrift.Server;
using Thrift.Transport;
using Thrift.Transport.Client;
using Thrift.Transport.Server;

namespace Thrift.Tests.Transports
{
    // AcceptAsync() returns to its caller while it waits for a client. It reports a cancelled token as the
    // TaskCanceledException the servers stop on, and a Close() during the wait as Interrupted. It hands out
    // only pipes that a client is connected to.
    [TestClass]
    public class TNamedPipeServerTransportTests
    {
        public enum ServerKind { Simple, ThreadPool }

        public enum StopKind { Token, Stop }

        // Every wait has a limit, because an AcceptAsync() that blocks its caller returns only once its token
        // is cancelled.
        private static readonly TimeSpan WaitLimit = TimeSpan.FromSeconds(10);
        private const int ConnectTimeoutMs = 10_000;

        // On Linux, the pipe is a Unix domain socket in the temp directory, whose path length is limited.
        private static string NewPipeName() => "thrift-" + Guid.NewGuid().ToString("N");

        private static TNamedPipeServerTransport NewServer(string pipeName, int listenPipes = 1)
        {
            var server = new TNamedPipeServerTransport(pipeName, new TConfiguration(), NamedPipeServerFlags.None, listenPipes);
            server.Listen();
            return server;
        }

        [TestMethod]
        public async Task AcceptAsync_NoClient_ReturnsPendingTask()
        {
            var server = NewServer(NewPipeName());
            try
            {
                using var cancel = new CancellationTokenSource(WaitLimit);
                var accept = server.AcceptAsync(cancel.Token);
                var pendingOnReturn = !accept.IsCompleted;

                cancel.Cancel();
                await Assert.ThrowsExactlyAsync<TaskCanceledException>(async () => await accept);
                Assert.IsTrue(pendingOnReturn, "AcceptAsync() returned only after the wait for a client had ended");
            }
            finally
            {
                server.Close();
            }
        }

        [TestMethod]
        public async Task AcceptAsync_TokenCancelledBeforeCall_ThrowsTaskCanceledException()
        {
            var server = NewServer(NewPipeName());
            try
            {
                using var cancel = new CancellationTokenSource();
                cancel.Cancel();
                Assert.AreEqual(nameof(TaskCanceledException), await OutcomeAsync(server.AcceptAsync(cancel.Token)));
            }
            finally
            {
                server.Close();
            }
        }

        // The token is cancelled while the pipe waits for a client, which is a race with that wait; hence the runs.
        [TestMethod]
        [DataRow(1)]
        [DataRow(5)]
        [DataRow(50)]
        public async Task AcceptAsync_TokenCancelledWhileWaiting_ThrowsTaskCanceledException(int delayMs)
        {
            for (var run = 0; run < 20; ++run)
            {
                var server = NewServer(NewPipeName());
                try
                {
                    using var cancel = new CancellationTokenSource(delayMs);
                    Assert.AreEqual(nameof(TaskCanceledException), await OutcomeAsync(server.AcceptAsync(cancel.Token)), $"run {run}");
                }
                finally
                {
                    server.Close();
                }
            }
        }

        // TSimpleAsyncServer.Stop() and TThreadPoolAsyncServer.Stop() close the server transport this way.
        [TestMethod]
        public async Task AcceptAsync_ClosedWhileWaiting_ThrowsInterrupted()
        {
            var server = NewServer(NewPipeName());
            using var cancel = new CancellationTokenSource(WaitLimit);
            var accept = server.AcceptAsync(cancel.Token);
            server.Close();

            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(async () => await accept);
            Assert.AreEqual(TTransportException.ExceptionType.Interrupted, ex.Type, ex.ToString());
        }

        [TestMethod]
        [DataRow(1)]
        [DataRow(3)]
        public async Task AcceptAsync_ClientsConnectOneAfterAnother_ReturnsConnectedTransports(int listenPipes)
        {
            var pipeName = NewPipeName();
            var server = NewServer(pipeName, listenPipes);
            try
            {
                using var cancel = new CancellationTokenSource(WaitLimit);
                for (var i = 0; i < 5; ++i)
                {
                    using var client = new TNamedPipeTransport(pipeName, new TConfiguration(), ConnectTimeoutMs);
                    var connecting = client.OpenAsync(cancel.Token);  // before the accept, which may block the caller
                    using var accepted = await server.AcceptAsync(cancel.Token);
                    await connecting;

                    Assert.IsTrue(accepted.IsOpen, $"client {i}: AcceptAsync() returned a pipe no client is connected to");
                    await AssertRoundTripAsync(client, accepted, (byte)(10 * i), cancel.Token);
                }
            }
            finally
            {
                server.Close();
            }
        }

        // With more than one listen pipe, the pipes that got no client go on waiting after AcceptAsync() returns.
        // Cancelling the token of that call afterwards must not end those waits, or the next call takes such a
        // pipe for a connection.
        [TestMethod]
        public async Task AcceptAsync_TokenOfEarlierCallCancelled_WaitsForNextClient()
        {
            var pipeName = NewPipeName();
            var server = NewServer(pipeName, 2);
            try
            {
                using var limit = new CancellationTokenSource(WaitLimit);
                using var first = CancellationTokenSource.CreateLinkedTokenSource(limit.Token);
                using var client1 = new TNamedPipeTransport(pipeName, new TConfiguration(), ConnectTimeoutMs);
                var connecting1 = client1.OpenAsync(limit.Token);
                using var accepted1 = await server.AcceptAsync(first.Token);
                await connecting1;
                first.Cancel();  // the call that used it has returned

                using var client2 = new TNamedPipeTransport(pipeName, new TConfiguration(), ConnectTimeoutMs);
                var accepting2 = server.AcceptAsync(limit.Token);
                var connecting2 = client2.OpenAsync(limit.Token);
                using var accepted2 = await accepting2;
                await connecting2;

                Assert.IsTrue(accepted2.IsOpen, "AcceptAsync() returned a pipe no client is connected to");
                await AssertRoundTripAsync(client2, accepted2, 42, limit.Token);
                await AssertRoundTripAsync(client1, accepted1, 7, limit.Token);
            }
            finally
            {
                server.Close();
            }
        }

        // A cancelled call closes the server transport; the next call sets up the pipes again.
        [TestMethod]
        public async Task AcceptAsync_AfterCancelledCall_AcceptsNextClient()
        {
            var pipeName = NewPipeName();
            var server = NewServer(pipeName);
            try
            {
                using (var cancelled = new CancellationTokenSource(5))
                {
                    await OutcomeAsync(server.AcceptAsync(cancelled.Token));
                }

                using var limit = new CancellationTokenSource(WaitLimit);
                using var client = new TNamedPipeTransport(pipeName, new TConfiguration(), ConnectTimeoutMs);
                var connecting = client.OpenAsync(limit.Token);
                using var accepted = await server.AcceptAsync(limit.Token);
                await connecting;

                Assert.IsTrue(accepted.IsOpen, "AcceptAsync() returned a pipe no client is connected to");
                await AssertRoundTripAsync(client, accepted, 99, limit.Token);
            }
            finally
            {
                server.Close();
            }
        }

        // A server started on the caller's thread gives that thread back while it waits for clients, and stops
        // on a cancelled token or on Stop() without logging an error.
        [TestMethod]
        [DataRow(ServerKind.Simple, StopKind.Token)]
        [DataRow(ServerKind.Simple, StopKind.Stop)]
        [DataRow(ServerKind.ThreadPool, StopKind.Token)]
        [DataRow(ServerKind.ThreadPool, StopKind.Stop)]
        public async Task ServeAsync_WaitingForClient_ReturnsToCallerAndStopsQuietly(ServerKind kind, StopKind how)
        {
            var logger = new ErrorLogger();
            var processor = Substitute.For<ITAsyncProcessor>();
            var protocolFactory = new TBinaryProtocol.Factory();
            var transport = new TNamedPipeServerTransport(NewPipeName(), new TConfiguration(), NamedPipeServerFlags.None, 1);
            TServer server = kind switch
            {
                ServerKind.Simple => new TSimpleAsyncServer(new TSingletonProcessorFactory(processor), transport,
                    null, null, protocolFactory, protocolFactory, logger),
                ServerKind.ThreadPool => new TThreadPoolAsyncServer(processor, transport, logger),
                _ => throw new ArgumentOutOfRangeException(nameof(kind)),
            };

            using var cancel = new CancellationTokenSource(WaitLimit);
            // ServeAsync() returns at the first await that has to wait, the accept.
            var serving = server.ServeAsync(cancel.Token);
            var pendingOnReturn = !serving.IsCompleted;

            if (how == StopKind.Token)
                cancel.Cancel();
            else
                server.Stop();
            var ended = await Task.WhenAny(serving, Task.Delay(WaitLimit)) == serving;

            Assert.IsTrue(pendingOnReturn, "ServeAsync() returned only after the server had stopped");
            Assert.IsTrue(ended, "ServeAsync() did not end");
            await serving;
            Assert.IsEmpty(logger.Errors, string.Join(Environment.NewLine, logger.Errors));
        }

        // What an accept ended with, as text for the assertion message.
        private static async Task<string> OutcomeAsync(ValueTask<TTransport> accept)
        {
            try
            {
                using var transport = await accept;
                return $"a transport, IsOpen {transport.IsOpen}";
            }
            catch (TTransportException ex)
            {
                return $"TTransportException({ex.Type})" + (ex.InnerException == null ? "" : " wrapping " + ex.InnerException.GetType().Name);
            }
            catch (Exception ex)
            {
                return ex.GetType().Name;
            }
        }

        // One byte from the client to the server, and another one back.
        private static async Task AssertRoundTripAsync(TTransport client, TTransport server, byte value, CancellationToken cancellationToken)
        {
            var buffer = new byte[1];
            await client.WriteAsync(new[] { value }, cancellationToken);
            await client.FlushAsync(cancellationToken);
            Assert.AreEqual(1, await server.ReadAllAsync(buffer, 0, 1, cancellationToken));
            Assert.AreEqual(value, buffer[0]);

            await server.WriteAsync(new[] { (byte)(value + 1) }, cancellationToken);
            await server.FlushAsync(cancellationToken);
            Assert.AreEqual(1, await client.ReadAllAsync(buffer, 0, 1, cancellationToken));
            Assert.AreEqual((byte)(value + 1), buffer[0]);
        }

        // Collects what the server logs as an error.
        private sealed class ErrorLogger : ILogger
        {
            public ConcurrentQueue<string> Errors { get; } = new();

            public IDisposable? BeginScope<TState>(TState state) where TState : notnull => null;

            public bool IsEnabled(LogLevel logLevel) => logLevel >= LogLevel.Error;

            public void Log<TState>(LogLevel logLevel, EventId eventId, TState state, Exception? exception,
                Func<TState, Exception?, string> formatter)
            {
                if (IsEnabled(logLevel))
                    Errors.Enqueue(formatter(state, exception));
            }
        }
    }
}
