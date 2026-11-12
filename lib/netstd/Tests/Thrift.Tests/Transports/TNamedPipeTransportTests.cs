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
using System.IO.Pipes;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Transport.Client;

namespace Thrift.Tests.Transports
{
    // TNamedPipeTransport connects to the pipe on the server it is given, and to the one on the local machine when
    // it is given no server name.
    [TestClass]
    public class TNamedPipeTransportTests
    {
        private static readonly TimeSpan WaitLimit = TimeSpan.FromSeconds(10);
        private const int ConnectTimeoutMs = 10_000;

        // The client opens its pipe for synchronous I/O, so a write that waits for the server does not see a token.
        // Longer than WaitLimit, so that a wait which has a token fails with its own exception first.
        private static readonly TimeSpan ExchangeLimit = TimeSpan.FromSeconds(30);

        // The same buffer sizes as TNamedPipeServerTransport.
        private const int PipeBufferSize = 4096;

        // On Linux, the pipe is a Unix domain socket in the temp directory, whose path length is limited.
        private static string NewPipeName() => "thrift-" + Guid.NewGuid().ToString("N");

        [TestMethod]
        [DataRow(null)]
        [DataRow("")]
        [DataRow(" ")]
        [DataRow(".")]
        public async Task Constructor_NoServerName_ConnectsToLocalPipe(string? server)
        {
            await AssertConnectsToLocalPipeAsync(pipeName => new TNamedPipeTransport(server!, pipeName, new TConfiguration(), ConnectTimeoutMs));
        }

        [TestMethod]
        public async Task Constructor_PipeOnly_ConnectsToLocalPipe()
        {
            await AssertConnectsToLocalPipeAsync(pipeName => new TNamedPipeTransport(pipeName, new TConfiguration(), ConnectTimeoutMs));
        }

        // .NET supports the pipes of other machines only on Windows. Elsewhere, a server name that reaches the
        // pipe stream fails at once, instead of being replaced by the local machine.
        [TestMethod]
        [OSCondition(ConditionMode.Exclude, OperatingSystems.Windows)]
        public void Constructor_OtherServerWithoutRemotePipes_Throws()
        {
            Assert.ThrowsExactly<PlatformNotSupportedException>(
                () => new TNamedPipeTransport("other-host.invalid", NewPipeName(), new TConfiguration(), ConnectTimeoutMs));
        }

        private static async Task AssertConnectsToLocalPipeAsync(Func<string, TNamedPipeTransport> newClient)
        {
            var pipeName = NewPipeName();
            using var server = new NamedPipeServerStream(pipeName, PipeDirection.InOut, 1, PipeTransmissionMode.Byte,
                PipeOptions.Asynchronous, PipeBufferSize, PipeBufferSize);
            using var client = newClient(pipeName);
            using var cancel = new CancellationTokenSource(WaitLimit);

            var exchange = Task.Run(() => ExchangeByteAsync(server, client, cancel.Token));
            Assert.AreSame(exchange, await Task.WhenAny(exchange, Task.Delay(ExchangeLimit)), "The client and the server did not exchange a byte");
            await exchange;
        }

        private static async Task ExchangeByteAsync(NamedPipeServerStream server, TNamedPipeTransport client, CancellationToken cancellationToken)
        {
            var accepting = server.WaitForConnectionAsync(cancellationToken);
            await client.OpenAsync(cancellationToken);
            await accepting;
            Assert.IsTrue(client.IsOpen);

            // On Windows, a write that does not fit into the pipe's buffer waits until the other end reads, so the
            // server reads before the client writes.
            var buffer = new byte[1];
            var reading = server.ReadAsync(buffer.AsMemory(0, 1), cancellationToken).AsTask();
            await client.WriteAsync(new byte[] { 42 }, cancellationToken);
            await client.FlushAsync(cancellationToken);
            Assert.AreEqual(1, await reading);
            Assert.AreEqual(42, buffer[0]);
        }
    }
}
