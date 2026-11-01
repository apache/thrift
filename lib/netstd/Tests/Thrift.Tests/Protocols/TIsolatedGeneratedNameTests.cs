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
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Transport;
using NameCollision = IsolatedExceptionName;

namespace Thrift.Tests.Protocols
{
    /// <summary>
    /// The namespace of the service under test defines an exception named <c>Exception</c>. Generated code that
    /// meant <c>System.Exception</c> by an unqualified <c>Exception</c> still compiles, and then catches the wrong
    /// type: a timeout would not be reported as one. Only a run time test can see that.
    /// </summary>
    [TestClass]
    public class TIsolatedGeneratedNameTests
    {
        /// <summary>A call-isolated transport whose server never answers, with a declared per-call timeout.</summary>
        private sealed class SilentServerTransport : ICallIsolatedTransport, IPerCallTimeoutTransport
        {
            public TimeSpan PerCallTimeout { get; set; } = TimeSpan.FromMilliseconds(100);

            public TTransport CreateCallTransport() => new SilentView();

            public void Dispose() { }

            private sealed class SilentView : TEndpointTransport
            {
                public SilentView() : base(null) { }

                public override bool IsOpen => true;
                public override Task OpenAsync(CancellationToken cancellationToken) => Task.CompletedTask;
                public override void Close() { }
                public override Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken) => Task.CompletedTask;
                public override Task FlushAsync(CancellationToken cancellationToken) => Task.CompletedTask;
                protected override void Dispose(bool disposing) { }

                // the reply never arrives: only cancellation ends the wait
                public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
                {
                    await Task.Delay(Timeout.Infinite, cancellationToken);
                    return 0;
                }
            }
        }

        [TestMethod]
        public async Task Timeout_Is_Reported_As_TimedOut_When_The_Idl_Defines_An_Exception_Type()
        {
            var client = new NameCollision.Timeouts.Client(new SilentServerTransport(), new TBinaryProtocol.Factory());

            var ex = await Assert.ThrowsExactlyAsync<TTransportException>(() => client.add(1, 2));
            Assert.AreEqual(TTransportException.ExceptionType.TimedOut, ex.Type);
        }

        [TestMethod]
        public async Task Caller_Cancellation_Is_Still_Not_A_Timeout_When_The_Idl_Defines_An_Exception_Type()
        {
            var client = new NameCollision.Timeouts.Client(new SilentServerTransport { PerCallTimeout = TimeSpan.FromMinutes(10) },
                new TBinaryProtocol.Factory());
            using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(100));

            await Assert.ThrowsAsync<OperationCanceledException>(() => client.add(1, 2, cts.Token));
        }
    }
}
