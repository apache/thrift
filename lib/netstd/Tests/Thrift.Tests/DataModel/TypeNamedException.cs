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
using Thrift.Transport.Client;
using ThrowingService = Thrift6474.exceptions.ThrowingService;

namespace Thrift.Tests.DataModel
{
    // THRIFT-6474: the namespace of the service has a type named Exception
    [TestClass]
    public class TypeNamedExceptionTests
    {
        private sealed class ThrowingHandler : ThrowingService.IAsync
        {
            private readonly Exception _thrown;

            public ThrowingHandler(Exception thrown)
            {
                _thrown = thrown;
            }

            public Task<int> Call(int value, CancellationToken cancellationToken = default) => throw _thrown;
        }

        private static async Task<ThrowingService.Client> CallProcessorAsync(Exception thrown)
        {
            var config = new TConfiguration();
            var request = new TMemoryBufferTransport(config);
            await new ThrowingService.Client(new TBinaryProtocol(request)).send_Call(1, CancellationToken.None);

            var reply = new TMemoryBufferTransport(config);
            var processor = new ThrowingService.AsyncProcessor(new ThrowingHandler(thrown));
            var more = await processor.ProcessAsync(
                new TBinaryProtocol(new TMemoryBufferTransport(request.GetBuffer(), config)),
                new TBinaryProtocol(reply),
                CancellationToken.None);

            Assert.IsTrue(more);
            return new ThrowingService.Client(new TBinaryProtocol(new TMemoryBufferTransport(reply.GetBuffer(), config)));
        }

        [TestMethod]
        public async Task Undeclared_Exception_Becomes_An_InternalError_Reply()
        {
            var client = await CallProcessorAsync(new InvalidOperationException("from the handler"));

            var ex = await Assert.ThrowsExactlyAsync<TApplicationException>(() => client.recv_Call(CancellationToken.None));
            Assert.AreEqual(TApplicationException.ExceptionType.InternalError, ex.Type);
        }

        [TestMethod]
        public async Task Declared_Exception_Reaches_The_Client()
        {
            var client = await CallProcessorAsync(new Thrift6474.exceptions.Exception { Msg = "declared" });

            var ex = await Assert.ThrowsExactlyAsync<Thrift6474.exceptions.Exception>(() => client.recv_Call(CancellationToken.None));
            Assert.AreEqual("declared", ex.Msg);
        }
    }
}
