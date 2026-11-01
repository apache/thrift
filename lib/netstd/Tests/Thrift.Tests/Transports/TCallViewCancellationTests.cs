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
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Transports
{
    /// <summary>A cancelled or timed-out call must not keep reading or writing through its transport view.</summary>
    [TestClass]
    public class TCallViewCancellationTests
    {
        private static CancellationToken Cancelled()
        {
            var cts = new CancellationTokenSource();
            cts.Cancel();
            return cts.Token;
        }

        private static TTransport HttpView()
        {
            var http = new THttpTransport(new HttpClient(), null, new Uri("http://localhost/x"));
            return http.CreateCallTransport();
        }

        private static TTransport MemoryView() => new TMemoryBufferTransport(null).CreateCallTransport();

        [TestMethod]
        public async Task Http_Call_View_Honors_The_Token_Of_The_Convenience_Write_Overload()
        {
            using var view = HttpView();
            await Assert.ThrowsAsync<OperationCanceledException>(() => view.WriteAsync(new byte[] { 1, 2 }, Cancelled()));
        }

        [TestMethod]
        public async Task Http_Call_View_Honors_The_Token_Of_Write_Through_The_Json_Protocol()
        {
            // the Json protocol writes its quotes through the overload without offset and count
            using var view = HttpView();
            var protocol = new TJsonProtocol(view);
            await Assert.ThrowsAsync<OperationCanceledException>(() => protocol.WriteStringAsync("a\\b", Cancelled()));
        }

        [TestMethod]
        public async Task Memory_Call_View_Honors_The_Token_On_Write()
        {
            using var view = MemoryView();
            await Assert.ThrowsAsync<OperationCanceledException>(() => view.WriteAsync(new byte[] { 1, 2 }, 0, 2, Cancelled()));
            await Assert.ThrowsAsync<OperationCanceledException>(() => view.WriteAsync(new byte[] { 1, 2 }, Cancelled()));
        }

        [TestMethod]
        public async Task Memory_Call_View_Honors_The_Token_On_Read()
        {
            using var view = MemoryView();
            await view.WriteAsync(new byte[] { 1, 2, 3 }, CancellationToken.None);
            await Assert.ThrowsAsync<OperationCanceledException>(() => view.ReadAsync(new byte[3], 0, 3, Cancelled()).AsTask());
        }

        [TestMethod]
        public async Task Memory_Call_View_Still_Works_With_A_Live_Token()
        {
            using var view = MemoryView();
            await view.WriteAsync(new byte[] { 1, 2, 3 }, CancellationToken.None);
            ((TMemoryBufferTransport)view).Seek(0, System.IO.SeekOrigin.Begin);
            var buffer = new byte[3];
            Assert.AreEqual(3, await view.ReadAsync(buffer, 0, 3, CancellationToken.None));
            CollectionAssert.AreEqual(new byte[] { 1, 2, 3 }, buffer);
        }
    }
}
