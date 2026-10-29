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
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
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
    }
}
