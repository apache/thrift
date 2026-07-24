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
using System.IO;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Protocols
{
    // A JSON string and a JSON numeric literal are read without a declared length,
    // so unlike a length-prefixed binary string there is nothing to reject up front:
    // TJsonProtocol bounds their running byte count against the configured
    // MaxMessageSize instead. These reads run over a TStreamTransport, which does no
    // per-read message-size accounting (unlike TMemoryBufferTransport, whose own
    // decrement would already bound the read and so mask the protocol's own bound),
    // so the tests isolate TJsonProtocol's bound on the real streaming read path.
    [TestClass]
    public class TJsonProtocolSizeLimitTests
    {
        private static TJsonProtocol JsonReading(string json, int maxSize)
        {
            var config = new TConfiguration { MaxMessageSize = maxSize };
            var stream = new MemoryStream(Encoding.UTF8.GetBytes(json));
            return new TJsonProtocol(new TStreamTransport(stream, null, config));
        }

        [TestMethod]
        public async Task ReadString_WithinMaxMessageSize_Accepted()
        {
            var proto = JsonReading("\"" + new string('A', 512) + "\"", 4096);
            var result = await proto.ReadStringAsync(default);
            Assert.AreEqual(512, result.Length);
        }

        [TestMethod]
        public async Task ReadString_ExceedsMaxMessageSize_Rejected()
        {
            // A quote-delimited JSON string carries no declared length to reject up
            // front, so this exercises the running byte-count bound in ReadJsonStringAsync.
            var proto = JsonReading("\"" + new string('A', 4096) + "\"", 1024);
            await AssertMessageSizeLimitAsync(async () => await proto.ReadStringAsync(default));
        }

        [TestMethod]
        public async Task ReadNumber_WithinMaxMessageSize_Accepted()
        {
            // A legitimate numeric literal within the limit must still read cleanly
            // (guards against the size bound false-rejecting normal numbers).
            var proto = JsonReading("3.14159 ", 4096);
            var result = await proto.ReadDoubleAsync(default);
            Assert.AreEqual(3.14159, result, 0.00001);
        }

        [TestMethod]
        public async Task ReadNumber_ExceedsMaxMessageSize_Rejected()
        {
            // Terminate the numeric run with a non-numeric byte so the unpatched path
            // stops cleanly at the terminator rather than on EOF, isolating the size bound.
            var proto = JsonReading(new string('1', 4096) + " ", 1024);
            await AssertMessageSizeLimitAsync(async () => await proto.ReadDoubleAsync(default));
        }

        private static async Task AssertMessageSizeLimitAsync(Func<Task> act)
        {
            try
            {
                await act();
                Assert.Fail("Expected the oversized JSON value to be rejected, but it was accepted");
            }
            catch (TTransportException ex)
            {
                Assert.AreEqual(TTransportException.ExceptionType.MessageSizeLimit, ex.Type);
            }
        }
    }
}
