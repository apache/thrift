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
using System.Threading;
using System.Threading.Tasks;
using KellermanSoftware.CompareNetObjects;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Protocol.Entities;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.IntegrationTests.Protocols
{
    [TestClass]
    public class ProtocolConformityTests : TestBase
    {
        [DataTestMethod]
        [DataRow(typeof(TBinaryProtocol))]
        [DataRow(typeof(TCompactProtocol))]
        [DataRow(typeof(TJsonProtocol))]
        public async Task ReadUuidFromStream(Type protocolType)
        {
            var expected = new Guid("{00112233-4455-6677-8899-aabbccddeeff}");

            try
            {
                var tuple = GetProtocolInstance(protocolType);
                using var stream = tuple.Stream;

                await GenerateGuidTestData(stream, protocolType, default);
                stream.Seek(0, SeekOrigin.Begin);
                tuple.Transport.ResetMessageSizeAndConsumedBytes();  // length has changed

                var actual = await tuple.Protocol.ReadUuidAsync(default);

                var result = _compareLogic.Compare(expected, actual);
                Assert.IsTrue(result.AreEqual, result.DifferencesString);
            }
            catch (Exception e)
            {
                throw new Exception($"Exception during testing of protocol: {protocolType.FullName}", e);
            }
        }

        private static async Task GenerateGuidTestData(Stream stream, Type protocolType, CancellationToken cancel)
        {
            stream.Seek(0, SeekOrigin.Begin);

            if(protocolType == typeof(TJsonProtocol))
            {
                await stream.WriteAsync(Encoding.UTF8.GetBytes("\"00112233-4455-6677-8899-aabbccddeeff\""), cancel);
                return;
            }

            if (protocolType == typeof(TBinaryProtocol))
            {
                var data = new byte[16] { 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff };
                await stream.WriteAsync(data, cancel);
                return;
            }

            if (protocolType == typeof(TCompactProtocol))
            {
                var data = new byte[16] { 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff };
                await stream.WriteAsync(data, cancel);
                return;
            }

            throw new Exception($"Unhandled protocol: {protocolType.FullName}");
        }
    }
}
