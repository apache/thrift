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
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using test.ExceptionStruct;
using Thrift.Collections;
using Thrift.Protocol;
using Thrift.Transport.Client;

namespace Thrift.Tests.DataModel
{
    // ReSharper disable once InconsistentNaming
    [TestClass]
    public class ExceptionAsStructTests
    {
        [TestMethod]
        public async Task Test_Serialize_Deserialize()
        {
            var initial = CreateInitialData();
            var checking = await WriteAndReadBack(initial);
            VerifyIdenticalContent(checking, initial);
        }

        private static string FormatKey(int i) => $"Test {i}";

        private static BatchGetResponse CreateInitialData()
        {
            var initial = new BatchGetResponse()
            {
                Errors = [],
                Responses = [],
            };

            var i = 0;
            initial.Errors.Add(FormatKey(++i), new() { Error = ErrorCode.GenericError });
            initial.Errors.Add(FormatKey(++i), new() { Error = ErrorCode.InvalidData });
            initial.Errors.Add(FormatKey(++i), new() { Error = ErrorCode.ServerOverload });
            initial.Responses.Add(FormatKey(++i), new() { Id = FormatKey(i), Data = [0x00, 0x11, 0x22] });
            initial.Responses.Add(FormatKey(++i), new() { Id = FormatKey(i), Data = [0x45, 0x56, 0x64] });
            initial.Responses.Add(FormatKey(++i), new() { Id = FormatKey(i), Data = [0x78, 0x9a, 0xbc] });

            return initial;
        }

        private static async Task<T> WriteAndReadBack<T>(T input) where T : TBase,new()
        {
            var stream = new MemoryStream();
            var config = new TConfiguration();

            // write data
            var trans = new TStreamTransport(null, stream, config);
            var proto = new TCompactProtocol(trans);
            await input.WriteAsync(proto, default);

            // read data
            stream.Position = 0;
            trans = new TStreamTransport(stream, null, config);
            proto = new TCompactProtocol(trans);
            var output = new T();
            await output.ReadAsync(proto, default);

            return output;
        }

        private static void VerifyIdenticalContent(BatchGetResponse left, BatchGetResponse right)
        {
            // Errors
            Assert.IsNotNull(left.Errors);
            Assert.IsNotNull(right.Errors);
            Assert.AreEqual(left.Errors.Count, right.Errors.Count);
            foreach(var key in left.Errors.Keys)
            {
                Assert.AreEqual(left.Errors[key].Error, right.Errors[key].Error);
            }

            // Responses
            Assert.IsNotNull(left.Responses);
            Assert.IsNotNull(right.Responses);
            Assert.AreEqual(left.Responses.Count, right.Responses.Count);
            foreach (var key in left.Responses.Keys)
            {
                Assert.AreEqual(left.Responses[key].Id, right.Responses[key].Id);

                var leftData = left.Responses[key].Data;
                var rightData = right.Responses[key].Data;
                Assert.IsNotNull(leftData);
                Assert.IsNotNull(rightData);
                Assert.AreEqual(leftData.Length, rightData.Length);
                for (var index = 0; index < leftData.Length; ++index)
                    Assert.AreEqual(leftData[index], rightData[index]);
            }
        }

    }
}
