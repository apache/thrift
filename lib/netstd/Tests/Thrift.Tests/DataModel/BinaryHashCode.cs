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

using System.Collections.Generic;
using Apache.Cassandra.Test;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using test.ExceptionStruct;

namespace Thrift.Tests.DataModel
{
    // THRIFT-6494: Equals compares a binary field by its content, so GetHashCode() must hash it by
    // its content too, not by the reference of the array
    [TestClass]
    public class BinaryHashCodeTests
    {
        [TestMethod]
        public void EqualBinaryContent_HashesEqually()
        {
            var first = new GetResponse { Data = new byte[] { 1, 2, 3 } };
            var second = new GetResponse { Data = new byte[] { 1, 2, 3 } };
            Assert.IsTrue(first.Equals(second));
            Assert.AreEqual(first.GetHashCode(), second.GetHashCode());
        }

        [TestMethod]
        public void EqualBinaryContentNextToOtherFields_HashesEqually()
        {
            var request = new GetRequest { Id = "id", Data = new byte[] { 1, 2, 3 } };
            var requestCopy = new GetRequest { Id = "id", Data = new byte[] { 1, 2, 3 } };
            Assert.IsTrue(request.Equals(requestCopy));
            Assert.AreEqual(request.GetHashCode(), requestCopy.GetHashCode());
            Assert.AreEqual(request.GetHashCode(), request.DeepCopy().GetHashCode());

            var column = new Column { Name = new byte[] { 1 }, Value = new byte[] { 2, 3 }, Timestamp = 4 };
            var columnCopy = new Column { Name = new byte[] { 1 }, Value = new byte[] { 2, 3 }, Timestamp = 4 };
            Assert.IsTrue(column.Equals(columnCopy));
            Assert.AreEqual(column.GetHashCode(), columnCopy.GetHashCode());
        }

        [TestMethod]
        public void EqualBinaryContent_IsOneElementOfAHashSet()
        {
            var set = new HashSet<GetResponse>
            {
                new GetResponse { Data = new byte[] { 1, 2, 3 } },
                new GetResponse { Data = new byte[] { 1, 2, 3 } },
            };
            Assert.HasCount(1, set);
        }

        [TestMethod]
        public void DifferentBinaryContent_HashesDifferently()
        {
            var first = new GetResponse { Data = new byte[] { 1, 2, 3 } };
            var second = new GetResponse { Data = new byte[] { 1, 2, 4 } };
            Assert.IsFalse(first.Equals(second));
            Assert.AreNotEqual(first.GetHashCode(), second.GetHashCode());
        }
    }
}
