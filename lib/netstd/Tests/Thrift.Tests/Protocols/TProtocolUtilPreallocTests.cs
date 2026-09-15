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
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol.Utilities;

namespace Thrift.Tests.Protocols
{
    // TProtocolUtil.PreallocSize caps the initial capacity that generated decoders reserve
    // from a wire-supplied container element count, so a peer cannot make a short message
    // reserve capacity it never backs with data.
    [TestClass]
    public class TProtocolUtilPreallocTests
    {
        [TestMethod]
        public void PreallocSize_CountUnderCap_ReservesExactly()
        {
            Assert.AreEqual(0, TProtocolUtil.PreallocSize(0));
            Assert.AreEqual(1, TProtocolUtil.PreallocSize(1));
            Assert.AreEqual(500, TProtocolUtil.PreallocSize(500));
            Assert.AreEqual(TProtocolUtil.MaxPreallocSize, TProtocolUtil.PreallocSize(TProtocolUtil.MaxPreallocSize));
        }

        [TestMethod]
        public void PreallocSize_CountAboveCap_ReservesOnlyTheCap()
        {
            Assert.AreEqual(TProtocolUtil.MaxPreallocSize, TProtocolUtil.PreallocSize(TProtocolUtil.MaxPreallocSize + 1));
            Assert.AreEqual(TProtocolUtil.MaxPreallocSize, TProtocolUtil.PreallocSize(1_000_000));
            Assert.AreEqual(TProtocolUtil.MaxPreallocSize, TProtocolUtil.PreallocSize(int.MaxValue));
        }

        [TestMethod]
        public void PreallocSize_NegativeCount_ReservesNothing()
        {
            Assert.AreEqual(0, TProtocolUtil.PreallocSize(-1));
            Assert.AreEqual(0, TProtocolUtil.PreallocSize(int.MinValue));
        }
    }
}
