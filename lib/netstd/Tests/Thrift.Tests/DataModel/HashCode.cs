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
using Microsoft.VisualStudio.TestTools.UnitTesting;
using ThriftTest;

namespace Thrift.Tests.DataModel
{
    // THRIFT-5056: the generated GetHashCode() of a struct must tell apart which fields
    // hold a value, not only the values themselves
    [TestClass]
    public class HashCodeTests
    {
        // An Xtruct3 that holds value in those of its fields changed, i32_thing and i64_thing that are requested
        private static Xtruct3 MakeXtruct3(bool changed, bool i32, bool i64, int value)
        {
            var instance = new Xtruct3();
            if (changed)
                instance.Changed = value;
            if (i32)
                instance.I32_thing = value;
            if (i64)
                instance.I64_thing = value;
            return instance;
        }

        [TestMethod]
        public void SameValueInDifferentFields_HashesDifferently()
        {
            var a = new Bools { Im_true = true };
            var b = new Bools { Im_false = true };
            Assert.AreNotEqual(a, b);
            Assert.AreNotEqual(a.GetHashCode(), b.GetHashCode());

            var i32 = new Xtruct { I32_thing = 5 };
            var i64 = new Xtruct { I64_thing = 5 };
            Assert.AreNotEqual(i32, i64);
            Assert.AreNotEqual(i32.GetHashCode(), i64.GetHashCode());
        }

        [TestMethod]
        public void SameValuesShiftedToOtherFields_HashDifferently()
        {
            var first = new Xtruct3 { Changed = 1, I32_thing = 2 };
            var second = new Xtruct3 { I32_thing = 1, I64_thing = 2 };
            Assert.AreNotEqual(first, second);
            Assert.AreNotEqual(first.GetHashCode(), second.GetHashCode());
        }

        [TestMethod]
        public void EveryCombinationOfSetFields_HashesDifferently()
        {
            // all eight combinations of three integer fields that hold the same value
            var hashCodes = new HashSet<int>();
            foreach (var changed in new[] { false, true })
                foreach (var i32 in new[] { false, true })
                    foreach (var i64 in new[] { false, true })
                        hashCodes.Add(MakeXtruct3(changed, i32, i64, 7).GetHashCode());
            Assert.HasCount(8, hashCodes);
        }

        [TestMethod]
        public void EqualInstances_HashEqually()
        {
            var first = new Xtruct3 { String_thing = "text", Changed = 1, I64_thing = 2 };
            var second = new Xtruct3 { String_thing = "text", Changed = 1, I64_thing = 2 };
            Assert.AreEqual(first, second);
            Assert.AreEqual(first.GetHashCode(), second.GetHashCode());
            Assert.AreEqual(first.GetHashCode(), first.DeepCopy().GetHashCode());

            var empty = new Bools();
            Assert.AreEqual(empty.GetHashCode(), new Bools().GetHashCode());
            Assert.AreEqual(new Bools { Im_true = true }.GetHashCode(), new Bools { Im_true = true }.GetHashCode());
        }
    }
}
