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
using Thrift6493;

namespace Thrift.Tests.DataModel
{
    // THRIFT-6493: two values of a union are equal only if they hold the same member, with equal
    // values, and the hash code of a value depends on which member it holds
    [TestClass]
    public class UnionEqualityTests
    {
        [TestMethod]
        public void DifferentMembersWithDefaultValues_AreNotEqual()
        {
            Assert.IsFalse(new U.x(0).Equals(new U.y(0)));
            Assert.IsFalse(new U.y(0).Equals(new U.x(0)));
        }

        [TestMethod]
        public void DifferentMembers_AreNotEqualInEitherDirection()
        {
            Assert.IsFalse(new U.x(0).Equals(new U.s("text")));
            Assert.IsFalse(new U.s("text").Equals(new U.x(0)));
        }

        [TestMethod]
        public void DifferentMembersWithEqualValues_HashDifferently()
        {
            Assert.AreNotEqual(new U.x(5).GetHashCode(), new U.y(5).GetHashCode());
        }

        [TestMethod]
        public void EqualValues_AreEqualAndHashEqually()
        {
            var pairs = new (U, U)[]
            {
                (new U.x(0), new U.x(0)),
                (new U.x(5), new U.x(5)),
                (new U.y(5), new U.y(5)),
                (new U.s("text"), new U.s("text")),
                (new U.x(5), new U.x(5).DeepCopy()),
                (new U.___undefined(), new U.___undefined()),
            };
            foreach (var (first, second) in pairs)
            {
                Assert.IsTrue(first.Equals(second), $"{first} equals {second}");
                Assert.IsTrue(second.Equals(first), $"{second} equals {first}");
                Assert.AreEqual(first.GetHashCode(), second.GetHashCode(), $"hash codes of {first} and {second}");
            }
        }

        [TestMethod]
        public void DifferentMembers_AreKeptApartInAHashSet()
        {
            var set = new HashSet<U> { new U.x(0), new U.y(0), new U.x(5), new U.y(5), new U.s("text"), new U.x(5) };
            Assert.HasCount(5, set);
        }
    }
}
