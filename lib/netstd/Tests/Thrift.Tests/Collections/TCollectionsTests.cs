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
using System.Security.Cryptography.Xml;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Collections;

namespace Thrift.Tests.Collections
{
    // ReSharper disable once InconsistentNaming
    [TestClass]
    public class TCollectionsTests
    {
        //TODO: Add tests for IEnumerable with objects and primitive values inside

        [TestMethod]
        public void TCollection_List_Equals_Primitive_Test()
        {
            var collection1 = new List<int> {1,2,3};
            var collection2 = new List<int> {1,2,3};
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_List_Equals_Primitive_Different_Test()
        {
            var collection1 = new List<int> { 1, 2, 3 };
            var collection2 = new List<int> { 1, 2 };
            Assert.IsFalse(TCollections.Equals(collection1, collection2));
            Assert.IsFalse(collection1.SequenceEqual(collection2));

            collection2.Add(4);
            Assert.IsFalse(TCollections.Equals(collection1, collection2));
            Assert.IsFalse(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_List_Equals_Objects_Test()
        {
            var collection1 = new List<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } };
            var collection2 = new List<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } };
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_List_List_Equals_Objects_Test()
        {
            var collection1 = new List<List<ExampleClass>> { new List<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } } };
            var collection2 = new List<List<ExampleClass>> { new List<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } } };
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsFalse(collection1.SequenceEqual(collection2));  // SequenceEqual() calls Equals() of the inner list instead of SequenceEqual()
        }

        [TestMethod]
        public void TCollection_List_Equals_OneAndTheSameObject_Test()
        {
            var collection1 = new List<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } };
            var collection2 = collection1;
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_Set_Equals_Primitive_Test()
        {
            var collection1 = new THashSet<int> {1,2,3};
            var collection2 = new THashSet<int> {1,2,3};
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_Set_Equals_Primitive_Different_Test()
        {
            var collection1 = new THashSet<int> { 1, 2, 3 };
            var collection2 = new THashSet<int> { 1, 2 };
            Assert.IsFalse(TCollections.Equals(collection1, collection2));
            Assert.IsFalse(collection1.SequenceEqual(collection2));

            collection2.Add(4);
            Assert.IsFalse(TCollections.Equals(collection1, collection2));
            Assert.IsFalse(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_Set_Equals_Objects_Test()
        {
            var collection1 = new THashSet<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } };
            var collection2 = new THashSet<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } };
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_Set_Set_Equals_Objects_Test()
        {
            var collection1 = new THashSet<THashSet<ExampleClass>> { new THashSet<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } } };
            var collection2 = new THashSet<THashSet<ExampleClass>> { new THashSet<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } } };
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsFalse(collection1.SequenceEqual(collection2));  // SequenceEqual() calls Equals() of the inner list instead of SequenceEqual()
        }

        [TestMethod]
        public void TCollection_Set_Equals_OneAndTheSameObject_Test()
        {
            var collection1 = new THashSet<ExampleClass> { new ExampleClass { X = 1 }, new ExampleClass { X = 2 } };
            var collection2 = collection1;      // references to one and the same collection
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }


        [TestMethod]
        public void TCollection_Map_Equals_Primitive_Test()
        {
            var collection1 = new Dictionary<int, int> { [1] = 1, [2] = 2, [3] = 3 };
            var collection2 = new Dictionary<int, int> { [1] = 1, [2] = 2, [3] = 3 };
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_Map_Equals_Primitive_Different_Test()
        {
            var collection1 = new Dictionary<int, int> { [1] = 1, [2] = 2, [3] = 3 };
            var collection2 = new Dictionary<int, int> { [1] = 1, [2] = 2 };
            Assert.IsFalse(TCollections.Equals(collection1, collection2));
            Assert.IsFalse(collection1.SequenceEqual(collection2));

            collection2[3] = 3;
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));

            collection2[3] = 4;
            Assert.IsFalse(TCollections.Equals(collection1, collection2));
        }

        [TestMethod]
        public void TCollection_Map_Equals_Objects_Test()
        {
            var collection1 = new Dictionary<int, ExampleClass>
            {
                [1] = new ExampleClass { X = 1 },
                [-1] = new ExampleClass { X = 2 }
            };
            var collection2 = new Dictionary<int, ExampleClass>
            {
                [1] = new ExampleClass { X = 1 },
                [-1] = new ExampleClass { X = 2 }
            };

            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }

        [TestMethod]
        public void TCollection_Map_Map_Equals_Objects_Test()
        {
            var collection1 = new Dictionary<int, Dictionary<int, ExampleClass>>
            {
                [0] = new Dictionary<int, ExampleClass>
                {
                    [1] = new ExampleClass { X = 1 },
                    [-1] = new ExampleClass { X = 2 }
                }
            };
            var collection2 = new Dictionary<int, Dictionary<int, ExampleClass>>
            {
                [0] = new Dictionary<int, ExampleClass>
                {
                    [1] = new ExampleClass { X = 1 },
                    [-1] = new ExampleClass { X = 2 }
                }
            };

            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsFalse(collection1.SequenceEqual(collection2));  // SequenceEqual() calls Equals() of the inner list instead of SequenceEqual()
        }

        [TestMethod]
        public void TCollection_Map_Equals_OneAndTheSameObject_Test()
        {
            var collection1 = new Dictionary<int, ExampleClass>
            {
                [1] = new ExampleClass { X = 1 },
                [-1] = new ExampleClass { X = 2 }
            };
            var collection2 = collection1;
            Assert.IsTrue(TCollections.Equals(collection1, collection2));
            Assert.IsTrue(collection1.SequenceEqual(collection2));
        }


        private class ExampleClass
        {
            public int X { get; set; }

            // all Thrift-generated classes override Equals(), we do just the same
            public override bool Equals(object that)
            {
                if (!(that is ExampleClass other)) return false;
                if (ReferenceEquals(this, other)) return true;

                return this.X == other.X;
            }

            //  overriding Equals() requires GetHashCode() as well
            public override int GetHashCode()
            {
                int hashcode = 157;
                unchecked
                {
                    hashcode = (hashcode * 397) + X.GetHashCode();
                }
                return hashcode;
            }
        }
    }
}

