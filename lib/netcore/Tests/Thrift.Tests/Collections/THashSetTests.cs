using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Collections;

namespace Thrift.Tests.Collections
{
    // ReSharper disable once InconsistentNaming
    [TestClass]
    public class THashSetTests
    {
        [TestMethod]
        public void THashSet_Equals_Primitive_Test()
        {
            const int value = 1;

            var hashSet = new THashSet<int> {value};
            
            Assert.IsTrue(hashSet.Contains(value));

            hashSet.Remove(value);

            Assert.IsTrue(hashSet.Count == 0);

            hashSet.Add(value);

            Assert.IsTrue(hashSet.Contains(value));

            hashSet.Clear();

            Assert.IsTrue(hashSet.Count == 0);

            var newArr = new int[1];
            hashSet.Add(value);
            hashSet.CopyTo(newArr, 0);

            Assert.IsTrue(newArr.Contains(value));

            var en = hashSet.GetEnumerator();
            en.MoveNext();

            Assert.IsTrue((int)en.Current == value);
            
            using (var ien = ((IEnumerable<int>)hashSet).GetEnumerator())
            {
                ien.MoveNext();

                Assert.IsTrue(ien.Current == value);
            }
        }
    }
}
