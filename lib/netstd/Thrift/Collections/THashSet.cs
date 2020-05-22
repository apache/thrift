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

using System.Collections;
using System.Collections.Generic;

namespace Thrift.Collections
{
    // ReSharper disable once InconsistentNaming
    public class THashSet<T> : ICollection<T>
    {
        private readonly HashSet<T> Items;

        public THashSet()
        {
            Items = new HashSet<T>();
        }

        public THashSet(int capacity)
        {
            // TODO: uncomment capacity when NET Standard also implements it
            Items = new HashSet<T>(/*capacity*/);
        }

        public int Count => Items.Count;

        public bool IsReadOnly => false;

        public void Add(T item)
        {
            Items.Add(item);
        }

        public void Clear()
        {
            Items.Clear();
        }

        public bool Contains(T item)
        {
            return Items.Contains(item);
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            Items.CopyTo(array, arrayIndex);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return Items.GetEnumerator();
        }

        public IEnumerator<T> GetEnumerator()
        {
            return ((IEnumerable<T>) Items).GetEnumerator();
        }

        public bool Remove(T item)
        {
            return Items.Remove(item);
        }
    }
}
