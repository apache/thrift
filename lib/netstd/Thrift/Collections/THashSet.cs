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
using System.Collections;
using System.Collections.Generic;

namespace Thrift.Collections
{
    // ReSharper disable once InconsistentNaming
	[Obsolete("deprecated, use HashSet<T> instead")]
    public class THashSet<T> : System.Collections.Generic.HashSet<T>
    {
        public THashSet()
            : base()
        {
        }

        public THashSet(int capacity)
#if NET5_0_OR_GREATER
            : base(capacity)
#elif NETFRAMEWORK || NETSTANDARD
            : base(/*capacity not supported*/)
#else
#error Unknown platform
#endif
        {
        }

        public THashSet(IEnumerable<T> collection)
            : base(collection)
        {
        }

    }
}

