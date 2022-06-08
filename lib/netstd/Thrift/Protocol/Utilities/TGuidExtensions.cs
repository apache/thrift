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
using System.Text;

namespace Thrift.Protocol.Utilities
{
    public static class TGuidExtensions
    {
        public static Guid SwapByteOrder(this Guid self)
        {
            var bytes = self.ToByteArray();

            SwapBytes(ref bytes[0], ref bytes[3]);
            SwapBytes(ref bytes[1], ref bytes[2]);
            SwapBytes(ref bytes[4], ref bytes[5]);
            SwapBytes(ref bytes[6], ref bytes[7]);

            return new Guid(bytes);
        }

        private static void SwapBytes(ref byte one, ref byte two)
        {
            var tmp = one;
            one = two;
            two = tmp;
        }

        #region SelfTest
#if DEBUG
        static private readonly Guid INTERNAL_HOST_ORDER = new Guid("{67452301-ab89-efcd-0123-456789abcdef}");
        static private readonly Guid NETWORK_BYTE_ORDER = new Guid("{01234567-89ab-cdef-0123-456789abcdef}");

        static TGuidExtensions()
        {
            SelfTest();
        }

        private static void SelfTest()
        {
            var guid = INTERNAL_HOST_ORDER;
            guid = guid.SwapByteOrder();
            Debug.Assert(guid.Equals(NETWORK_BYTE_ORDER));
            guid = guid.SwapByteOrder();
            Debug.Assert(guid.Equals(INTERNAL_HOST_ORDER));
        }

#endif
        #endregion

    }
}
