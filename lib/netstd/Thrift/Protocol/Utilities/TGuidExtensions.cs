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

            // already network order on BigEndian machines
            if (BitConverter.IsLittleEndian)
            {
                SwapBytes(ref bytes[0], ref bytes[3]);
                SwapBytes(ref bytes[1], ref bytes[2]);
                SwapBytes(ref bytes[4], ref bytes[5]);
                SwapBytes(ref bytes[6], ref bytes[7]);
            }

            return new Guid(bytes);
        }

        private static void SwapBytes(ref byte one, ref byte two)
        {
            (two, one) = (one, two);
        }

        #region SelfTest
#if DEBUG
        static private readonly Guid TEST_GUID = new Guid("{00112233-4455-6677-8899-aabbccddeeff}");

        static TGuidExtensions()
        {
            SelfTest();
        }

        private static void SelfTest()
        {
            // host to network
            var guid = TEST_GUID;
            guid = guid.SwapByteOrder();

            // validate network order
            var bytes = guid.ToByteArray();
            for (var i = 0; i < 10; ++i)
            {
                var expected = i * 0x11;
                Debug.Assert( bytes[i] == expected);
            }

            // network to host and final validation
            guid = guid.SwapByteOrder();
            Debug.Assert(guid.Equals(TEST_GUID));
        }

#endif
        #endregion

    }
}
