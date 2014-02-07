/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Thrift.Protocol;
using Thrift.Transport;

namespace JSONTest
{
    class Program
    {
        static void Main(string[] args)
        {
            TestThrift2336();
        }

        public static void TestThrift2336()
        {
            const string RUSSIAN_TEXT = "\u0420\u0443\u0441\u0441\u043a\u043e\u0435 \u041d\u0430\u0437\u0432\u0430\u043d\u0438\u0435";
            const string RUSSIAN_JSON = "\"\\u0420\\u0443\\u0441\\u0441\\u043a\\u043e\\u0435 \\u041d\\u0430\\u0437\\u0432\\u0430\\u043d\\u0438\\u0435\"";
            
            // prepare buffer with JOSN data
            byte[] rawBytes = new byte[RUSSIAN_JSON.Length];
            for (var i = 0; i < RUSSIAN_JSON.Length; ++i)
                rawBytes[i] = (byte)(RUSSIAN_JSON[i] & (char)0xFF);  // only low bytes

            // parse and check
            var stm = new MemoryStream(rawBytes);
            var trans = new TStreamTransport(stm, null);
            var prot = new TJSONProtocol(trans);
            Debug.Assert(prot.ReadString() == RUSSIAN_TEXT, "reading JSON with hex-encoded chars > 8 bit");
        }
    }
}
