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
using System.Text;

namespace Thrift
{
    public class TConfiguration
    {
        public const int DEFAULT_MAX_MESSAGE_SIZE = 100 * 1024 * 1024;
        public const int DEFAULT_MAX_FRAME_SIZE = 16384000;      // this value is used consistently across all Thrift libraries
        public const int DEFAULT_RECURSION_DEPTH = 64;

        public int MaxMessageSize { get; set; } = DEFAULT_MAX_MESSAGE_SIZE;
        public int MaxFrameSize { get; set; } = DEFAULT_MAX_FRAME_SIZE;
        public int RecursionLimit { get; set; } = DEFAULT_RECURSION_DEPTH;

        // TODO(JensG): add connection and i/o timeouts
    }
}
