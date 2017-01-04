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
using Test;

namespace ThirftTest
{
    public class Program
    {
        public static int Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("must provide 'server' or 'client' arg");
                return -1;
            }

            try
            {
                Console.SetBufferSize(Console.BufferWidth, 4096);
            }
            catch (Exception)
            {
                Console.WriteLine("Failed to grow scroll-back buffer");
            }

            var subArgs = new string[args.Length - 1];
            for (var i = 1; i < args.Length; i++)
            {
                subArgs[i - 1] = args[i];
            }

            if (args[0] == "client")
            {
                return TestClient.Execute(subArgs);
            }

            if (args[0] == "server")
            {
                return TestServer.Execute(subArgs) ? 0 : 1;
            }

            Console.WriteLine("first argument must be 'server' or 'client'");
            return 0;
        }
    }
}


