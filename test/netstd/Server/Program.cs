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
using System.Linq;
using System.Collections.Generic;
using ThriftTest;

namespace Server
{
    public class Program
    {
        public static int Main(string[] args)
        {
            try
            {
                Console.SetBufferSize(Console.BufferWidth, 4096);
            }
            catch (Exception)
            {
                Console.WriteLine("Failed to grow scroll-back buffer");
            }

            // run whatever mode is choosen, default to test impl
            var argslist = new List<string>(args);
            switch (argslist.FirstOrDefault())
            {
                case "server":  // crosstest wants to pass this, so just emit a hint and ignore
                    Console.WriteLine("Hint: The 'server' argument is no longer required.");
                    argslist.RemoveAt(0);
                    return TestServer.Execute(argslist);
                case "--help":
                    PrintHelp();
                    return 0;
                default:
                    return TestServer.Execute(argslist);
            }
        }

        private static void PrintHelp()
        {
            Console.WriteLine("Usage:");
            Console.WriteLine("  Server  [options]");
            Console.WriteLine("  Server  --help");
            Console.WriteLine("");

            ServerParam.PrintOptionsHelp();
        }
    }
}


