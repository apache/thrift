/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
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
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Thrift.Server;
using Thrift.Transport;

namespace CSharpAsyncTutorial.Server
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                CalculatorHandler handler = new CalculatorHandler();
                Calculator.Processor processor = new Calculator.Processor(handler);
                TServerTransport serverTransport = new TServerSocket(9090);
                TServer server = new TSimpleServer(processor, serverTransport,(str)=>Console.WriteLine($"Thrift Log:{str}"));

                // Use this for a multithreaded server
                // server = new TThreadPoolServer(processor, serverTransport);

                Console.WriteLine("Starting the server...");
                server.Serve();
            }
            catch (Exception x)
            {
                Console.WriteLine(x.StackTrace);
            }
            Console.WriteLine("done.");
        }
    }

    public class CalculatorHandler :Calculator.ISync
    {
        public CalculatorHandler()
        {
        }

        public void ping()
        {
            Console.WriteLine("ping()");
        }

        public int add(int n1, int n2)
        {
            //Console.WriteLine("add({0},{1})", n1, n2);
            return n1 + n2;
        }

        public void zip()
        {
            Console.WriteLine("zip()");
            throw new InvalidOperation();
        }

        public int calculate(int logid, Work w)
        {
            var ex = new InvalidOperation();
            ex.WhatOp =(int)Operation.ADD;
            ex.Why = "Invalid calculate";
            throw ex;
        }

        public SharedStruct getStruct(int key)
        {
            throw new NotImplementedException();
        }
    }
}
