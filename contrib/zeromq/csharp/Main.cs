/*
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
using System.Threading;
using Thrift.Protocol;
using ZMQ;
using ZmqServer;
using ZmqClient;

namespace ZmqServer
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			new Thread(Server.serve).Start();
			Client.work();
		}
		
		static class Server{
			public static void serve(){
				StorageHandler s=new StorageHandler();
				Storage.Processor p=new Storage.Processor(s);
				
				ZMQ.Context c=new ZMQ.Context();
				
				TZmqServer tzs=new TZmqServer(p,c,"tcp://127.0.0.1:9090",ZMQ.SocketType.PAIR);
				tzs.Serve();
			}
			
			class StorageHandler:Storage.Iface{
				int val=0;
				
				public void incr(int amount){
					val+=amount;
					Console.WriteLine("incr({0})",amount);
				}
				
				public int get(){
					return val;
				} 
			}
		}
		
		static class Client{
			public static void work()
			{
				Context ctx=new Context();
				TZmqClient tzc=new TZmqClient(ctx,"tcp://127.0.0.1:9090",SocketType.PAIR);
				TBinaryProtocol p=new TBinaryProtocol(tzc);
				
				Storage.Client client=new Storage.Client(p);
				tzc.Open();
				
				Console.WriteLine(client.@get());
				client.incr(1);
				client.incr(41);
				Console.WriteLine(client.@get());
			}
		}
	}
}
