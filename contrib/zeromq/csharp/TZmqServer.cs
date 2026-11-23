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
using Thrift;
using Thrift.Server;
using Thrift.Transport;
using Thrift.Protocol;
using ZMQ;
using System.IO;

using System.Collections.Generic;

namespace ZmqServer
{
	public class TZmqServer
	{
		Socket _socket ;
		TProcessor _processor;
		
		void debug (string msg)
		{
			//Uncomment to enable debug
//			Console.WriteLine (msg);
		}

		public TZmqServer (TProcessor processor, Context ctx, String endpoint, SocketType sockType)
		{
			new TSimpleServer (processor,null);
			_socket = ctx.Socket (sockType);
			_socket.Bind (endpoint);
			_processor = processor;
		}

		public void ServeOne ()
		{
			debug ("Server_ServeOne");
			Byte[] msg = _socket.Recv ();
			MemoryStream istream = new MemoryStream (msg);
			MemoryStream ostream = new MemoryStream ();
			TProtocol tProtocol = new TBinaryProtocol (new TStreamTransport (istream, ostream));
			_processor.Process (tProtocol, tProtocol);

			if (ostream.Length != 0) {
				byte[] newBuf = new byte[ostream.Length];
				Array.Copy (ostream.GetBuffer (), newBuf, ostream.Length);
				debug (string.Format ("Server_ServeOne sending {0}b", ostream.Length));
				_socket.Send (newBuf);
			}
		}

		public void Serve ()
		{
			while (true)
				ServeOne ();
		}
	}
}

