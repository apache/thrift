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
using Thrift.Protocol;
using Thrift.Transport;
using System.IO;

namespace Thrift.Server
{
	public abstract class TServer
	{
		/**
		 * Core processor
		 */
		protected TProcessor processor;

		/**
		 * Server transport
		 */
		protected TServerTransport serverTransport;

		/**
		 * Input Transport Factory
		 */
		protected TTransportFactory inputTransportFactory;

		/**
		 * Output Transport Factory
		 */
		protected TTransportFactory outputTransportFactory;

		/**
		 * Input Protocol Factory
		 */
		protected TProtocolFactory inputProtocolFactory;

		/**
		 * Output Protocol Factory
		 */
		protected TProtocolFactory outputProtocolFactory;
		public delegate void LogDelegate(string str);
		protected LogDelegate logDelegate;

		/**
		 * Default constructors.
		 */

		public TServer(TProcessor processor,
						  TServerTransport serverTransport)
			:this(processor, serverTransport, new TTransportFactory(), new TTransportFactory(), new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(), DefaultLogDelegate)
		{
		}

		public TServer(TProcessor processor,
						TServerTransport serverTransport,
						LogDelegate logDelegate)
			: this(processor, serverTransport, new TTransportFactory(), new TTransportFactory(), new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(), DefaultLogDelegate)
		{
		}

		public TServer(TProcessor processor,
						  TServerTransport serverTransport,
						  TTransportFactory transportFactory)
			:this(processor,
				 serverTransport,
				 transportFactory,
				 transportFactory,
				 new TBinaryProtocol.Factory(),
				 new TBinaryProtocol.Factory(),
				 DefaultLogDelegate)
		{
		}

		public TServer(TProcessor processor,
						  TServerTransport serverTransport,
						  TTransportFactory transportFactory,
						  TProtocolFactory protocolFactory)
			:this(processor,
				 serverTransport,
				 transportFactory,
				 transportFactory,
				 protocolFactory,
				 protocolFactory,
			     DefaultLogDelegate)
		{
		}

		public TServer(TProcessor processor,
						  TServerTransport serverTransport,
						  TTransportFactory inputTransportFactory,
						  TTransportFactory outputTransportFactory,
						  TProtocolFactory inputProtocolFactory,
						  TProtocolFactory outputProtocolFactory,
						  LogDelegate logDelegate)
		{
			this.processor = processor;
			this.serverTransport = serverTransport;
			this.inputTransportFactory = inputTransportFactory;
			this.outputTransportFactory = outputTransportFactory;
			this.inputProtocolFactory = inputProtocolFactory;
			this.outputProtocolFactory = outputProtocolFactory;
			this.logDelegate = logDelegate;
		}

		/**
		 * The run method fires up the server and gets things going.
		 */
		public abstract void Serve();

		public abstract void Stop();

		protected static void DefaultLogDelegate(string s)
		{
			Console.Error.WriteLine(s);
		}
	}
}

