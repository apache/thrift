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
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

using System;
using Thrift.Transport;
using Thrift.Protocol;

namespace Thrift.Server
{
	/// <summary>
	/// Simple single-threaded server for testing
	/// </summary>
	public class TSimpleServer : TServer
	{
		private bool stop = false;

		public TSimpleServer(TProcessor processor,
						  TServerTransport serverTransport)
			:base(processor, serverTransport, new TTransportFactory(), new TTransportFactory(), new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(), DefaultLogDelegate)
		{
		}

		public TSimpleServer(TProcessor processor,
							TServerTransport serverTransport,
							LogDelegate logDel)
			: base(processor, serverTransport, new TTransportFactory(), new TTransportFactory(), new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(), logDel)
		{
		}

		public TSimpleServer(TProcessor processor,
						  TServerTransport serverTransport,
						  TTransportFactory transportFactory)
			:base(processor,
				 serverTransport,
				 transportFactory,
				 transportFactory,
				 new TBinaryProtocol.Factory(),
				 new TBinaryProtocol.Factory(),
			     DefaultLogDelegate)
		{
		}

		public TSimpleServer(TProcessor processor,
						  TServerTransport serverTransport,
						  TTransportFactory transportFactory,
						  TProtocolFactory protocolFactory)
			:base(processor,
				 serverTransport,
				 transportFactory,
				 transportFactory,
				 protocolFactory,
				 protocolFactory,
				 DefaultLogDelegate)
		{
		}

		public override void Serve()
		{
			try
			{
				serverTransport.Listen();
			}
			catch (TTransportException ttx)
			{
				logDelegate(ttx.ToString());
				return;
			}

			while (!stop)
			{
				TTransport client = null;
				TTransport inputTransport = null;
				TTransport outputTransport = null;
				TProtocol inputProtocol = null;
				TProtocol outputProtocol = null;
				try
				{
          using(client = serverTransport.Accept())
          {
            if (client != null)
            {
              using(inputTransport = inputTransportFactory.GetTransport(client))
              {
                using (outputTransport = outputTransportFactory.GetTransport(client))
                {
                  inputProtocol = inputProtocolFactory.GetProtocol(inputTransport);
                  outputProtocol = outputProtocolFactory.GetProtocol(outputTransport);
                  while (processor.Process(inputProtocol, outputProtocol)) { }
                }
              }
            }
          }
        }
        catch (TTransportException ttx)
        {
          // Client died, just move on
          if (stop)
          {
            logDelegate("TSimpleServer was shutting down, caught " + ttx.GetType().Name);
          }
        }
        catch (Exception x)
        {
          logDelegate(x.ToString());
        }
      }

			if (stop)
			{
				try
				{
					serverTransport.Close();
				}
				catch (TTransportException ttx)
				{
					logDelegate("TServerTranport failed on close: " + ttx.Message);
				}
				stop = false;
			}
		}

		public override void Stop()
		{
			stop = true;
			serverTransport.Close();
		}
	}
}
