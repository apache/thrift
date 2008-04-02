//
//  TSimpleServer.cs
//
//  Begin:  Dec 3, 2007
//  Authors:
//		Will Palmeri <wpalmeri@imeem.com>
//
//  Distributed under the Thrift Software License
//
//  See accompanying file LICENSE or visit the Thrift site at:
//  http://developers.facebook.com/thrift/using
using System;
using System.Collections.Generic;
using System.Text;
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
			:base(processor, serverTransport, new TTransportFactory(), new TTransportFactory(), new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory())
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
				 new TBinaryProtocol.Factory())
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
				 protocolFactory)
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
				Console.Error.WriteLine(ttx);
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
					client = serverTransport.Accept();
					if (client != null)
					{
						inputTransport = inputTransportFactory.GetTransport(client);
						outputTransport = outputTransportFactory.GetTransport(client);
						inputProtocol = inputProtocolFactory.GetProtocol(inputTransport);
						outputProtocol = outputProtocolFactory.GetProtocol(outputTransport);
						while (processor.Process(inputProtocol, outputProtocol)) { }
					}
				}
				catch (TTransportException)
				{
					// Client died, just move on
				}
				catch (Exception x)
				{
					Console.Error.WriteLine(x);
				}

				if (inputTransport != null)
				{
					inputTransport.Close();
				}

				if (outputTransport != null)
				{
					outputTransport.Close();
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
					Console.Error.WriteLine("TServerTrasnport failed on close: " + ttx.Message);
				}
				stop = false;
			}
		}

		public override void Stop()
		{
			stop = true;
		}
	}
}
