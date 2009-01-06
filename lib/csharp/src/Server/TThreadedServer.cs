//
//  TThreadPoolServer.cs
//
//  Begin:  Apr 21, 2008
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
using System.Threading;
using Thrift.Protocol;
using Thrift.Transport;

namespace Thrift.Server
{
	/// <summary>
	/// Server that uses C# threads (as opposed to the ThreadPool) when handling requests
	/// </summary>
	public class TThreadedServer : TServer
	{
		private const int DEFAULT_MAX_THREADS = 100;
		private volatile bool stop = false;
		private readonly int maxThreads;

		private Queue<TTransport> clientQueue;
		private HashSet<Thread> clientThreads;
		private object clientLock;
		private Thread workerThread;

		public TThreadedServer(TProcessor processor, TServerTransport serverTransport)
			: this(processor, serverTransport,
				 new TTransportFactory(), new TTransportFactory(),
				 new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(),
				 DEFAULT_MAX_THREADS, DefaultLogDelegate)
		{
		}

		public TThreadedServer(TProcessor processor, TServerTransport serverTransport, LogDelegate logDelegate)
			: this(processor, serverTransport,
				 new TTransportFactory(), new TTransportFactory(),
				 new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(),
				 DEFAULT_MAX_THREADS, logDelegate)
		{
		}


		public TThreadedServer(TProcessor processor,
								 TServerTransport serverTransport,
								 TTransportFactory transportFactory,
								 TProtocolFactory protocolFactory)
			: this(processor, serverTransport,
				 transportFactory, transportFactory,
				 protocolFactory, protocolFactory,
				 DEFAULT_MAX_THREADS, DefaultLogDelegate)
		{
		}

		public TThreadedServer(TProcessor processor,
								 TServerTransport serverTransport,
								 TTransportFactory inputTransportFactory,
								 TTransportFactory outputTransportFactory,
								 TProtocolFactory inputProtocolFactory,
								 TProtocolFactory outputProtocolFactory,
								 int maxThreads, LogDelegate logDel)
			: base(processor, serverTransport, inputTransportFactory, outputTransportFactory,
				  inputProtocolFactory, outputProtocolFactory, logDel)
		{
			this.maxThreads = maxThreads;
			clientQueue = new Queue<TTransport>();
			clientLock = new object();
			clientThreads = new HashSet<Thread>();
		}

		/// <summary>
		/// Use new Thread for each new client connection. block until numConnections < maxTHreads
		/// </summary>
		public override void Serve()
		{
			try
			{
				//start worker thread
				workerThread = new Thread(new ThreadStart(Execute));
				workerThread.Start();
				serverTransport.Listen();
			}
			catch (TTransportException ttx)
			{
				logDelegate("Error, could not listen on ServerTransport: " + ttx);
				return;
			}

			while (!stop)
			{
				int failureCount = 0;
				try
				{
					TTransport client = serverTransport.Accept();
					lock (clientLock)
					{
						clientQueue.Enqueue(client);
						Monitor.Pulse(clientLock);
					}
				}
				catch (TTransportException ttx)
				{
					if (stop)
					{
						logDelegate("TThreadPoolServer was shutting down, caught " + ttx);
					}
					else
					{
						++failureCount;
						logDelegate(ttx.ToString());
					}

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
					logDelegate("TServeTransport failed on close: " + ttx.Message);
				}
				stop = false;
			}
		}

		/// <summary>
		/// Loops on processing a client forever
		/// threadContext will be a TTransport instance
		/// </summary>
		/// <param name="threadContext"></param>
		private void Execute()
		{
			while (!stop)
			{
				TTransport client;
				Thread t;
				lock (clientLock)
				{
					//don't dequeue if too many connections
					while (clientThreads.Count >= maxThreads)
					{
						Monitor.Wait(clientLock);
					}

					while (clientQueue.Count == 0)
					{
						Monitor.Wait(clientLock);
					}

					client = clientQueue.Dequeue();
					t = new Thread(new ParameterizedThreadStart(ClientWorker));
					clientThreads.Add(t);
				}
				//start processing requests from client on new thread
				t.Start(client);
			}
		}

		private void ClientWorker(Object context)
		{
			TTransport client = (TTransport)context;
			TTransport inputTransport = null;
			TTransport outputTransport = null;
			TProtocol inputProtocol = null;
			TProtocol outputProtocol = null;
			try
			{
				inputTransport = inputTransportFactory.GetTransport(client);
				outputTransport = outputTransportFactory.GetTransport(client);
				inputProtocol = inputProtocolFactory.GetProtocol(inputTransport);
				outputProtocol = outputProtocolFactory.GetProtocol(outputTransport);
				while (processor.Process(inputProtocol, outputProtocol))
				{
					//keep processing requests until client disconnects
				}
			}
			catch (TTransportException)
			{
			}
			catch (Exception x)
			{
				logDelegate("Error: " + x);
			}

			if (inputTransport != null)
			{
				inputTransport.Close();
			}
			if (outputTransport != null)
			{
				outputTransport.Close();
			}

			lock (clientLock)
			{
				clientThreads.Remove(Thread.CurrentThread);
				Monitor.Pulse(clientLock);
			}
			return;
		}

		public override void Stop()
		{
			stop = true;
			serverTransport.Close();
			//clean up all the threads myself
			workerThread.Abort();
			foreach (Thread t in clientThreads)
			{
				t.Abort();
			}
		}
	}
}
