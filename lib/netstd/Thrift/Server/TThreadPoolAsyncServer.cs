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
using System.Threading;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Processor;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;

namespace Thrift.Server
{
    /// <summary>
    /// Server that uses C# built-in ThreadPool to spawn threads when handling requests.
    /// </summary>
    public class TThreadPoolAsyncServer : TServer
    {
        private const int DEFAULT_MIN_THREADS = -1;  // use .NET ThreadPool defaults
        private const int DEFAULT_MAX_THREADS = -1;  // use .NET ThreadPool defaults
        private volatile bool stop = false;

        private CancellationToken ServerCancellationToken;

        public struct Configuration
        {
            public int MinWorkerThreads;
            public int MaxWorkerThreads;
            public int MinIOThreads;
            public int MaxIOThreads;

            public Configuration(int min = DEFAULT_MIN_THREADS, int max = DEFAULT_MAX_THREADS)
            {
                MinWorkerThreads = min;
                MaxWorkerThreads = max;
                MinIOThreads = min;
                MaxIOThreads = max;
            }

            public Configuration(int minWork, int maxWork, int minIO, int maxIO)
            {
                MinWorkerThreads = minWork;
                MaxWorkerThreads = maxWork;
                MinIOThreads = minIO;
                MaxIOThreads = maxIO;
            }
        }

        public TThreadPoolAsyncServer(ITAsyncProcessor processor, TServerTransport serverTransport, ILogger logger = null)
            : this(new TSingletonProcessorFactory(processor), serverTransport,
             null, null, // defaults to TTransportFactory()
             new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(),
             new Configuration(), logger)
        {
        }

        public TThreadPoolAsyncServer(ITAsyncProcessor processor,
         TServerTransport serverTransport,
         TTransportFactory transportFactory,
         TProtocolFactory protocolFactory)
            : this(new TSingletonProcessorFactory(processor), serverTransport,
               transportFactory, transportFactory,
               protocolFactory, protocolFactory,
               new Configuration())
        {
        }

        public TThreadPoolAsyncServer(ITProcessorFactory processorFactory,
                     TServerTransport serverTransport,
                     TTransportFactory transportFactory,
                     TProtocolFactory protocolFactory)
            : this(processorFactory, serverTransport,
             transportFactory, transportFactory,
             protocolFactory, protocolFactory,
             new Configuration())
        {
        }

        public TThreadPoolAsyncServer(ITProcessorFactory processorFactory,
                     TServerTransport serverTransport,
                     TTransportFactory inputTransportFactory,
                     TTransportFactory outputTransportFactory,
                     TProtocolFactory inputProtocolFactory,
                     TProtocolFactory outputProtocolFactory,
                     int minThreadPoolThreads, int maxThreadPoolThreads, ILogger logger= null)
            : this(processorFactory, serverTransport, inputTransportFactory, outputTransportFactory,
             inputProtocolFactory, outputProtocolFactory,
             new Configuration(minThreadPoolThreads, maxThreadPoolThreads),
             logger)
        {
        }

        public TThreadPoolAsyncServer(ITProcessorFactory processorFactory,
                     TServerTransport serverTransport,
                     TTransportFactory inputTransportFactory,
                     TTransportFactory outputTransportFactory,
                     TProtocolFactory inputProtocolFactory,
                     TProtocolFactory outputProtocolFactory,
                     Configuration threadConfig,
                     ILogger logger = null)
            : base(processorFactory, serverTransport, inputTransportFactory, outputTransportFactory,
            inputProtocolFactory, outputProtocolFactory, logger)
        {
            lock (typeof(TThreadPoolAsyncServer))
            {
                if ((threadConfig.MaxWorkerThreads > 0) || (threadConfig.MaxIOThreads > 0))
                {
                    int work, comm;
                    ThreadPool.GetMaxThreads(out work, out comm);
                    if (threadConfig.MaxWorkerThreads > 0)
                        work = threadConfig.MaxWorkerThreads;
                    if (threadConfig.MaxIOThreads > 0)
                        comm = threadConfig.MaxIOThreads;
                    if (!ThreadPool.SetMaxThreads(work, comm))
                        throw new Exception("Error: could not SetMaxThreads in ThreadPool");
                }

                if ((threadConfig.MinWorkerThreads > 0) || (threadConfig.MinIOThreads > 0))
                {
                    int work, comm;
                    ThreadPool.GetMinThreads(out work, out comm);
                    if (threadConfig.MinWorkerThreads > 0)
                        work = threadConfig.MinWorkerThreads;
                    if (threadConfig.MinIOThreads > 0)
                        comm = threadConfig.MinIOThreads;
                    if (!ThreadPool.SetMinThreads(work, comm))
                        throw new Exception("Error: could not SetMinThreads in ThreadPool");
                }
            }
        }


        /// <summary>
        /// Use new ThreadPool thread for each new client connection.
        /// </summary>
        public override async Task ServeAsync(CancellationToken cancellationToken)
        {
            ServerCancellationToken = cancellationToken;
            try
            {
                try
                {
                    ServerTransport.Listen();
                }
                catch (TTransportException ttx)
                {
                    LogError("Error, could not listen on ServerTransport: " + ttx);
                    return;
                }

                //Fire the preServe server event when server is up but before any client connections
                if (ServerEventHandler != null)
                    await ServerEventHandler.PreServeAsync(cancellationToken);

                while (!stop)
                {
                    int failureCount = 0;
                    try
                    {
                        TTransport client = await ServerTransport.AcceptAsync(cancellationToken);
                        ThreadPool.QueueUserWorkItem(this.Execute, client);
                    }
                    catch (TTransportException ttx)
                    {
                        if (!stop || ttx.Type != TTransportException.ExceptionType.Interrupted)
                        {
                            ++failureCount;
                            LogError(ttx.ToString());
                        }

                    }
                }

                if (stop)
                {
                    try
                    {
                        ServerTransport.Close();
                    }
                    catch (TTransportException ttx)
                    {
                        LogError("TServerTransport failed on close: " + ttx.Message);
                    }
                    stop = false;
                }

            }
            finally
            {
                ServerCancellationToken = default(CancellationToken);
            }
        }

        /// <summary>
        /// Loops on processing a client forever
        /// threadContext will be a TTransport instance
        /// </summary>
        /// <param name="threadContext"></param>
        private void Execute(object threadContext)
        {
            var cancellationToken = ServerCancellationToken;

            using (TTransport client = (TTransport)threadContext)
            {
                ITAsyncProcessor processor = ProcessorFactory.GetAsyncProcessor(client, this);
                TTransport inputTransport = null;
                TTransport outputTransport = null;
                TProtocol inputProtocol = null;
                TProtocol outputProtocol = null;
                object connectionContext = null;
                try
                {
                    try
                    {
                        inputTransport = InputTransportFactory.GetTransport(client);
                        outputTransport = OutputTransportFactory.GetTransport(client);
                        inputProtocol = InputProtocolFactory.GetProtocol(inputTransport);
                        outputProtocol = OutputProtocolFactory.GetProtocol(outputTransport);

                        //Recover event handler (if any) and fire createContext server event when a client connects
                        if (ServerEventHandler != null)
                            connectionContext = ServerEventHandler.CreateContextAsync(inputProtocol, outputProtocol, cancellationToken).Result;

                        //Process client requests until client disconnects
                        while (!stop)
                        {
                            if (! inputTransport.PeekAsync(cancellationToken).Result)
                                break;

                            //Fire processContext server event
                            //N.B. This is the pattern implemented in C++ and the event fires provisionally.
                            //That is to say it may be many minutes between the event firing and the client request
                            //actually arriving or the client may hang up without ever makeing a request.
                            if (ServerEventHandler != null)
                                ServerEventHandler.ProcessContextAsync(connectionContext, inputTransport, cancellationToken).Wait();
                            //Process client request (blocks until transport is readable)
                            if (!processor.ProcessAsync(inputProtocol, outputProtocol, cancellationToken).Result)
                                break;
                        }
                    }
                    catch (TTransportException)
                    {
                        //Usually a client disconnect, expected
                    }
                    catch (Exception x)
                    {
                        //Unexpected
                        LogError("Error: " + x);
                    }

                    //Fire deleteContext server event after client disconnects
                    if (ServerEventHandler != null)
                        ServerEventHandler.DeleteContextAsync(connectionContext, inputProtocol, outputProtocol, cancellationToken).Wait();

                }
                finally
                {
                    //Close transports
                    inputTransport?.Close();
                    outputTransport?.Close();

                    // disposable stuff should be disposed
                    inputProtocol?.Dispose();
                    outputProtocol?.Dispose();
                    inputTransport?.Dispose();
                    outputTransport?.Dispose();
                }
            }
        }

        public override void Stop()
        {
            stop = true;
            ServerTransport?.Close();
        }
    }
}
