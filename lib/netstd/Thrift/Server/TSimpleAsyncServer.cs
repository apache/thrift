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
using System.Threading;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Processor;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;

#pragma warning disable IDE0079 // net20 - unneeded suppression
#pragma warning disable IDE0290 // net8 - primary CTOR

namespace Thrift.Server
{

    // ReSharper disable once InconsistentNaming
    public class TSimpleAsyncServer : TServer
    {
        private volatile bool stop = false;

        private CancellationToken ServerCancellationToken;

        public TSimpleAsyncServer(ITProcessorFactory itProcessorFactory,
            TServerTransport serverTransport,
            TTransportFactory inputTransportFactory,
            TTransportFactory outputTransportFactory,
            TProtocolFactory inputProtocolFactory,
            TProtocolFactory outputProtocolFactory,
            ILogger logger)
            : base(itProcessorFactory,
                  serverTransport,
                  inputTransportFactory,
                  outputTransportFactory,
                  inputProtocolFactory,
                  outputProtocolFactory,
                  logger)
        {
        }

        public TSimpleAsyncServer(ITProcessorFactory itProcessorFactory,
            TServerTransport serverTransport,
            TTransportFactory inputTransportFactory,
            TTransportFactory outputTransportFactory,
            TProtocolFactory inputProtocolFactory,
            TProtocolFactory outputProtocolFactory,
            ILoggerFactory loggerFactory)
            : this(itProcessorFactory,
                  serverTransport,
                  inputTransportFactory,
                  outputTransportFactory,
                  inputProtocolFactory,
                  outputProtocolFactory,
                  loggerFactory.CreateLogger<TSimpleAsyncServer>())
        {
        }

        public TSimpleAsyncServer(ITAsyncProcessor processor,
            TServerTransport serverTransport,
            TProtocolFactory inputProtocolFactory,
            TProtocolFactory outputProtocolFactory,
            ILoggerFactory loggerFactory)
            : this(new TSingletonProcessorFactory(processor),
                  serverTransport,
                  null, // defaults to TTransportFactory()
                  null, // defaults to TTransportFactory()
                  inputProtocolFactory,
                  outputProtocolFactory,
                  loggerFactory.CreateLogger(nameof(TSimpleAsyncServer)))
        {
        }

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

                while (!(stop || ServerCancellationToken.IsCancellationRequested))
                {
                    try
                    {
                        using (TTransport client = await ServerTransport.AcceptAsync(cancellationToken))
                        {
                            await ExecuteAsync(client);
                        }
                    }
                    catch (TaskCanceledException)
                    {
                        stop = true;
                    }
                    catch (TTransportException ttx)
                    {
                        if (!stop || ttx.Type != TTransportException.ExceptionType.Interrupted)
                        {
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
                ServerCancellationToken = default;
            }
        }

        /// <summary>
        /// Loops on processing a client forever
        /// client will be a TTransport instance
        /// </summary>
        /// <param name="client"></param>
        private async Task ExecuteAsync(TTransport client)
        {
            var cancellationToken = ServerCancellationToken;

            var processor = ProcessorFactory.GetAsyncProcessor(client, this);

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
                        connectionContext = await ServerEventHandler.CreateContextAsync(inputProtocol, outputProtocol, cancellationToken);

                    //Process client requests until client disconnects
                    while (!(stop || cancellationToken.IsCancellationRequested))
                    {
                        if (!await inputTransport.PeekAsync(cancellationToken))
                            break;

                        //Fire processContext server event
                        //N.B. This is the pattern implemented in C++ and the event fires provisionally.
                        //That is to say it may be many minutes between the event firing and the client request
                        //actually arriving or the client may hang up without ever makeing a request.
                        if (ServerEventHandler != null)
                            await ServerEventHandler.ProcessContextAsync(connectionContext, inputTransport, cancellationToken);

                        //Process client request (blocks until transport is readable)
                        if (!await processor.ProcessAsync(inputProtocol, outputProtocol, cancellationToken))
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
                    await ServerEventHandler.DeleteContextAsync(connectionContext, inputProtocol, outputProtocol, cancellationToken);

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

        public override void Stop()
        {
            stop = true;
            ServerTransport?.Close();
        }
    }
}
