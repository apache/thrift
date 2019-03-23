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
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Thrift.Protocol;
using Thrift.Processor;
using Thrift.Transport;

namespace Thrift.Server
{
    //TODO: unhandled exceptions, etc.

    // ReSharper disable once InconsistentNaming
    public class TSimpleAsyncServer : TServer
    {
        private readonly int _clientWaitingDelay;
        private volatile Task _serverTask;

        public TSimpleAsyncServer(ITProcessorFactory itProcessorFactory,
            TServerTransport serverTransport,
            TTransportFactory inputTransportFactory,
            TTransportFactory outputTransportFactory,
            TProtocolFactory inputProtocolFactory,
            TProtocolFactory outputProtocolFactory,
            ILogger logger,
            int clientWaitingDelay = 10)
            : base(itProcessorFactory,
                  serverTransport,
                  inputTransportFactory,
                  outputTransportFactory,
                  inputProtocolFactory,
                  outputProtocolFactory,
                  logger)
        {
            _clientWaitingDelay = clientWaitingDelay;
        }

        public TSimpleAsyncServer(ITProcessorFactory itProcessorFactory,
            TServerTransport serverTransport,
            TTransportFactory inputTransportFactory,
            TTransportFactory outputTransportFactory,
            TProtocolFactory inputProtocolFactory,
            TProtocolFactory outputProtocolFactory,
            ILoggerFactory loggerFactory,
            int clientWaitingDelay = 10)
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
            ILoggerFactory loggerFactory,
            int clientWaitingDelay = 10)
            : this(new TSingletonProcessorFactory(processor),
                  serverTransport,
                  null, // defaults to TTransportFactory()
                  null, // defaults to TTransportFactory()
                  inputProtocolFactory,
                  outputProtocolFactory,
                  loggerFactory.CreateLogger(nameof(TSimpleAsyncServer)),
                  clientWaitingDelay)
        {
        }

        public override async Task ServeAsync(CancellationToken cancellationToken)
        {
            try
            {
                // cancelation token
                _serverTask = Task.Factory.StartNew(() => StartListening(cancellationToken), TaskCreationOptions.LongRunning);
                await _serverTask;
            }
            catch (Exception ex)
            {
                Logger.LogError(ex.ToString());
            }
        }

        private async Task StartListening(CancellationToken cancellationToken)
        {
            ServerTransport.Listen();

            Logger.LogTrace("Started listening at server");

            if (ServerEventHandler != null)
            {
                await ServerEventHandler.PreServeAsync(cancellationToken);
            }

            while (!cancellationToken.IsCancellationRequested)
            {
                if (ServerTransport.IsClientPending())
                {
                    Logger.LogTrace("Waiting for client connection");

                    try
                    {
                        var client = await ServerTransport.AcceptAsync(cancellationToken);
                        await Task.Factory.StartNew(() => Execute(client, cancellationToken), cancellationToken);
                    }
                    catch (TTransportException ttx)
                    {
                        Logger.LogTrace($"Transport exception: {ttx}");

                        if (ttx.Type != TTransportException.ExceptionType.Interrupted)
                        {
                            Logger.LogError(ttx.ToString());
                        }
                    }
                }
                else
                {
                    try
                    {
                        await Task.Delay(TimeSpan.FromMilliseconds(_clientWaitingDelay), cancellationToken);
                    }
                    catch (TaskCanceledException) { }
                }
            }

            ServerTransport.Close();

            Logger.LogTrace("Completed listening at server");
        }

        public override void Stop()
        {
        }

        private async Task Execute(TTransport client, CancellationToken cancellationToken)
        {
            Logger.LogTrace("Started client request processing");

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

                    if (ServerEventHandler != null)
                    {
                        connectionContext = await ServerEventHandler.CreateContextAsync(inputProtocol, outputProtocol, cancellationToken);
                    }

                    while (!cancellationToken.IsCancellationRequested)
                    {
                        if (!await inputTransport.PeekAsync(cancellationToken))
                        {
                            break;
                        }

                        if (ServerEventHandler != null)
                        {
                            await ServerEventHandler.ProcessContextAsync(connectionContext, inputTransport, cancellationToken);
                        }

                        if (!await processor.ProcessAsync(inputProtocol, outputProtocol, cancellationToken))
                        {
                            break;
                        }
                    }
                }
                catch (TTransportException ttx)
                {
                    Logger.LogTrace($"Transport exception: {ttx}");
                }
                catch (Exception x)
                {
                    Logger.LogError($"Error: {x}");
                }

                if (ServerEventHandler != null)
                {
                    await ServerEventHandler.DeleteContextAsync(connectionContext, inputProtocol, outputProtocol, cancellationToken);
                }

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

            Logger.LogTrace("Completed client request processing");
        }
    }
}
