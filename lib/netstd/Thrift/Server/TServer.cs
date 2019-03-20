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
using Thrift.Transport;
using Thrift.Processor;

namespace Thrift.Server
{
    // ReSharper disable once InconsistentNaming
    public abstract class TServer
    {
        protected readonly ILogger Logger;
        protected TProtocolFactory InputProtocolFactory;
        protected TTransportFactory InputTransportFactory;
        protected ITProcessorFactory ProcessorFactory;
        protected TProtocolFactory OutputProtocolFactory;
        protected TTransportFactory OutputTransportFactory;

        protected TServerEventHandler ServerEventHandler;
        protected TServerTransport ServerTransport;

        protected TServer(ITProcessorFactory processorFactory, TServerTransport serverTransport,
            TTransportFactory inputTransportFactory, TTransportFactory outputTransportFactory,
            TProtocolFactory inputProtocolFactory, TProtocolFactory outputProtocolFactory,
            ILogger logger = null)
        {
            ProcessorFactory = processorFactory ?? throw new ArgumentNullException(nameof(processorFactory));
            ServerTransport = serverTransport;
            InputTransportFactory = inputTransportFactory ?? new TTransportFactory();
            OutputTransportFactory = outputTransportFactory ?? new TTransportFactory();
            InputProtocolFactory = inputProtocolFactory ?? throw new ArgumentNullException(nameof(inputProtocolFactory));
            OutputProtocolFactory = outputProtocolFactory ?? throw new ArgumentNullException(nameof(outputProtocolFactory));
            Logger = logger; // null is absolutely legal
        }

        public void SetEventHandler(TServerEventHandler seh)
        {
            ServerEventHandler = seh;
        }

        public TServerEventHandler GetEventHandler()
        {
            return ServerEventHandler;
        }

        // Log delegation? deprecated, use ILogger 
        protected void LogError( string msg)
        {
            if (Logger != null)
                Logger.LogError(msg);
        }

        public abstract void Stop();

        public virtual void Start()
        {
            // do nothing
        }

        public virtual async Task ServeAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }
    }
}
