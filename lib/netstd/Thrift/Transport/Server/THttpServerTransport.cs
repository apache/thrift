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
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;
using Thrift.Processor;
using Thrift.Protocol;
using Thrift.Transport.Client;

namespace Thrift.Transport.Server
{
    // ReSharper disable once InconsistentNaming
    public class THttpServerTransport
    {
        protected const string ContentType = "application/x-thrift";
        private readonly ILogger _logger;
        private readonly RequestDelegate _next;
        protected Encoding Encoding = Encoding.UTF8;

        protected TProtocolFactory InputProtocolFactory;
        protected TProtocolFactory OutputProtocolFactory;

        protected TTransportFactory InputTransportFactory;
        protected TTransportFactory OutputTransportFactory;

        protected ITAsyncProcessor Processor;

        public THttpServerTransport(ITAsyncProcessor processor, RequestDelegate next = null, ILoggerFactory loggerFactory = null)
            : this(processor, new TBinaryProtocol.Factory(), null, next, loggerFactory)
        {
        }

        public THttpServerTransport(
            ITAsyncProcessor processor, 
            TProtocolFactory protocolFactory, 
            TTransportFactory transFactory = null, 
            RequestDelegate next = null,
            ILoggerFactory loggerFactory = null)
            : this(processor, protocolFactory, protocolFactory, transFactory, transFactory, next, loggerFactory)
        {
        }

        public THttpServerTransport(
            ITAsyncProcessor processor, 
            TProtocolFactory inputProtocolFactory,
            TProtocolFactory outputProtocolFactory,
            TTransportFactory inputTransFactory = null,
            TTransportFactory outputTransFactory = null,
            RequestDelegate next = null, 
            ILoggerFactory loggerFactory = null)
        {
            // loggerFactory == null is not illegal anymore

            Processor = processor ?? throw new ArgumentNullException(nameof(processor));
            InputProtocolFactory = inputProtocolFactory ?? throw new ArgumentNullException(nameof(inputProtocolFactory));
            OutputProtocolFactory = outputProtocolFactory ?? throw new ArgumentNullException(nameof(outputProtocolFactory));

            InputTransportFactory = inputTransFactory;
            OutputTransportFactory = outputTransFactory;

            _next = next;
            _logger = (loggerFactory != null) ? loggerFactory.CreateLogger<THttpServerTransport>() : new NullLogger<THttpServerTransport>();
        }

        public async Task Invoke(HttpContext context)
        {
            context.Response.ContentType = ContentType;
            await ProcessRequestAsync(context, context.RequestAborted); //TODO: check for correct logic
        }

        public async Task ProcessRequestAsync(HttpContext context, CancellationToken cancellationToken)
        {
            var transport = new TStreamTransport(context.Request.Body, context.Response.Body);

            try
            {
                var intrans = (InputTransportFactory != null) ? InputTransportFactory.GetTransport(transport) : transport;
                var outtrans = (OutputTransportFactory != null) ? OutputTransportFactory.GetTransport(transport) : transport;

                var input = InputProtocolFactory.GetProtocol(intrans);
                var output = OutputProtocolFactory.GetProtocol(outtrans);

                while (await Processor.ProcessAsync(input, output, cancellationToken))
                {
                    if (!context.Response.HasStarted)  // oneway method called
                        await context.Response.Body.FlushAsync(cancellationToken);
                }
            }
            catch (TTransportException)
            {
                if (!context.Response.HasStarted)  // if something goes bust, let the client know
                    context.Response.StatusCode = 500;
            }
            finally
            {
                transport.Close();
            }
        }
    }
}
