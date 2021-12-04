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
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Thrift;
using Thrift.Protocol;
using Thrift.Server;
using Thrift.Transport;
using Thrift.Transport.Server;
using tutorial;
using shared;
using Thrift.Processor;
using System.Diagnostics;

#pragma warning disable IDE0057  // substr

namespace Server
{
    public static class LoggingHelper
    {
        public static ILoggerFactory LogFactory { get; } = LoggerFactory.Create(builder => {
            ConfigureLogging(builder);
        });

        public static void ConfigureLogging(ILoggingBuilder logging)
        {
            logging.SetMinimumLevel(LogLevel.Trace);
            logging.AddConsole();
            logging.AddDebug();
        }

        public static ILogger<T> CreateLogger<T>() => LogFactory.CreateLogger<T>();
    }

    public class Program
    {
        private static readonly ILogger Logger = LoggingHelper.CreateLogger<Program>();
        private static readonly TConfiguration Configuration = null;  // new TConfiguration() if  needed

        public static void Main(string[] args)
        {
            args ??= Array.Empty<string>();

            if (args.Any(x => x.StartsWith("-help", StringComparison.OrdinalIgnoreCase)))
            {
                DisplayHelp();
                return;
            }

            using (var source = new CancellationTokenSource())
            {
                RunAsync(args, source.Token).GetAwaiter().GetResult();

                Logger.LogInformation("Press any key to stop...");

                Console.ReadLine();
                source.Cancel();
            }

            Logger.LogInformation("Server stopped");
        }


        private static void DisplayHelp()
        {
            Logger.LogInformation(@"
Usage: 
    Server -help
        will diplay help information 

    Server -tr:<transport> -bf:<buffering> -pr:<protocol>  [-multiplex]
        will run server with specified arguments (tcp transport, no buffering, and binary protocol by default)

Options:
    -tr (transport): 
        tcp - (default) tcp transport (localhost:9090)
        tcptls - tcp transport with tls (localhost:9090)
        namedpipe - namedpipe transport (pipe "".test"")
        http - http transport (localhost:9090)

    -bf (buffering): 
        none - (default) no buffering
        buffered - buffered transport
        framed - framed transport

    -pr (protocol): 
        binary - (default) binary protocol
        compact - compact protocol
        json - json protocol

    -multiplex - adds multiplexed protocol

Sample:
    Server -tr:tcp
");
        }

        private static async Task RunAsync(string[] args, CancellationToken cancellationToken)
        {
            var selectedTransport = GetTransport(args);
            var selectedBuffering = GetBuffering(args);
            var selectedProtocol = GetProtocol(args);
            var multiplex = GetMultiplex(args);

            if (selectedTransport == Transport.Http)
            {
                if (multiplex)
                    throw new Exception("This tutorial semple code does not yet allow multiplex over http (although Thrift itself of course does)");
                new HttpServerSample().Run(cancellationToken);
            }
            else
            {
                await RunSelectedConfigurationAsync(selectedTransport, selectedBuffering, selectedProtocol, multiplex, cancellationToken);
            }
        }


        private static bool GetMultiplex(string[] args)
        {
            var mplex = args.FirstOrDefault(x => x.StartsWith("-multiplex"));
            return !string.IsNullOrEmpty(mplex);
        }

        private static Protocol GetProtocol(string[] args)
        {
            var protocol = args.FirstOrDefault(x => x.StartsWith("-pr"))?.Split(':')?[1];
            if (string.IsNullOrEmpty(protocol))
                return Protocol.Binary;

            protocol = protocol.Substring(0, 1).ToUpperInvariant() + protocol.Substring(1).ToLowerInvariant();
            if (Enum.TryParse(protocol, true, out Protocol selectedProtocol))
                return selectedProtocol;
            else
                return Protocol.Binary;
        }

        private static Buffering GetBuffering(string[] args)
        {
            var buffering = args.FirstOrDefault(x => x.StartsWith("-bf"))?.Split(":")?[1];
            if (string.IsNullOrEmpty(buffering))
                return Buffering.None;

            buffering = buffering.Substring(0, 1).ToUpperInvariant() + buffering.Substring(1).ToLowerInvariant();
            if( Enum.TryParse<Buffering>(buffering, out var selectedBuffering))
                return selectedBuffering;
            else
                return Buffering.None;
        }

        private static Transport GetTransport(string[] args)
        {
            var transport = args.FirstOrDefault(x => x.StartsWith("-tr"))?.Split(':')?[1];
            if (string.IsNullOrEmpty(transport))
                return Transport.Tcp;

            transport = transport.Substring(0, 1).ToUpperInvariant() + transport.Substring(1).ToLowerInvariant();
            if( Enum.TryParse(transport, true, out Transport selectedTransport))
                return selectedTransport;
            else
                return Transport.Tcp;
        }

        private static async Task RunSelectedConfigurationAsync(Transport transport, Buffering buffering, Protocol protocol, bool multiplex, CancellationToken cancellationToken)
        {
            TServerTransport serverTransport = transport switch
            {
                Transport.Tcp => new TServerSocketTransport(9090, Configuration),
                Transport.NamedPipe => new TNamedPipeServerTransport(".test", Configuration, NamedPipeClientFlags.None),
                Transport.TcpTls => new TTlsServerSocketTransport(9090, Configuration, GetCertificate(), ClientCertValidator, LocalCertificateSelectionCallback),
                _ => throw new ArgumentException("unsupported value $transport", nameof(transport)),
            };

            TTransportFactory transportFactory = buffering switch
            {
                Buffering.Buffered => new TBufferedTransport.Factory(),
                Buffering.Framed => new TFramedTransport.Factory(),
                // layered transport(s) are optional
                Buffering.None => null,
                _ => throw new ArgumentException("unsupported value $buffering", nameof(buffering)),
            };

            TProtocolFactory protocolFactory = protocol switch
            {
                Protocol.Binary => new TBinaryProtocol.Factory(),
                Protocol.Compact => new TCompactProtocol.Factory(),
                Protocol.Json => new TJsonProtocol.Factory(),
                _ => throw new ArgumentException("unsupported value $protocol", nameof(protocol)),
            };

            var handler = new CalculatorAsyncHandler();
            ITAsyncProcessor processor = new Calculator.AsyncProcessor(handler);

            if (multiplex)
            {
                var multiplexedProcessor = new TMultiplexedProcessor();
                multiplexedProcessor.RegisterProcessor(nameof(Calculator), processor);

                processor = multiplexedProcessor;
            }


            try
            {
                Logger.LogInformation(
                    "TSimpleAsyncServer with \n{transport} transport\n{buffering} buffering\nmultiplex = {multiplex}\n{protocol} protocol",
                    transport,
                    buffering,
                    multiplex ? "yes" : "no",
                    protocol
                    );

                var server = new TSimpleAsyncServer(
                    itProcessorFactory: new TSingletonProcessorFactory(processor),
                    serverTransport: serverTransport,
                    inputTransportFactory: transportFactory,
                    outputTransportFactory: transportFactory,
                    inputProtocolFactory: protocolFactory,
                    outputProtocolFactory: protocolFactory,
                    logger: LoggingHelper.CreateLogger<TSimpleAsyncServer >());

                Logger.LogInformation("Starting the server...");

                await server.ServeAsync(cancellationToken);
            }
            catch (Exception x)
            {
                Logger.LogInformation("{x}",x);
            }
        }

        private static X509Certificate2 GetCertificate()
        {
            // due to files location in net core better to take certs from top folder
            var certFile = GetCertPath(Directory.GetParent(Directory.GetCurrentDirectory()));
            return new X509Certificate2(certFile, "ThriftTest");
        }

        private static string GetCertPath(DirectoryInfo di, int maxCount = 6)
        {
            var topDir = di;
            var certFile =
                topDir.EnumerateFiles("ThriftTest.pfx", SearchOption.AllDirectories)
                    .FirstOrDefault();
            if (certFile == null)
            {
                if (maxCount == 0)
                    throw new FileNotFoundException("Cannot find file in directories");
                return GetCertPath(di.Parent, maxCount - 1);
            }

            return certFile.FullName;
        }

        private static X509Certificate LocalCertificateSelectionCallback(object sender,
            string targetHost, X509CertificateCollection localCertificates,
            X509Certificate remoteCertificate, string[] acceptableIssuers)
        {
            return GetCertificate();
        }

        private static bool ClientCertValidator(object sender, X509Certificate certificate,
            X509Chain chain, SslPolicyErrors sslPolicyErrors)
        {
            return true;
        }

        private enum Transport
        {
            Tcp,
            NamedPipe,
            Http,
            TcpTls,
        }

        private enum Buffering
        {
            None,
            Buffered,
            Framed,
        }

        private enum Protocol
        {
            Binary,
            Compact,
            Json,
        }

        public class HttpServerSample
        {
            public void Run(CancellationToken cancellationToken)
            {
                var config = new ConfigurationBuilder()
                    .AddEnvironmentVariables(prefix: "ASPNETCORE_")
                    .Build();

                var host = new WebHostBuilder()
                    .UseConfiguration(config)
                    .UseKestrel()
                    .UseUrls("http://localhost:9090")
                    .UseContentRoot(Directory.GetCurrentDirectory())
                    .UseStartup<Startup>()
                    .ConfigureLogging((ctx,logging) => LoggingHelper.ConfigureLogging(logging))
                    .Build();

                Logger.LogTrace("test");
                Logger.LogCritical("test");
                host.RunAsync(cancellationToken).GetAwaiter().GetResult();
            }

            public class Startup
            {
                public Startup(IWebHostEnvironment env)
                {
                    var builder = new ConfigurationBuilder()
                        .SetBasePath(env.ContentRootPath)
                        .AddEnvironmentVariables();

                    Configuration = builder.Build();
                }

                public IConfigurationRoot Configuration { get; }

                // This method gets called by the runtime. Use this method to add services to the container.
                public void ConfigureServices(IServiceCollection services)
                {
                    // NOTE: this is not really the recommended way to do it
                    // because the HTTP server cannot be configured properly to e.g. accept framed or multiplex
                    services.AddTransient<Calculator.IAsync, CalculatorAsyncHandler>();
                    services.AddTransient<ITAsyncProcessor, Calculator.AsyncProcessor>();
                    services.AddTransient<THttpServerTransport, THttpServerTransport>();
                }

                // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
                public void Configure(IApplicationBuilder app, IWebHostEnvironment env, ILoggerFactory loggerFactory)
                {
                    _ = env;
                    _ = loggerFactory;
                    app.UseMiddleware<THttpServerTransport>();
                }
            }
        }

        public class CalculatorAsyncHandler : Calculator.IAsync
        {
            private readonly Dictionary<int, SharedStruct> _log = new();

            public CalculatorAsyncHandler()
            {
            }

            public async Task<SharedStruct> getStruct(int key,
                CancellationToken cancellationToken)
            {
                Logger.LogInformation("GetStruct({key})", key);
                return await Task.FromResult(_log[key]);
            }

            public async Task ping(CancellationToken cancellationToken)
            {
                Logger.LogInformation("Ping()");
                await Task.CompletedTask;
            }

            public async Task<int> add(int num1, int num2, CancellationToken cancellationToken)
            {
                Logger.LogInformation("Add({num1},{num2})", num1, num2);
                return await Task.FromResult(num1 + num2);
            }

            public async Task<int> calculate(int logid, Work w, CancellationToken cancellationToken)
            {
                Logger.LogInformation("Calculate({logid}, [{w.Op},{w.Num1},{w.Num2}])", logid, w.Op, w.Num1, w.Num2);

                int val;
                switch (w.Op)
                {
                    case Operation.ADD:
                        val = w.Num1 + w.Num2;
                        break;

                    case Operation.SUBTRACT:
                        val = w.Num1 - w.Num2;
                        break;

                    case Operation.MULTIPLY:
                        val = w.Num1 * w.Num2;
                        break;

                    case Operation.DIVIDE:
                        if (w.Num2 == 0)
                        {
                            var io = new InvalidOperation
                            {
                                WhatOp = (int) w.Op,
                                Why = "Cannot divide by 0"
                            };

                            throw io;
                        }
                        val = w.Num1 / w.Num2;
                        break;

                    default:
                    {
                        var io = new InvalidOperation
                        {
                            WhatOp = (int) w.Op,
                            Why = "Unknown operation"
                        };

                        throw io;
                    }
                }

                var entry = new SharedStruct
                {
                    Key = logid,
                    Value = val.ToString()
                };

                _log[logid] = entry;

                return await Task.FromResult(val);
            }

            public async Task zip(CancellationToken cancellationToken)
            {
                Logger.LogInformation("Zip() with delay 100mc");
                await Task.Delay(100, CancellationToken.None);
            }
        }

        public class SharedServiceAsyncHandler : SharedService.IAsync
        {
            public async Task<SharedStruct> getStruct(int key, CancellationToken cancellationToken)
            {
                Logger.LogInformation("GetStruct({key})", key);
                return await Task.FromResult(new SharedStruct()
                {
                    Key = key,
                    Value = "GetStruct"
                });
            }
        }
    }
}
