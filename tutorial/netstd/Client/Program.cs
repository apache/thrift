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
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;
using Thrift;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;
using tutorial;
using shared;

#pragma warning disable IDE0063  // using
#pragma warning disable IDE0057  // substr

namespace Client
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

        private static void DisplayHelp()
        {
            Logger.LogInformation(@"
Usage: 
    Client -help
        will diplay help information 

    Client -tr:<transport> -bf:<buffering> -pr:<protocol> [-mc:<numClients>]  [-multiplex]
        will run client with specified arguments (tcp transport and binary protocol by default) and with 1 client

Options:
    -tr (transport): 
        tcp - (default) tcp transport  (localhost:9090)
        tcptls - tcp tls transport  (localhost:9090)
        namedpipe - namedpipe transport  (pipe "".test"")
        http - http transport  (http://localhost:9090)

    -bf (buffering): 
        none - (default) no buffering 
        buffered - buffered transport 
        framed - framed transport 

    -pr (protocol): 
        binary - (default) binary protocol 
        compact - compact protocol 
        json - json protocol 

    -multiplex - adds multiplexed protocol

    -mc (multiple clients):
        <numClients> - number of multiple clients to connect to server (max 100, default 1)

Sample:
    Client -tr:tcp -pr:binary
");
        }

        public static void Main(string[] args)
        {
            args ??= Array.Empty<string>();

            if (args.Any(x => x.StartsWith("-help", StringComparison.OrdinalIgnoreCase)))
            {
                DisplayHelp();
                return;
            }

            Logger.LogInformation("Starting client...");

            using (var source = new CancellationTokenSource())
            {
                RunAsync(args, source.Token).GetAwaiter().GetResult();
            }
        }

        
        private static async Task RunAsync(string[] args, CancellationToken cancellationToken)
        {
            var numClients = GetNumberOfClients(args);

            Logger.LogInformation("Selected # of clients: {numClients}", numClients);

            var transport = GetTransport(args);
            Logger.LogInformation("Selected client transport: {transport}", transport);

            var protocol = MakeProtocol( args, MakeTransport(args));
            Logger.LogInformation("Selected client protocol: {GetProtocol(args)}", GetProtocol(args));

            var mplex = GetMultiplex(args);
            Logger.LogInformation("Multiplex {mplex}", mplex);

            var tasks = new Task[numClients];
            for (int i = 0; i < numClients; i++)
            {
                var task = RunClientAsync(protocol, mplex, cancellationToken);
                tasks[i] = task;
            }

            Task.WaitAll(tasks,cancellationToken);
            await Task.CompletedTask;
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
            if (Enum.TryParse<Buffering>(buffering, out var selectedBuffering))
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
            if (Enum.TryParse(transport, true, out Transport selectedTransport))
                return selectedTransport;
            else
                return Transport.Tcp;
        }


        private static TTransport MakeTransport(string[] args)
        {
            // construct endpoint transport
            TTransport transport = null;
            Transport selectedTransport = GetTransport(args);
            {
                switch (selectedTransport)
                {
                    case Transport.Tcp:
                        transport = new TSocketTransport(IPAddress.Loopback, 9090, Configuration);
                        break;

                    case Transport.NamedPipe:
                        transport = new TNamedPipeTransport(".test", Configuration);
                        break;

                    case Transport.Http:
                        transport = new THttpTransport(new Uri("http://localhost:9090"), Configuration);
                        break;

                    case Transport.TcpTls:
                        transport = new TTlsSocketTransport(IPAddress.Loopback, 9090, Configuration,
                            GetCertificate(), CertValidator, LocalCertificateSelectionCallback);
                        break;

                    default:
                        Debug.Assert(false, "unhandled case");
                        break;
                }
            }

            // optionally add layered transport(s)
            Buffering selectedBuffering = GetBuffering(args);
            switch (selectedBuffering)
            {
                case Buffering.Buffered:
                    transport = new TBufferedTransport(transport);
                    break;

                case Buffering.Framed:
                    transport = new TFramedTransport(transport);
                    break;

                default: // layered transport(s) are optional
                    Debug.Assert(selectedBuffering == Buffering.None, "unhandled case");
                    break;
            }

            return transport;
        }

        private static int GetNumberOfClients(string[] args)
        {
            var numClients = args.FirstOrDefault(x => x.StartsWith("-mc"))?.Split(':')?[1];

            Logger.LogInformation("Selected # of clients: {numClients}", numClients);

            if (int.TryParse(numClients, out int c) && (0 < c) && (c <= 100))
                return c;
            else
                return 1;
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

        private static bool CertValidator(object sender, X509Certificate certificate,
            X509Chain chain, SslPolicyErrors sslPolicyErrors)
        {
            return true;
        }

        private static TProtocol MakeProtocol(string[] args, TTransport transport)
        {
            Protocol selectedProtocol = GetProtocol(args);
            return selectedProtocol switch
            {
                Protocol.Binary => new TBinaryProtocol(transport),
                Protocol.Compact => new TCompactProtocol(transport),
                Protocol.Json => new TJsonProtocol(transport),
                _ => throw new Exception("unhandled protocol"),
            };
        }

        private static async Task RunClientAsync(TProtocol protocol, bool multiplex, CancellationToken cancellationToken)
        {
            try
            {
                try
                {
                    if( multiplex)
                        protocol = new TMultiplexedProtocol(protocol, nameof(Calculator));

                    var client = new Calculator.Client(protocol);
                    await ExecuteCalculatorClientOperations(client, cancellationToken);
                }
                catch (Exception ex)
                {
                    Logger.LogError("{ex}",ex);
                }
                finally
                {
                    protocol.Transport.Close();
                }
            }
            catch (TApplicationException x)
            {
                Logger.LogError("{x}",x);
            }
        }

        private static async Task ExecuteCalculatorClientOperations( Calculator.Client client, CancellationToken cancellationToken)
        {
            await client.OpenTransportAsync(cancellationToken);

            // Async version

            Logger.LogInformation("{client.ClientId} Ping()", client.ClientId);
            await client.ping(cancellationToken);

            Logger.LogInformation("{client.ClientId} Add(1,1)", client.ClientId);
            var sum = await client.add(1, 1, cancellationToken);
            Logger.LogInformation("{client.ClientId} Add(1,1)={sum}", client.ClientId, sum);

            var work = new Work
            {
                Op = Operation.DIVIDE,
                Num1 = 1,
                Num2 = 0
            };

            try
            {
                Logger.LogInformation("{client.ClientId} Calculate(1)", client.ClientId);
                await client.calculate(1, work, cancellationToken);
                Logger.LogInformation("{client.ClientId} Whoa we can divide by 0", client.ClientId);
            }
            catch (InvalidOperation io)
            {
                Logger.LogInformation("{client.ClientId} Invalid operation: {io}", client.ClientId, io);
            }

            work.Op = Operation.SUBTRACT;
            work.Num1 = 15;
            work.Num2 = 10;

            try
            {
                Logger.LogInformation("{client.ClientId} Calculate(1)", client.ClientId);
                var diff = await client.calculate(1, work, cancellationToken);
                Logger.LogInformation("{client.ClientId} 15-10={diff}", client.ClientId, diff);
            }
            catch (InvalidOperation io)
            {
                Logger.LogInformation("{client.ClientId} Invalid operation: {io}", client.ClientId, io);
            }

            Logger.LogInformation("{client.ClientId} GetStruct(1)", client.ClientId);
            var log = await client.getStruct(1, cancellationToken);
            Logger.LogInformation("{client.ClientId} Check log: {log.Value}", client.ClientId, log.Value);

            Logger.LogInformation("{client.ClientId} Zip() with delay 100mc on server side", client.ClientId);
            await client.zip(cancellationToken);
        }


        private enum Transport
        {
            Tcp,
            NamedPipe,
            Http,
            TcpBuffered,
            Framed,
            TcpTls
        }

        private enum Protocol
        {
            Binary,
            Compact,
            Json,
        }

        private enum Buffering
        {
            None,
            Buffered,
            Framed
        }
    }
}
