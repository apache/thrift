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
using System.Net;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Thrift;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;
using tutorial;
using shared;
using Microsoft.Extensions.DependencyInjection;
using System.Diagnostics;

namespace Client
{
    public class Program
    {
        private static ServiceCollection ServiceCollection = new ServiceCollection();
        private static ILogger Logger;

        private static void DisplayHelp()
        {
            Logger.LogInformation(@"
Usage: 
    Client.exe -help
        will diplay help information 

    Client.exe -tr:<transport> -bf:<buffering> -pr:<protocol> -mc:<numClients>
        will run client with specified arguments (tcp transport and binary protocol by default) and with 1 client

Options:
    -tr (transport): 
        tcp - (default) tcp transport will be used (host - ""localhost"", port - 9090)
        namedpipe - namedpipe transport will be used (pipe address - "".test"")
        http - http transport will be used (address - ""http://localhost:9090"")        
        tcptls - tcp tls transport will be used (host - ""localhost"", port - 9090)

    -bf (buffering): 
        none - (default) no buffering will be used
        buffered - buffered transport will be used
        framed - framed transport will be used

    -pr (protocol): 
        binary - (default) binary protocol will be used
        compact - compact protocol will be used
        json - json protocol will be used
        multiplexed - multiplexed protocol will be used

    -mc (multiple clients):
        <numClients> - number of multiple clients to connect to server (max 100, default 1)

Sample:
    Client.exe -tr:tcp -p:binary
");
        }

        public static void Main(string[] args)
        {
            args = args ?? new string[0];

            ServiceCollection.AddLogging(logging => ConfigureLogging(logging));
            Logger = ServiceCollection.BuildServiceProvider().GetService<ILoggerFactory>().CreateLogger(nameof(Client));

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

        private static void ConfigureLogging(ILoggingBuilder logging)
        {
            logging.SetMinimumLevel(LogLevel.Trace);
            logging.AddConsole();
            logging.AddDebug();
        }

        private static async Task RunAsync(string[] args, CancellationToken cancellationToken)
        {
            var numClients = GetNumberOfClients(args);

            Logger.LogInformation($"Selected # of clients: {numClients}");

            var transports = new TTransport[numClients];
            for (int i = 0; i < numClients; i++)
            {
                var t = GetTransport(args);
                transports[i] = t;
            }
            
            Logger.LogInformation($"Selected client transport: {transports[0]}");

            var protocols = new Tuple<Protocol, TProtocol>[numClients];
            for (int i = 0; i < numClients; i++)
            {
                var p = GetProtocol(args, transports[i]);
                protocols[i] = p;
            }

            Logger.LogInformation($"Selected client protocol: {protocols[0].Item1}");

            var tasks = new Task[numClients];
            for (int i = 0; i < numClients; i++)
            {
                var task = RunClientAsync(protocols[i], cancellationToken);
                tasks[i] = task;
            }

            Task.WaitAll(tasks);

            await Task.CompletedTask;
        }

        private static TTransport GetTransport(string[] args)
        {
            TTransport transport = new TSocketTransport(IPAddress.Loopback, 9090);

            // construct endpoint transport
            var transportArg = args.FirstOrDefault(x => x.StartsWith("-tr"))?.Split(':')?[1];
            if (Enum.TryParse(transportArg, true, out Transport selectedTransport))
            {
                switch (selectedTransport)
                {
                    case Transport.Tcp:
                        transport = new TSocketTransport(IPAddress.Loopback, 9090);
                        break;

                    case Transport.NamedPipe:
                        transport = new TNamedPipeTransport(".test");
                        break;

                    case Transport.Http:
                        transport = new THttpTransport(new Uri("http://localhost:9090"), null);
                        break;

                    case Transport.TcpTls:
                        transport = new TTlsSocketTransport(IPAddress.Loopback, 9090, GetCertificate(), CertValidator, LocalCertificateSelectionCallback);
                        break;

                    default:
                        Debug.Assert(false, "unhandled case");
                        break;
                }
            }

            // optionally add layered transport(s)
            var bufferingArg = args.FirstOrDefault(x => x.StartsWith("-bf"))?.Split(':')?[1];
            if (Enum.TryParse<Buffering>(bufferingArg, out var selectedBuffering))
            {
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
            }

            return transport;
        }

        private static int GetNumberOfClients(string[] args)
        {
            var numClients = args.FirstOrDefault(x => x.StartsWith("-mc"))?.Split(':')?[1];

            Logger.LogInformation($"Selected # of clients: {numClients}");

            int c;
            if( int.TryParse(numClients, out c) && (0 < c) && (c <= 100))
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

        private static Tuple<Protocol, TProtocol> GetProtocol(string[] args, TTransport transport)
        {
            var protocol = args.FirstOrDefault(x => x.StartsWith("-pr"))?.Split(':')?[1];

            Protocol selectedProtocol;
            if (Enum.TryParse(protocol, true, out selectedProtocol))
            {
                switch (selectedProtocol)
                {
                    case Protocol.Binary:
                        return new Tuple<Protocol, TProtocol>(selectedProtocol, new TBinaryProtocol(transport));
                    case Protocol.Compact:
                        return new Tuple<Protocol, TProtocol>(selectedProtocol, new TCompactProtocol(transport));
                    case Protocol.Json:
                        return new Tuple<Protocol, TProtocol>(selectedProtocol, new TJsonProtocol(transport));
                    case Protocol.Multiplexed:
                        // it returns BinaryProtocol to avoid making wrapped protocol as public in TProtocolDecorator (in RunClientAsync it will be wrapped into Multiplexed protocol)
                        return new Tuple<Protocol, TProtocol>(selectedProtocol, new TBinaryProtocol(transport));
                    default:
                        Debug.Assert(false, "unhandled case");
                        break;
                }
            }

            return new Tuple<Protocol, TProtocol>(selectedProtocol, new TBinaryProtocol(transport));
        }

        private static async Task RunClientAsync(Tuple<Protocol, TProtocol> protocolTuple, CancellationToken cancellationToken)
        {
            try
            {
                var protocol = protocolTuple.Item2;
                var protocolType = protocolTuple.Item1;

                TBaseClient client = null;

                try
                {
                    if (protocolType != Protocol.Multiplexed)
                    {

                        client = new Calculator.Client(protocol);
                        await ExecuteCalculatorClientOperations(cancellationToken, (Calculator.Client)client);
                    }
                    else
                    {
                        // it uses binary protocol there  to create Multiplexed protocols
                        var multiplex = new TMultiplexedProtocol(protocol, nameof(Calculator));
                        client = new Calculator.Client(multiplex);
                        await ExecuteCalculatorClientOperations(cancellationToken, (Calculator.Client)client);

                        multiplex = new TMultiplexedProtocol(protocol, nameof(SharedService));
                        client = new SharedService.Client(multiplex);
                        await ExecuteSharedServiceClientOperations(cancellationToken, (SharedService.Client)client);
                    }
                }
                catch (Exception ex)
                {
                    Logger.LogError($"{client?.ClientId} " + ex);
                }
                finally
                {
                    protocol.Transport.Close();
                }
            }
            catch (TApplicationException x)
            {
                Logger.LogError(x.ToString());
            }
        }

        private static async Task ExecuteCalculatorClientOperations(CancellationToken cancellationToken, Calculator.Client client)
        {
            await client.OpenTransportAsync(cancellationToken);

            // Async version

            Logger.LogInformation($"{client.ClientId} PingAsync()");
            await client.pingAsync(cancellationToken);

            Logger.LogInformation($"{client.ClientId} AddAsync(1,1)");
            var sum = await client.addAsync(1, 1, cancellationToken);
            Logger.LogInformation($"{client.ClientId} AddAsync(1,1)={sum}");

            var work = new Work
            {
                Op = Operation.DIVIDE,
                Num1 = 1,
                Num2 = 0
            };

            try
            {
                Logger.LogInformation($"{client.ClientId} CalculateAsync(1)");
                await client.calculateAsync(1, work, cancellationToken);
                Logger.LogInformation($"{client.ClientId} Whoa we can divide by 0");
            }
            catch (InvalidOperation io)
            {
                Logger.LogInformation($"{client.ClientId} Invalid operation: " + io);
            }

            work.Op = Operation.SUBTRACT;
            work.Num1 = 15;
            work.Num2 = 10;

            try
            {
                Logger.LogInformation($"{client.ClientId} CalculateAsync(1)");
                var diff = await client.calculateAsync(1, work, cancellationToken);
                Logger.LogInformation($"{client.ClientId} 15-10={diff}");
            }
            catch (InvalidOperation io)
            {
                Logger.LogInformation($"{client.ClientId} Invalid operation: " + io);
            }

            Logger.LogInformation($"{client.ClientId} GetStructAsync(1)");
            var log = await client.getStructAsync(1, cancellationToken);
            Logger.LogInformation($"{client.ClientId} Check log: {log.Value}");

            Logger.LogInformation($"{client.ClientId} ZipAsync() with delay 100mc on server side");
            await client.zipAsync(cancellationToken);
        }
        private static async Task ExecuteSharedServiceClientOperations(CancellationToken cancellationToken, SharedService.Client client)
        {
            await client.OpenTransportAsync(cancellationToken);

            // Async version

            Logger.LogInformation($"{client.ClientId} SharedService GetStructAsync(1)");
            var log = await client.getStructAsync(1, cancellationToken);
            Logger.LogInformation($"{client.ClientId} SharedService Value: {log.Value}");
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
            Multiplexed
        }

        private enum Buffering
        {
            None,
            Buffered,
            Framed
        }
    }
}
