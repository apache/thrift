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
using Thrift.Protocols;
using Thrift.Transports;
using Thrift.Transports.Client;
using tutorial;
using shared;

namespace Client
{
    public class Program
    {
        private static readonly ILogger Logger = new LoggerFactory().CreateLogger(nameof(Client));

        private static void DisplayHelp()
        {
            Console.WriteLine(@"
Usage: 
    Client.exe -h
        will diplay help information 

    Client.exe -t:<transport> -p:<protocol>
        will run client with specified arguments (tcp transport and binary protocol by default)

Options:
    -t (transport): 
        tcp - (default) tcp transport will be used (host - ""localhost"", port - 9090)
        tcpbuffered - buffered transport over tcp will be used (host - ""localhost"", port - 9090)
        namedpipe - namedpipe transport will be used (pipe address - "".test"")
        http - http transport will be used (address - ""http://localhost:9090"")        
        tcptls - tcp tls transport will be used (host - ""localhost"", port - 9090)

    -p (protocol): 
        binary - (default) binary protocol will be used
        compact - compact protocol will be used
        json - json protocol will be used

Sample:
    Client.exe -t:tcp -p:binary
");
        }

        public static void Main(string[] args)
        {
            args = args ?? new string[0];

            if (args.Any(x => x.StartsWith("-h", StringComparison.OrdinalIgnoreCase)))
            {
                DisplayHelp();
                return;
            }


            using (var source = new CancellationTokenSource())
            {
                RunAsync(args, source.Token).GetAwaiter().GetResult();
            }
        }

        private static async Task RunAsync(string[] args, CancellationToken cancellationToken)
        {
            var clientTransport = GetTransport(args);

            Logger.LogInformation($"Selected client transport: {clientTransport}");

            var clientProtocol = GetProtocol(args, clientTransport);

            Logger.LogInformation($"Selected client protocol: {clientProtocol}");

            await RunClientAsync(clientProtocol, cancellationToken);
        }

        private static TClientTransport GetTransport(string[] args)
        {
            var transport = args.FirstOrDefault(x => x.StartsWith("-t"))?.Split(':')?[1];

            Transport selectedTransport;
            if (Enum.TryParse(transport, true, out selectedTransport))
            {
                switch (selectedTransport)
                {
                    case Transport.Tcp:
                        return new TSocketClientTransport(IPAddress.Loopback, 9090);
                    case Transport.NamedPipe:
                        return new TNamedPipeClientTransport(".test");
                    case Transport.Http:
                        return new THttpClientTransport(new Uri("http://localhost:9090"), null);
                    case Transport.TcpBuffered:
                        return
                            new TBufferedClientTransport(
                                new TSocketClientTransport(IPAddress.Loopback, 9090));
                    case Transport.TcpTls:
                        return new TTlsSocketClientTransport(IPAddress.Loopback, 9090,
                            GetCertificate(), CertValidator, LocalCertificateSelectionCallback);
                    case Transport.Framed:
                        throw new NotSupportedException("Framed is not ready for samples");
                }
            }

            return new TSocketClientTransport(IPAddress.Loopback, 9090);
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

        private static TProtocol GetProtocol(string[] args, TClientTransport transport)
        {
            var protocol = args.FirstOrDefault(x => x.StartsWith("-p"))?.Split(':')?[1];

            Protocol selectedProtocol;
            if (Enum.TryParse(protocol, true, out selectedProtocol))
            {
                switch (selectedProtocol)
                {
                    case Protocol.Binary:
                        return new TBinaryProtocol(transport);
                    case Protocol.Compact:
                        return new TCompactProtocol(transport);
                    case Protocol.Json:
                        return new TJsonProtocol(transport);
                }
            }

            return new TBinaryProtocol(transport);
        }

        private static async Task RunClientAsync(TProtocol protocol,
            CancellationToken cancellationToken)
        {
            try
            {
                var client = new Calculator.Client(protocol);
                await client.OpenTransportAsync(cancellationToken);

                try
                {
                    // Async version

                    Logger.LogInformation("PingAsync()");
                    await client.pingAsync(cancellationToken);

                    Logger.LogInformation("AddAsync(1,1)");
                    var sum = await client.addAsync(1, 1, cancellationToken);
                    Logger.LogInformation($"AddAsync(1,1)={sum}");

                    var work = new Work
                    {
                        Op = Operation.DIVIDE,
                        Num1 = 1,
                        Num2 = 0
                    };

                    try
                    {
                        Logger.LogInformation("CalculateAsync(1)");
                        await client.calculateAsync(1, work, cancellationToken);
                        Logger.LogInformation("Whoa we can divide by 0");
                    }
                    catch (InvalidOperation io)
                    {
                        Logger.LogInformation("Invalid operation: " + io);
                    }

                    work.Op = Operation.SUBTRACT;
                    work.Num1 = 15;
                    work.Num2 = 10;

                    try
                    {
                        Logger.LogInformation("CalculateAsync(1)");
                        var diff = await client.calculateAsync(1, work, cancellationToken);
                        Logger.LogInformation($"15-10={diff}");
                    }
                    catch (InvalidOperation io)
                    {
                        Logger.LogInformation("Invalid operation: " + io);
                    }

                    Logger.LogInformation("GetStructAsync(1)");
                    var log = await client.getStructAsync(1, cancellationToken);
                    Logger.LogInformation($"Check log: {log.Value}");

                    Logger.LogInformation("ZipAsync() with delay 100mc on server side");
                    await client.zipAsync(cancellationToken);
                }
                catch (Exception ex)
                {
                    Logger.LogError(ex.ToString());
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
    }
}