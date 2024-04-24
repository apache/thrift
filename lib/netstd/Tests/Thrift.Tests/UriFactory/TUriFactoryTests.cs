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
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Transport;

namespace Thrift.Tests.UriFactory
{
    [TestClass]
    public class TUriFactoryTests
    {
        private enum BuiltinProtocols { binary, compact, json };
        private enum BuiltinTransports { http, namedpipes, socket, tlssocket, file, memory };
        private enum BuiltinProtocolDecorators { none, mplex };
        private enum BuiltinLayeredTransports { none, framed, buffered };

        private readonly string InputFile = Path.GetTempFileName();
        private readonly string OutputFile = Path.GetTempFileName();

        private static HashSet<BuiltinTransports> GetAllSocketTransports()
        {
            // these are known to slow down the test
            return [
                BuiltinTransports.socket,
                BuiltinTransports.tlssocket,
            ];
        }

        [TestMethod]
        public void TFactory_Can_Parse_And_Construct_Complex_ThriftUris()
        {
            var exclude = GetAllSocketTransports();
            var transports = new HashSet<BuiltinTransports>();
            foreach (var trans in Enum.GetValues<BuiltinTransports>())
                if (!exclude.Contains(trans))
                    transports.Add(trans);

            InternalTestMethodImplementation(
                Enum.GetValues<BuiltinProtocols>(),
                transports,
                Enum.GetValues<BuiltinProtocolDecorators>(),
                Enum.GetValues<BuiltinLayeredTransports>()
                );
        }

        [TestMethod]
        public void TFactory_Can_Parse_And_Construct_Socket_Transports()
        {
            var transports = GetAllSocketTransports();

            // minimize iterations, as these are known to slow down the test
            InternalTestMethodImplementation(
                [BuiltinProtocols.binary],
                transports,
                [BuiltinProtocolDecorators.none],
                [BuiltinLayeredTransports.none]
                );
        }

        private void InternalTestMethodImplementation(IEnumerable<BuiltinProtocols> protocols, IEnumerable<BuiltinTransports> transports, IEnumerable<BuiltinProtocolDecorators> decorators, IEnumerable<BuiltinLayeredTransports> layeredTransports)
        {
            // those must not be empty to have at least one test run
            Assert.IsTrue(protocols.Any());
            Assert.IsTrue(transports.Any());

            // "none" should always be included and should be first
            Assert.IsTrue(decorators.FirstOrDefault() == BuiltinProtocolDecorators.none);
            Assert.IsTrue(layeredTransports.FirstOrDefault() == BuiltinLayeredTransports.none);

            // thrift://protocol/transport/layer/layer?data
            var numTests = 0;
            foreach (var proto in protocols)
            {
                foreach (var trans in transports)
                {
                    var iTest = 0;
                    while (InitializeTransportSpecificArgs(trans, iTest++, out var connection))
                    {
                        // test basic combination first
                        var sData = MakeQueryString(connection);
                        var sUri = TThriftUri.THRIFT_URI_SCHEME + proto + "/" + trans;

                        // decorators can be stacked upon each other, so lets do exactly that - just to test it
                        var sDecorator = string.Empty;
                        foreach (var decorator in decorators)
                        {
                            sDecorator += MakeDecoratorString(decorator);

                            // layers can be stacked upon each other, so lets do exactly that - just to test it
                            var sLayer = string.Empty;
                            foreach (var layer in layeredTransports)
                            {
                                sLayer += MakeLayeredTransportString(layer);

                                // build final URI
                                TestUri(sUri + sDecorator + sLayer +sData);
                                numTests++;
                            }
                        }
                    }
                }
            }

            // we had at least one test?
            Assert.IsTrue(numTests > 0);

            File.Delete(InputFile);
            File.Delete(OutputFile);
        }

        private static string MakeLayeredTransportString(BuiltinLayeredTransports layer)
        {
            return layer switch
            {
                BuiltinLayeredTransports.none => string.Empty,
                _ => "/" + layer.ToString()
            };
        }

        private static string MakeDecoratorString(BuiltinProtocolDecorators protdeco)
        {
            return protdeco switch
            {
                BuiltinProtocolDecorators.none => string.Empty,
                BuiltinProtocolDecorators.mplex => "/" + protdeco.ToString() + ":" + Uri.EscapeDataString("Rather\0Obscure\tMplex\nService/Name"),
                _ => "/" + protdeco.ToString(),
            };
        }

        private bool InitializeTransportSpecificArgs(BuiltinTransports trans, int test, out Dictionary<string, string> connection)
        {
            if (test > 64)  // prevent against endless loops
                throw new Exception("Internal test error");

            connection = [];
            switch (trans)
            {
                case BuiltinTransports.http:
                    connection.Add("https://user:pass@example.com/myservice?arg=one&arg=two", string.Empty);
                    return (test == 0);

                case BuiltinTransports.namedpipes:
                    switch (test)
                    {
                        case 0:  // full pipe name
                            connection.Add(@"\\myserver\pipe\mypath\myname", string.Empty);
                            return true;
                        case 1: // pipe name w/o server part
                            connection.Add(@"mypath\myname", string.Empty);
                            return true;
                        case 2: // simple pipe name w/o server part
                            connection.Add(@"mypipe", string.Empty);
                            return true;
                        default:
                            return false;
                    };

                case BuiltinTransports.file:
                    switch (test)
                    {
                        case 0:  // no argument at all
                            return true;
                        case 1: // file stream
                            connection.Add("infile", InputFile);
                            return true;
                        case 2: // file stream
                            connection.Add("outfile", OutputFile);
                            return true;
                        default:
                            return false;
                    };

                case BuiltinTransports.socket:
                    connection.Add("host", "localhost");
                    connection.Add("port", 8080.ToString());
                    return (test == 0);

                case BuiltinTransports.tlssocket:
                    connection.Add("host", "localhost");
                    connection.Add("port", 8080.ToString());
                    connection.Add("cert", Path.Combine(Path.GetTempPath(), "client.p12"));
                    return (test == 0);

                case BuiltinTransports.memory:
                    // none
                    return (test == 0);

                default:
                    throw new NotImplementedException(trans.ToString());
            };
        }

        private static string MakeQueryString(Dictionary<string, string> data)
        {
            if ((data == null) || (data.Count == 0))
                return string.Empty;

            var kvpair = new List<string>();
            foreach (var pair in data)
            {
                var sTmp = Uri.EscapeDataString(pair.Key);
                if (!string.IsNullOrEmpty(pair.Value))
                    sTmp += "=" + Uri.EscapeDataString(pair.Value);
                kvpair.Add(sTmp);
            }
            return "?" + string.Join("&", kvpair);
        }

        private static void TestUri(string sUri)
        {
            var parsed = new TThriftUri(sUri);
            Assert.AreEqual(sUri, parsed.ToString());

            try
            {
                var proto = TFactory.ConstructClientProtocolTransportStack(parsed, new(), out var trans);
                try
                {
                    Assert.IsNotNull(proto);
                    Assert.IsNotNull(trans);
                }
                finally
                {
                    trans?.Dispose();
                    proto?.Dispose();
                }
            }
            catch (System.Security.Cryptography.CryptographicException)
            {
                // that may happen, but is not relevant here
            }
            catch (TTransportException)
            {
                // that may happen, but is not relevant here
            }
        }
    }
}
