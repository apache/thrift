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
using System.Reflection;
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;
using System.ServiceModel;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Thrift;
using Thrift.Collections;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace ThriftTest
{
    internal enum ProtocolChoice
    {
        Binary,
        Compact,
        Json
    }

    // it does not make much sense to use buffered when we already use framed
    internal enum LayeredChoice
    {
        None,
        Buffered,
        Framed
    }

    internal enum TransportChoice
    {
        Socket,
        TlsSocket,
        Http,
        NamedPipe
    }

    public class TestClient
    {
        private class TestParams
        {
            public int numIterations = 1;
            public string host = "localhost";
            public int port = 9090;
            public int numThreads = 1;
            public string url;
            public string pipe;
            public LayeredChoice layered = LayeredChoice.None;
            public ProtocolChoice protocol = ProtocolChoice.Binary;
            public TransportChoice transport = TransportChoice.Socket;
            private readonly TConfiguration Configuration = null;  // or new TConfiguration() if needed

            internal void Parse(List<string> args)
            {
                for (var i = 0; i < args.Count; ++i)
                {
                    if (args[i] == "-u")
                    {
                        url = args[++i];
                        transport = TransportChoice.Http;
                    }
                    else if (args[i] == "-n")
                    {
                        numIterations = Convert.ToInt32(args[++i]);
                    }
                    else if (args[i].StartsWith("--pipe="))
                    {
                        pipe = args[i].Substring(args[i].IndexOf("=") + 1);
                        transport = TransportChoice.NamedPipe;
                    }
                    else if (args[i].StartsWith("--host="))
                    {
                        // check there for ipaddress
                        host = args[i].Substring(args[i].IndexOf("=") + 1);
                        if (transport != TransportChoice.TlsSocket)
                            transport = TransportChoice.Socket;
                    }
                    else if (args[i].StartsWith("--port="))
                    {
                        port = int.Parse(args[i].Substring(args[i].IndexOf("=") + 1));
                        if (transport != TransportChoice.TlsSocket)
                            transport = TransportChoice.Socket;
                    }
                    else if (args[i] == "-b" || args[i] == "--buffered" || args[i] == "--transport=buffered")
                    {
                        layered = LayeredChoice.Buffered;
                    }
                    else if (args[i] == "-f" || args[i] == "--framed" || args[i] == "--transport=framed")
                    {
                        layered = LayeredChoice.Framed;
                    }
                    else if (args[i] == "-t")
                    {
                        numThreads = Convert.ToInt32(args[++i]);
                    }
                    else if (args[i] == "--binary" || args[i] == "--protocol=binary")
                    {
                        protocol = ProtocolChoice.Binary;
                    }
                    else if (args[i] == "--compact" || args[i] == "--protocol=compact")
                    {
                        protocol = ProtocolChoice.Compact;
                    }
                    else if (args[i] == "--json" || args[i] == "--protocol=json")
                    {
                        protocol = ProtocolChoice.Json;
                    }
                    else if (args[i] == "--ssl")
                    {
                        transport = TransportChoice.TlsSocket;
                    }
                    else if (args[i] == "--help")
                    {
                        PrintOptionsHelp();
                        return;
                    }
                    else
                    {
                        Console.WriteLine("Invalid argument: {0}", args[i]);
                        PrintOptionsHelp();
                        return;
                    }
                }

                switch (transport)
                {
                    case TransportChoice.Socket:
                        Console.WriteLine("Using socket transport");
                        break;
                    case TransportChoice.TlsSocket:
                        Console.WriteLine("Using encrypted transport");
                        break;
                    case TransportChoice.Http:
                        Console.WriteLine("Using HTTP transport");
                        break;
                    case TransportChoice.NamedPipe:
                        Console.WriteLine("Using named pipes transport");
                        break;
                    default:  // unhandled case
                        Debug.Assert(false);
                        break;
                }

                switch (layered)
                {
                    case LayeredChoice.Framed:
                        Console.WriteLine("Using framed transport");
                        break;
                    case LayeredChoice.Buffered:
                        Console.WriteLine("Using buffered transport");
                        break;
                    default:  // unhandled case?
                        Debug.Assert(layered == LayeredChoice.None);
                        break;
                }

                switch (protocol)
                {
                    case ProtocolChoice.Binary:
                        Console.WriteLine("Using binary protocol");
                        break;
                    case ProtocolChoice.Compact:
                        Console.WriteLine("Using compact protocol");
                        break;
                    case ProtocolChoice.Json:
                        Console.WriteLine("Using JSON protocol");
                        break;
                    default:  // unhandled case?
                        Debug.Assert(false);
                        break;
                }
            }

            private static X509Certificate2 GetClientCert()
            {
                var clientCertName = "client.p12";
                var possiblePaths = new List<string>
                {
                    "../../../keys/",
                    "../../keys/",
                    "../keys/",
                    "keys/",
                };

                string existingPath = null;
                foreach (var possiblePath in possiblePaths)
                {
                    var path = Path.GetFullPath(possiblePath + clientCertName);
                    if (File.Exists(path))
                    {
                        existingPath = path;
                        break;
                    }
                }

                if (string.IsNullOrEmpty(existingPath))
                {
                    throw new FileNotFoundException($"Cannot find file: {clientCertName}");
                }

                var cert = new X509Certificate2(existingPath, "thrift");

                return cert;
            }

            public TTransport CreateTransport()
            {
                // endpoint transport
                TTransport trans = null;

                switch (transport)
                {
                    case TransportChoice.Http:
                        Debug.Assert(url != null);
                        trans = new THttpTransport(new Uri(url), Configuration);
                        break;

                    case TransportChoice.NamedPipe:
                        Debug.Assert(pipe != null);
                        trans = new TNamedPipeTransport(pipe,Configuration);
                        break;

                    case TransportChoice.TlsSocket:
                        var cert = GetClientCert();
                        if (cert == null || !cert.HasPrivateKey)
                        {
                            throw new InvalidOperationException("Certificate doesn't contain private key");
                        }

                        trans = new TTlsSocketTransport(host, port, Configuration, 0,
                            cert,
                            (sender, certificate, chain, errors) => true,
                            null, SslProtocols.Tls | SslProtocols.Tls11 | SslProtocols.Tls12);
                        break;

                    case TransportChoice.Socket:
                    default:
                        trans = new TSocketTransport(host, port, Configuration);
                        break;
                }


                // layered transport
                switch (layered)
                {
                    case LayeredChoice.Buffered:
                        trans = new TBufferedTransport(trans);
                        break;
                    case LayeredChoice.Framed:
                        trans = new TFramedTransport(trans);
                        break;
                    default:
                        Debug.Assert(layered == LayeredChoice.None);
                        break;
                }

                return trans;
            }

            public TProtocol CreateProtocol(TTransport transport)
            {
                switch (protocol)
                {
                    case ProtocolChoice.Compact:
                        return new TCompactProtocol(transport);
                    case ProtocolChoice.Json:
                        return new TJsonProtocol(transport);
                    case ProtocolChoice.Binary:
                    default:
                        return new TBinaryProtocol(transport);
                }
            }
        }


        private const int ErrorBaseTypes = 1;
        private const int ErrorStructs = 2;
        private const int ErrorContainers = 4;
        private const int ErrorExceptions = 8;
        private const int ErrorUnknown = 64;

        private class ClientTest
        {
            private readonly TTransport transport;
            private readonly ThriftTest.Client client;
            private readonly int numIterations;
            private bool done;

            public int ReturnCode { get; set; }

            public ClientTest(TestParams param)
            {
                transport = param.CreateTransport();
                client = new ThriftTest.Client(param.CreateProtocol(transport));
                numIterations = param.numIterations;
            }

            public void Execute()
            {
                if (done)
                {
                    Console.WriteLine("Execute called more than once");
                    throw new InvalidOperationException();
                }

                for (var i = 0; i < numIterations; i++)
                {
                    try
                    {
                        if (!transport.IsOpen)
                            transport.OpenAsync(MakeTimeoutToken()).GetAwaiter().GetResult();
                    }
                    catch (TTransportException ex)
                    {
                        Console.WriteLine("*** FAILED ***");
                        Console.WriteLine("Connect failed: " + ex.Message);
                        ReturnCode |= ErrorUnknown;
                        Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
                        continue;
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine("*** FAILED ***");
                        Console.WriteLine("Connect failed: " + ex.Message);
                        ReturnCode |= ErrorUnknown;
                        Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
                        continue;
                    }

                    try
                    {
                        ReturnCode |= ExecuteClientTestAsync(client).GetAwaiter().GetResult(); ;
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine("*** FAILED ***");
                        Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
                        ReturnCode |= ErrorUnknown;
                    }
                }
                try
                {
                    transport.Close();
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error while closing transport");
                    Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
                }
                done = true;
            }
        }

        internal static void PrintOptionsHelp()
        {
            Console.WriteLine("Client options:");
            Console.WriteLine("  -u <URL>");
            Console.WriteLine("  -t <# of threads to run>        default = 1");
            Console.WriteLine("  -n <# of iterations>            per thread");
            Console.WriteLine("  --pipe=<pipe name>");
            Console.WriteLine("  --host=<IP address>");
            Console.WriteLine("  --port=<port number>");
            Console.WriteLine("  --transport=<transport name>    one of buffered,framed  (defaults to none)");
            Console.WriteLine("  --protocol=<protocol name>      one of compact,json  (defaults to binary)");
            Console.WriteLine("  --ssl");
            Console.WriteLine();
        }

        public static int Execute(List<string> args)
        {
            try
            {
                var param = new TestParams();

                try
                {
                    param.Parse(args);
                }
                catch (Exception ex)
                {
                    Console.WriteLine("*** FAILED ***");
                    Console.WriteLine("Error while parsing arguments");
                    Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
                    return ErrorUnknown;
                }

                var tests = Enumerable.Range(0, param.numThreads).Select(_ => new ClientTest(param)).ToArray();

                //issue tests on separate threads simultaneously
                var threads = tests.Select(test => new Task(test.Execute)).ToArray();
                var start = DateTime.Now;
                foreach (var t in threads)
                {
                    t.Start();
                }

                Task.WaitAll(threads);

                Console.WriteLine("Total time: " + (DateTime.Now - start));
                Console.WriteLine();
                return tests.Select(t => t.ReturnCode).Aggregate((r1, r2) => r1 | r2);
            }
            catch (Exception outerEx)
            {
                Console.WriteLine("*** FAILED ***");
                Console.WriteLine("Unexpected error");
                Console.WriteLine(outerEx.Message + "\n" + outerEx.StackTrace);
                return ErrorUnknown;
            }
        }

        public static string BytesToHex(byte[] data)
        {
            return BitConverter.ToString(data).Replace("-", string.Empty);
        }


        public enum BinaryTestSize
        {
            Empty,           // Edge case: the zero-length empty binary
            Normal,          // Fairly small array of usual size (256 bytes)
            Large,           // Large writes/reads may cause range check errors
            PipeWriteLimit,  // Windows Limit: Pipe write operations across a network are limited to 65,535 bytes per write.
            FifteenMB        // that's quite a bit of data
        };

        public static byte[] PrepareTestData(bool randomDist, BinaryTestSize testcase)
        {
            int amount;
            switch (testcase)
            {
                case BinaryTestSize.Empty:
                    amount = 0;
                    break;
                case BinaryTestSize.Normal:
                    amount = 0x100;
                    break;
                case BinaryTestSize.Large:
                    amount = 0x8000 + 128;
                    break;
                case BinaryTestSize.PipeWriteLimit:
                    amount = 0xFFFF + 128;
                    break;
                case BinaryTestSize.FifteenMB:
                    amount = 15 * 1024 * 1024;
                    break;
                default:
                    throw new ArgumentException(nameof(testcase));
            }

            var retval = new byte[amount];

            // linear distribution, unless random is requested
            if (!randomDist)
            {
                for (var i = 0; i < retval.Length; ++i)
                {
                    retval[i] = (byte)i;
                }
                return retval;
            }

            // random distribution
            var rnd = new Random();
            for (var i = 1; i < retval.Length; ++i)
            {
                retval[i] = (byte)rnd.Next(0x100);
            }
            return retval;
        }

        private static CancellationToken MakeTimeoutToken(int msec = 5000)
        {
            var token = new CancellationTokenSource(msec);
            return token.Token;
        }

        public static async Task<int> ExecuteClientTestAsync(ThriftTest.Client client)
        {
            var returnCode = 0;

            Console.Write("testVoid()");
            await client.testVoidAsync(MakeTimeoutToken());
            Console.WriteLine(" = void");

            Console.Write("testString(\"Test\")");
            var s = await client.testStringAsync("Test", MakeTimeoutToken());
            Console.WriteLine(" = \"" + s + "\"");
            if ("Test" != s)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }

            Console.Write("testBool(true)");
            var t = await client.testBoolAsync((bool)true, MakeTimeoutToken());
            Console.WriteLine(" = " + t);
            if (!t)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }
            Console.Write("testBool(false)");
            var f = await client.testBoolAsync((bool)false, MakeTimeoutToken());
            Console.WriteLine(" = " + f);
            if (f)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }

            Console.Write("testByte(1)");
            var i8 = await client.testByteAsync((sbyte)1, MakeTimeoutToken());
            Console.WriteLine(" = " + i8);
            if (1 != i8)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }

            Console.Write("testI32(-1)");
            var i32 = await client.testI32Async(-1, MakeTimeoutToken());
            Console.WriteLine(" = " + i32);
            if (-1 != i32)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }

            Console.Write("testI64(-34359738368)");
            var i64 = await client.testI64Async(-34359738368, MakeTimeoutToken());
            Console.WriteLine(" = " + i64);
            if (-34359738368 != i64)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }

            // TODO: Validate received message
            Console.Write("testDouble(5.325098235)");
            var dub = await client.testDoubleAsync(5.325098235, MakeTimeoutToken());
            Console.WriteLine(" = " + dub);
            if (5.325098235 != dub)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }
            Console.Write("testDouble(-0.000341012439638598279)");
            dub = await client.testDoubleAsync(-0.000341012439638598279, MakeTimeoutToken());
            Console.WriteLine(" = " + dub);
            if (-0.000341012439638598279 != dub)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }

            // testBinary()
            foreach(BinaryTestSize binTestCase in Enum.GetValues(typeof(BinaryTestSize)))
            {
                var binOut = PrepareTestData(true, binTestCase);

                Console.Write("testBinary({0} bytes)", binOut.Length);
                try
                {
                    var binIn = await client.testBinaryAsync(binOut, MakeTimeoutToken());
                    Console.WriteLine(" = {0} bytes", binIn.Length);
                    if (binIn.Length != binOut.Length)
                    {
                        Console.WriteLine("*** FAILED ***");
                        returnCode |= ErrorBaseTypes;
                    }
                    for (var ofs = 0; ofs < Math.Min(binIn.Length, binOut.Length); ++ofs)
                    {
                        if (binIn[ofs] != binOut[ofs])
                        {
                            Console.WriteLine("*** FAILED ***");
                            returnCode |= ErrorBaseTypes;
                        }
                    }
                }
                catch (Thrift.TApplicationException ex)
                {
                    Console.WriteLine("*** FAILED ***");
                    returnCode |= ErrorBaseTypes;
                    Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
                }
            }

            // CrazyNesting
            Console.WriteLine("Test CrazyNesting");
            var one = new CrazyNesting();
            var two = new CrazyNesting();
            one.String_field = "crazy";
            two.String_field = "crazy";
            one.Binary_field = new byte[] { 0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0xFF };
            two.Binary_field = new byte[10] { 0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0xFF };
            if (typeof(CrazyNesting).GetMethod("Equals")?.DeclaringType == typeof(CrazyNesting))
            {
                if (!one.Equals(two))
                {
                    Console.WriteLine("*** FAILED ***");
                    returnCode |= ErrorContainers;
                }
            }

            // TODO: Validate received message
            Console.Write("testStruct({\"Zero\", 1, -3, -5})");
            var o = new Xtruct
            {
                String_thing = "Zero",
                Byte_thing = (sbyte)1,
                I32_thing = -3,
                I64_thing = -5
            };
            var i = await client.testStructAsync(o, MakeTimeoutToken());
            Console.WriteLine(" = {\"" + i.String_thing + "\", " + i.Byte_thing + ", " + i.I32_thing + ", " + i.I64_thing + "}");

            // TODO: Validate received message
            Console.Write("testNest({1, {\"Zero\", 1, -3, -5}, 5})");
            var o2 = new Xtruct2
            {
                Byte_thing = (sbyte)1,
                Struct_thing = o,
                I32_thing = 5
            };
            var i2 = await client.testNestAsync(o2, MakeTimeoutToken());
            i = i2.Struct_thing;
            Console.WriteLine(" = {" + i2.Byte_thing + ", {\"" + i.String_thing + "\", " + i.Byte_thing + ", " + i.I32_thing + ", " + i.I64_thing + "}, " + i2.I32_thing + "}");

            var mapout = new Dictionary<int, int>();
            for (var j = 0; j < 5; j++)
            {
                mapout[j] = j - 10;
            }
            Console.Write("testMap({");
            var first = true;
            foreach (var key in mapout.Keys)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    Console.Write(", ");
                }
                Console.Write(key + " => " + mapout[key]);
            }
            Console.Write("})");

            var mapin = await client.testMapAsync(mapout, MakeTimeoutToken());

            Console.Write(" = {");
            first = true;
            foreach (var key in mapin.Keys)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    Console.Write(", ");
                }
                Console.Write(key + " => " + mapin[key]);
            }
            Console.WriteLine("}");

            // TODO: Validate received message
            var listout = new List<int>();
            for (var j = -2; j < 3; j++)
            {
                listout.Add(j);
            }
            Console.Write("testList({");
            first = true;
            foreach (var j in listout)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    Console.Write(", ");
                }
                Console.Write(j);
            }
            Console.Write("})");

            var listin = await client.testListAsync(listout, MakeTimeoutToken());

            Console.Write(" = {");
            first = true;
            foreach (var j in listin)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    Console.Write(", ");
                }
                Console.Write(j);
            }
            Console.WriteLine("}");

            //set
            // TODO: Validate received message
            var setout = new THashSet<int>();
            for (var j = -2; j < 3; j++)
            {
                setout.Add(j);
            }
            Console.Write("testSet({");
            first = true;
            foreach (int j in setout)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    Console.Write(", ");
                }
                Console.Write(j);
            }
            Console.Write("})");

            var setin = await client.testSetAsync(setout, MakeTimeoutToken());

            Console.Write(" = {");
            first = true;
            foreach (int j in setin)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    Console.Write(", ");
                }
                Console.Write(j);
            }
            Console.WriteLine("}");


            Console.Write("testEnum(ONE)");
            var ret = await client.testEnumAsync(Numberz.ONE, MakeTimeoutToken());
            Console.WriteLine(" = " + ret);
            if (Numberz.ONE != ret)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorStructs;
            }

            Console.Write("testEnum(TWO)");
            ret = await client.testEnumAsync(Numberz.TWO, MakeTimeoutToken());
            Console.WriteLine(" = " + ret);
            if (Numberz.TWO != ret)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorStructs;
            }

            Console.Write("testEnum(THREE)");
            ret = await client.testEnumAsync(Numberz.THREE, MakeTimeoutToken());
            Console.WriteLine(" = " + ret);
            if (Numberz.THREE != ret)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorStructs;
            }

            Console.Write("testEnum(FIVE)");
            ret = await client.testEnumAsync(Numberz.FIVE, MakeTimeoutToken());
            Console.WriteLine(" = " + ret);
            if (Numberz.FIVE != ret)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorStructs;
            }

            Console.Write("testEnum(EIGHT)");
            ret = await client.testEnumAsync(Numberz.EIGHT, MakeTimeoutToken());
            Console.WriteLine(" = " + ret);
            if (Numberz.EIGHT != ret)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorStructs;
            }

            Console.Write("testTypedef(309858235082523)");
            var uid = await client.testTypedefAsync(309858235082523L, MakeTimeoutToken());
            Console.WriteLine(" = " + uid);
            if (309858235082523L != uid)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorStructs;
            }

            // TODO: Validate received message
            Console.Write("testMapMap(1)");
            var mm = await client.testMapMapAsync(1, MakeTimeoutToken());
            Console.Write(" = {");
            foreach (var key in mm.Keys)
            {
                Console.Write(key + " => {");
                var m2 = mm[key];
                foreach (var k2 in m2.Keys)
                {
                    Console.Write(k2 + " => " + m2[k2] + ", ");
                }
                Console.Write("}, ");
            }
            Console.WriteLine("}");

            // TODO: Validate received message
            var insane = new Insanity
            {
                UserMap = new Dictionary<Numberz, long>
                {
                    [Numberz.FIVE] = 5000L
                }
            };
            var truck = new Xtruct
            {
                String_thing = "Truck",
                Byte_thing = (sbyte)8,
                I32_thing = 8,
                I64_thing = 8
            };
            insane.Xtructs = new List<Xtruct>
            {
                truck
            };
            Console.Write("testInsanity()");
            var whoa = await client.testInsanityAsync(insane, MakeTimeoutToken());
            Console.Write(" = {");
            foreach (var key in whoa.Keys)
            {
                var val = whoa[key];
                Console.Write(key + " => {");

                foreach (var k2 in val.Keys)
                {
                    var v2 = val[k2];

                    Console.Write(k2 + " => {");
                    var userMap = v2.UserMap;

                    Console.Write("{");
                    if (userMap != null)
                    {
                        foreach (var k3 in userMap.Keys)
                        {
                            Console.Write(k3 + " => " + userMap[k3] + ", ");
                        }
                    }
                    else
                    {
                        Console.Write("null");
                    }
                    Console.Write("}, ");

                    var xtructs = v2.Xtructs;

                    Console.Write("{");
                    if (xtructs != null)
                    {
                        foreach (var x in xtructs)
                        {
                            Console.Write("{\"" + x.String_thing + "\", " + x.Byte_thing + ", " + x.I32_thing + ", " + x.I32_thing + "}, ");
                        }
                    }
                    else
                    {
                        Console.Write("null");
                    }
                    Console.Write("}");

                    Console.Write("}, ");
                }
                Console.Write("}, ");
            }
            Console.WriteLine("}");

            sbyte arg0 = 1;
            var arg1 = 2;
            var arg2 = long.MaxValue;
            var multiDict = new Dictionary<short, string>
            {
                [1] = "one"
            };

            var tmpMultiDict = new List<string>();
            foreach (var pair in multiDict)
                tmpMultiDict.Add(pair.Key +" => "+ pair.Value);

            var arg4 = Numberz.FIVE;
            long arg5 = 5000000;
            Console.Write("Test Multi(" + arg0 + "," + arg1 + "," + arg2 + ",{" + string.Join(",", tmpMultiDict) + "}," + arg4 + "," + arg5 + ")");
            var multiResponse = await client.testMultiAsync(arg0, arg1, arg2, multiDict, arg4, arg5, MakeTimeoutToken());
            Console.Write(" = Xtruct(byte_thing:" + multiResponse.Byte_thing + ",String_thing:" + multiResponse.String_thing
                          + ",i32_thing:" + multiResponse.I32_thing + ",i64_thing:" + multiResponse.I64_thing + ")\n");

            try
            {
                Console.WriteLine("testException(\"Xception\")");
                await client.testExceptionAsync("Xception", MakeTimeoutToken());
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
            }
            catch (Xception ex)
            {
                if (ex.ErrorCode != 1001 || ex.Message != "Xception")
                {
                    Console.WriteLine("*** FAILED ***");
                    returnCode |= ErrorExceptions;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
                Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
            }
            try
            {
                Console.WriteLine("testException(\"TException\")");
                await client.testExceptionAsync("TException", MakeTimeoutToken());
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
            }
            catch (Thrift.TException)
            {
                // OK
            }
            catch (Exception ex)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
                Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
            }
            try
            {
                Console.WriteLine("testException(\"ok\")");
                await client.testExceptionAsync("ok", MakeTimeoutToken());
                // OK
            }
            catch (Exception ex)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
                Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
            }

            try
            {
                Console.WriteLine("testMultiException(\"Xception\", ...)");
                await client.testMultiExceptionAsync("Xception", "ignore", MakeTimeoutToken());
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
            }
            catch (Xception ex)
            {
                if (ex.ErrorCode != 1001 || ex.Message != "This is an Xception")
                {
                    Console.WriteLine("*** FAILED ***");
                    returnCode |= ErrorExceptions;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
                Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
            }
            try
            {
                Console.WriteLine("testMultiException(\"Xception2\", ...)");
                await client.testMultiExceptionAsync("Xception2", "ignore", MakeTimeoutToken());
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
            }
            catch (Xception2 ex)
            {
                if (ex.ErrorCode != 2002 || ex.Struct_thing.String_thing != "This is an Xception2")
                {
                    Console.WriteLine("*** FAILED ***");
                    returnCode |= ErrorExceptions;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
                Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
            }
            try
            {
                Console.WriteLine("testMultiException(\"success\", \"OK\")");
                if ("OK" != (await client.testMultiExceptionAsync("success", "OK", MakeTimeoutToken())).String_thing)
                {
                    Console.WriteLine("*** FAILED ***");
                    returnCode |= ErrorExceptions;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorExceptions;
                Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
            }

            Console.WriteLine("Test Oneway(1)");
            var sw = new Stopwatch();
            sw.Start();
            await client.testOnewayAsync(1, MakeTimeoutToken());
            sw.Stop();
            if (sw.ElapsedMilliseconds > 1000)
            {
                Console.WriteLine("*** FAILED ***");
                returnCode |= ErrorBaseTypes;
            }

            Console.Write("Test Calltime()");
            var times = 50;
            sw.Reset();
            sw.Start();
            var token = MakeTimeoutToken(20000);
            for (var k = 0; k < times; ++k)
                await client.testVoidAsync(token);
            sw.Stop();
            Console.WriteLine(" = {0} ms a testVoid() call", sw.ElapsedMilliseconds / times);
            return returnCode;
        }
    }
}
