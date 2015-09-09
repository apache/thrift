/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Diagnostics;
using System.Collections.Generic;
using Thrift;
using Thrift.Collections;
using Thrift.Transport;
using Thrift.Protocol;
using Thrift.Test;
using Test;

namespace TestIOStreamClient
{
    [TestClass]
    public class TestIOStreamClient
    {
        private static string serverFileName = "..\\..\\..\\TestStreamServer\\bin\\Debug\\TestStreamServer.exe";
        private static string protocolName = "binary";
        private static TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();
        private static TStreamTransport transport;
        private static Process other;
        private static ThriftTest.Client client;

        [TestInitialize]
        public void setUp()
        {
            var server = new ProcessStartInfo();
            server.FileName = serverFileName;
            server.Arguments = "--" + protocolName;
            server.UseShellExecute = false;
            server.RedirectStandardInput = true;
            server.RedirectStandardOutput = true;

            other = Process.Start(server);

            Console.SetIn(other.StandardOutput);
            Console.SetOut(other.StandardInput);

            transport = new TStreamTransport(other.StandardOutput.BaseStream, other.StandardInput.BaseStream);
            TProtocol protocol = protocolFactory.GetProtocol(transport);
            client = new ThriftTest.Client(protocol);

            transport.Open();
        }

        [TestCleanup]
        public void tearDown()
        {
            transport.Close();
            //close server
            other.Kill();//kills process
            other.Close();//cleans up
        }

        [TestMethod]
        public void testVoid()
        {
            client.testVoid();
        }

        [TestMethod]
        public void testString()
        {
            string s = client.testString("Test");
            Assert.AreEqual("Test", s);
        }

        [TestMethod]
        public void testByte()
        {
            sbyte b = client.testByte((sbyte)1);
            Assert.AreEqual(1, b);
        }

        [TestMethod]
        public void testI32()
        {
            int i = client.testI32(-1);
            Assert.AreEqual(-1, i);
        }

        [TestMethod]
        public void testI64()
        {
            long l = client.testI64(-34359738368);
            Assert.AreEqual(-34359738368, l);
        }

        [TestMethod]
        public void testDouble()
        {
            double d = client.testDouble(5.325098235);
            Assert.AreEqual(5.325098235, d);
        }

        [TestMethod]
        public void testBinary()
        {
            byte[] binOut = TestClient.PrepareTestData(true);
            byte[] binIn = client.testBinary(binOut);
            Assert.AreEqual(binOut.Length, binIn.Length);
            for (int ofs = 0; ofs < binIn.Length; ++ofs)
                Assert.AreEqual(binOut[ofs], binIn[ofs]);
        }

        [TestMethod]
        public void testStruct()
        {
            Xtruct o = new Xtruct();
            o.String_thing = "Zero";
            o.Byte_thing = (sbyte)1;
            o.I32_thing = -3;
            o.I64_thing = -5;
            Xtruct i = client.testStruct(o);
            Assert.AreEqual(o.String_thing, i.String_thing);
            Assert.AreEqual(o.Byte_thing, i.Byte_thing);
            Assert.AreEqual(o.I32_thing, i.I32_thing);
            Assert.AreEqual(o.I64_thing, i.I64_thing);
        }

        [TestMethod]
        public void testNest()
        {
            Xtruct o = new Xtruct();
            o.String_thing = "Zero";
            o.Byte_thing = (sbyte)1;
            o.I32_thing = -3;
            o.I64_thing = -5;

            Xtruct2 o2 = new Xtruct2();
            o2.Byte_thing = (sbyte)1;
            o2.Struct_thing = o;
            o2.I32_thing = 5;
            Xtruct2 i2 = client.testNest(o2);
            Assert.AreEqual(o2.Byte_thing, i2.Byte_thing);
            Assert.AreEqual(o2.Struct_thing.String_thing, i2.Struct_thing.String_thing);
            Assert.AreEqual(o2.Struct_thing.Byte_thing, i2.Struct_thing.Byte_thing);
            Assert.AreEqual(o2.Struct_thing.I32_thing, i2.Struct_thing.I32_thing);
            Assert.AreEqual(o2.Struct_thing.I64_thing, i2.Struct_thing.I64_thing);
            Assert.AreEqual(o2.I32_thing, i2.I32_thing);
        }

        [TestMethod]
        public void testMap()
        {
            Dictionary<int, int> mapout = new Dictionary<int, int>();
            for (int j = 0; j < 5; j++)
            {
                mapout[j] = j - 10;
            }

            Dictionary<int, int> mapin = client.testMap(mapout);
            Assert.AreEqual(mapout.Count, mapin.Count);
            foreach (KeyValuePair<int, int> pair in mapout)
            {
                Assert.AreEqual(pair.Value, mapin[pair.Key]);
            }
        }

        [TestMethod]
        public void testList()
        {
            List<int> listout = new List<int>();
            for (int j = -2; j < 3; j++)
            {
                listout.Add(j);
            }

            List<int> listin = client.testList(listout);

            Assert.AreEqual(listout.Count, listin.Count);
            foreach (int i in listout)
            {
                Assert.IsTrue(listin.Contains(i));
            }
        }

        [TestMethod]
        public void testSet()
        {
            THashSet<int> setout = new THashSet<int>();
            for (int j = -2; j < 3; j++)
            {
                setout.Add(j);
            }

            THashSet<int> setin = client.testSet(setout);

            Assert.AreEqual(setout.Count, setin.Count);
            foreach (int i in setout)
            {
                Assert.IsTrue(setin.Contains(i));
            }
        }

        [TestMethod]
        public void testEnum()
        {
            Assert.AreEqual(Numberz.ONE, client.testEnum(Numberz.ONE));
            Assert.AreEqual(Numberz.TWO, client.testEnum(Numberz.TWO));
            Assert.AreEqual(Numberz.THREE, client.testEnum(Numberz.THREE));
            Assert.AreEqual(Numberz.FIVE, client.testEnum(Numberz.FIVE));
            Assert.AreEqual(Numberz.EIGHT, client.testEnum(Numberz.EIGHT));
        }

        [TestMethod]
        public void testTypedef()
        {
            long uid = client.testTypedef(309858235082523L);
            Assert.AreEqual(309858235082523L, uid);
        }

        [TestMethod]
        public void testMapMap()
        {
            Dictionary<int, Dictionary<int, int>> mm = client.testMapMap(1);

            Dictionary<int, Dictionary<int, int>> mapmap =
              new Dictionary<int, Dictionary<int, int>>();

            Dictionary<int, int> pos = new Dictionary<int, int>();
            Dictionary<int, int> neg = new Dictionary<int, int>();
            for (int i = 1; i < 5; i++)
            {
                pos[i] = i;
                neg[-i] = -i;
            }

            mapmap[4] = pos;
            mapmap[-4] = neg;

            Assert.AreEqual(mapmap.Count, mm.Count);
            foreach (KeyValuePair<int, Dictionary<int, int>> pair in mapmap)
            {
                foreach (KeyValuePair<int, int> p in mapmap[pair.Key])
                {
                    Assert.AreEqual(p.Value, mm[pair.Key][p.Key]);
                }
            }
        }

        [TestMethod]
        public void testInsanity()
        {
            //insane is what testInsanity always returns, regardless of its argument.
            Xtruct hello = new Xtruct();
            hello.String_thing = "Hello2";
            hello.Byte_thing = 2;
            hello.I32_thing = 2;
            hello.I64_thing = 2;

            Xtruct goodbye = new Xtruct();
            goodbye.String_thing = "Goodbye4";
            goodbye.Byte_thing = (sbyte)4;
            goodbye.I32_thing = 4;
            goodbye.I64_thing = (long)4;

            Insanity crazy = new Insanity();
            crazy.UserMap = new Dictionary<Numberz, long>();
            crazy.UserMap[Numberz.EIGHT] = (long)8;
            crazy.Xtructs = new List<Xtruct>();
            crazy.Xtructs.Add(goodbye);

            Insanity looney = new Insanity();
            crazy.UserMap[Numberz.FIVE] = (long)5;
            crazy.Xtructs.Add(hello);

            Dictionary<Numberz, Insanity> first_map = new Dictionary<Numberz, Insanity>();
            Dictionary<Numberz, Insanity> second_map = new Dictionary<Numberz, Insanity>(); ;

            first_map[Numberz.TWO] = crazy;
            first_map[Numberz.THREE] = crazy;

            second_map[Numberz.SIX] = looney;

            Dictionary<long, Dictionary<Numberz, Insanity>> insane =
              new Dictionary<long, Dictionary<Numberz, Insanity>>();
            insane[(long)1] = first_map;
            insane[(long)2] = second_map;

            //construct argument for testInsanity
            Insanity insaneNew = new Insanity();
            insaneNew.UserMap = new Dictionary<Numberz, long>();
            insaneNew.UserMap[Numberz.FIVE] = 5000L;
            Xtruct truck = new Xtruct();
            truck.String_thing = "Truck";
            truck.Byte_thing = (sbyte)8;
            truck.I32_thing = 8;
            truck.I64_thing = 8;
            insaneNew.Xtructs = new List<Xtruct>();
            insaneNew.Xtructs.Add(truck);

            Dictionary<long, Dictionary<Numberz, Insanity>> whoa = client.testInsanity(insaneNew);
            Assert.AreEqual(insane.Count, whoa.Count);
            foreach (KeyValuePair<long, Dictionary<Numberz, Insanity>> pair in insane)
            {
                Assert.AreEqual(pair.Value.Count, whoa[pair.Key].Count);
                foreach (KeyValuePair<Numberz, Insanity> p in pair.Value)
                {
                    Assert.IsTrue(whoa[pair.Key].ContainsKey(p.Key));
                    if (p.Key.Equals(Numberz.TWO) || p.Key.Equals(Numberz.THREE))
                    {
                        Assert.AreEqual(p.Value.UserMap[Numberz.EIGHT], whoa[pair.Key][p.Key].UserMap[Numberz.EIGHT]);
                        Assert.AreEqual(p.Value.Xtructs.Count, whoa[pair.Key][p.Key].Xtructs.Count);
                        Assert.AreEqual(p.Value.Xtructs.ToArray()[0].String_thing, whoa[pair.Key][p.Key].Xtructs.ToArray()[0].String_thing);
                        Assert.AreEqual(p.Value.Xtructs.ToArray()[0].Byte_thing, whoa[pair.Key][p.Key].Xtructs.ToArray()[0].Byte_thing);
                        Assert.AreEqual(p.Value.Xtructs.ToArray()[0].I32_thing, whoa[pair.Key][p.Key].Xtructs.ToArray()[0].I32_thing);
                        Assert.AreEqual(p.Value.Xtructs.ToArray()[0].I64_thing, whoa[pair.Key][p.Key].Xtructs.ToArray()[0].I64_thing);
                    }
                    else if (p.Key.Equals(Numberz.SIX))
                    {
                        Assert.IsNull(whoa[pair.Key][p.Key].Xtructs);
                        Assert.IsNull(whoa[pair.Key][p.Key].UserMap);
                    }
                }
            }
        }

        [TestMethod]
        public void testMulti()
        {
            sbyte arg0 = 1;
            int arg1 = 2;
            long arg2 = long.MaxValue;
            Dictionary<short, string> multiDict = new Dictionary<short, string>();
            multiDict[1] = "one";
            Numberz arg4 = Numberz.FIVE;
            long arg5 = 5000000;

            Xtruct multiResponse = client.testMulti(arg0, arg1, arg2, multiDict, arg4, arg5);

            Xtruct hello = new Xtruct(); ;
            hello.String_thing = "Hello2";
            hello.Byte_thing = arg0;
            hello.I32_thing = arg1;
            hello.I64_thing = arg2;

            Assert.AreEqual(hello.String_thing, multiResponse.String_thing);
            Assert.AreEqual(hello.Byte_thing, multiResponse.Byte_thing);
            Assert.AreEqual(hello.I32_thing, multiResponse.I32_thing);
            Assert.AreEqual(hello.I64_thing, multiResponse.I64_thing);
        }

        [TestMethod]
        public void testOneWay()
        {
            client.testOneway(1);
        }

        [TestMethod]
        public void testException()
        {
            try
            {
                client.testException("Xception");
                throw new AssertFailedException("should have thrown Xception");
            }
            catch (Xception x)
            {
                Assert.AreEqual("Xception", x.Message);
                Assert.AreEqual(1001, x.ErrorCode);
            }

            //this closes the server side, so it won't pass.
            //try
            //{
            //    client.testException("TException");
            //    throw new AssertFailedException("should have thrown TException");
            //}
            //catch (TException t)
            //{
            //    Assert.AreEqual("TException", t.Message);
            //}
    
            client.testException("success");
        }

        [TestMethod]
        public void testMultiException()
        {
            try
            {
                client.testMultiException("Xception", "hello");
                throw new AssertFailedException("should have thrown Xception");
            }
            catch(Xception x)
            {
                Assert.AreEqual("This is an Xception", x.Message);
                Assert.AreEqual(1001, x.ErrorCode);
            }

            try
            {
                client.testMultiException("Xception2", "hello");
                throw new AssertFailedException("should have thrown Xception2");
            }
            catch (Xception2 x)
            {
                Assert.AreEqual(2002, x.ErrorCode);
                Assert.AreEqual("This is an Xception2", x.Struct_thing.String_thing);
            }

            Assert.AreEqual("hello", client.testMultiException("success", "hello").String_thing);
        }
    }
}
