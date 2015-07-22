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
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Thrift.Transport;
using Thrift.Protocol;
using Thrift.Collections;
using System.Diagnostics;
using Test;

namespace Thrift.Test
{
    public class TestIOStreamClient
    {
        private static string protocolName = "";
        private static string serverFileName = "TestStreamServer.exe";
        //private static string serverFileName = "..\\..\\..\\TestStreamServer\\bin\\Debug\\TestStreamServer.exe";

        private static TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();

        public static bool Execute(string[] args)
        {
            try
            {
                //we know we are using the TStreamTransport--it's all the TStreamServer allows.
                //any protocol is OK though
                for (int i = 0; i < args.Length; i++)
                {
                    if (args[i] == "--binary" || args[i] == "--protocol=binary")
                    {
                        protocolName = "binary";
                        protocolFactory = new TBinaryProtocol.Factory();
                        Console.WriteLine("Using binary protocol");
                    }
                    if (args[i] == "--compact" || args[i] == "--protocol=compact")
                    {
                        protocolName = "compact";
                        protocolFactory = new TCompactProtocol.Factory();
                        Console.WriteLine("Using compact protocol");
                    }
                    else if (args[i] == "--json" || args[i] == "--protocol=json")
                    {
                        protocolName = "json";
                        protocolFactory = new TJSONProtocol.Factory();
                        Console.WriteLine("Using JSON protocol");
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.StackTrace);
            }

            //to store answers (since we can't print them right away with a redirected output stream
            string stringThing;
            sbyte sbyteThing;
            int intthing;
            long longThing;
            double doubleThing;
            byte[] byteArrayThing, binOut;
            Xtruct XtructThing;
            Xtruct2 Xtruct2Thing;
            Dictionary<int, int> intMapThing, mapout;
            THashSet<int> setThing, setout;
            List<int> listThing, listout;
            Numberz enumThing1, enumThing2, enumThing3, enumThing5, enumThing8;
            long typedefThing;
            Dictionary<int, Dictionary<int, int>> mapMapThing;
            Dictionary<long, Dictionary<Numberz, Insanity>> insanityThing;
            Xtruct multiThing;

            sbyte arg0;
            int arg1;
            long arg2;
            Dictionary<short, string> multiDict;
            Numberz arg4;
            long arg5;

            //spawn the server as a new process
            var tmp = Console.Out;//save output stream for writing the output back out to standard out at the end
            var server = new ProcessStartInfo();
            server.FileName = serverFileName;
            server.Arguments = "--" + protocolName;
            server.UseShellExecute = false;
            server.RedirectStandardInput = true;
            server.RedirectStandardOutput = true;

            Process other = Process.Start(server);

            Console.SetIn(other.StandardOutput);
            Console.SetOut(other.StandardInput);

            TStreamTransport transport = new TStreamTransport(other.StandardOutput.BaseStream, other.StandardInput.BaseStream);
            TProtocol protocol = protocolFactory.GetProtocol(transport);
            ThriftTest.Client client = new ThriftTest.Client(protocol);

            transport.Open();
            try
            {
                client.testVoid();
                stringThing = client.testString("Test");
                sbyteThing = client.testByte((sbyte)1);
                intthing = client.testI32(-1);
                longThing = client.testI64(-34359738368);
                doubleThing = client.testDouble(5.325098235);

                binOut = TestClient.PrepareTestData(true);
                byteArrayThing = client.testBinary(binOut);

                //struct
                Xtruct o = new Xtruct();
                o.String_thing = "Zero";
                o.Byte_thing = (sbyte)1;
                o.I32_thing = -3;
                o.I64_thing = -5;
                XtructThing = client.testStruct(o);

                //nest
                Xtruct2 o2 = new Xtruct2();
                o2.Byte_thing = (sbyte)1;
                o2.Struct_thing = o;
                o2.I32_thing = 5;
                Xtruct2Thing = client.testNest(o2);

                //map
                mapout = new Dictionary<int, int>();
                for (int j = 0; j < 5; j++)
                {
                    mapout[j] = j - 10;
                }
                intMapThing = client.testMap(mapout);

                //list
                listout = new List<int>();
                for (int j = -2; j < 3; j++)
                {
                    listout.Add(j);
                }
                listThing = client.testList(listout);

                //set
                setout = new THashSet<int>();
                for (int j = -2; j < 3; j++)
                {
                    setout.Add(j);
                }
                setThing = client.testSet(setout);

                enumThing1 = client.testEnum(Numberz.ONE);
                enumThing2 = client.testEnum(Numberz.TWO);
                enumThing3 = client.testEnum(Numberz.THREE);
                enumThing5 = client.testEnum(Numberz.FIVE);
                enumThing8 = client.testEnum(Numberz.EIGHT);

                typedefThing = client.testTypedef(309858235082523L);

                mapMapThing = client.testMapMap(1);

                Insanity insane = new Insanity();
                insane.UserMap = new Dictionary<Numberz, long>();
                insane.UserMap[Numberz.FIVE] = 5000L;
                Xtruct truck = new Xtruct();
                truck.String_thing = "Truck";
                truck.Byte_thing = (sbyte)8;
                truck.I32_thing = 8;
                truck.I64_thing = 8;
                insane.Xtructs = new List<Xtruct>();
                insane.Xtructs.Add(truck);
                insanityThing = client.testInsanity(insane);

                arg0 = 1;
                arg1 = 2;
                arg2 = long.MaxValue;
                multiDict = new Dictionary<short, string>();
                multiDict[1] = "one";
                arg4 = Numberz.FIVE;
                arg5 = 5000000;
                multiThing = client.testMulti(arg0, arg1, arg2, multiDict, arg4, arg5);

                client.testOneway(1);
            }
            finally
            {
                transport.Close();
            }

            //close server
            other.Kill();//kills process
            other.Close();//cleans up

            //write results
            Console.SetOut(tmp);//set output back to what it was.

            //I'm writing out these too so that I can diff them with the original output
            Console.WriteLine("testVoid() = void");

            Console.WriteLine("testString(\"Test\") = \"" + stringThing + "\"");
            Console.WriteLine("testByte(1) = " + sbyteThing);
            Console.WriteLine("testI32(-1) = " + intthing);
            Console.WriteLine("testI64(-34359738368) = " + longThing);
            Console.WriteLine("testDouble(5.325098235) = " + doubleThing);

            Console.Write("testBinary(" + TestClient.BytesToHex(binOut) + ")");
            Console.WriteLine(" = " + TestClient.BytesToHex(byteArrayThing));

            Console.WriteLine("testStruct({\"Zero\", 1, -3, -5}) = {\"" + XtructThing.String_thing + "\", " + XtructThing.Byte_thing + ", " + XtructThing.I32_thing + ", " + XtructThing.I64_thing + "}");
            Console.WriteLine("testNest({1, {\"Zero\", 1, -3, -5}, 5}) = {" + Xtruct2Thing.Byte_thing + ", {\"" + XtructThing.String_thing + "\", " + XtructThing.Byte_thing + ", " + XtructThing.I32_thing + ", " + XtructThing.I64_thing + "}, " + Xtruct2Thing.I32_thing + "}");
            
            Console.Write("testMap({");
            bool first = true;
            foreach (int key in mapout.Keys)
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
            Console.Write(" = {");
            first = true;
            foreach (int key in intMapThing.Keys)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    Console.Write(", ");
                }
                Console.Write(key + " => " + intMapThing[key]);
            }
            Console.WriteLine("}");

            
            Console.Write("testList({");
            first = true;
            foreach (int j in listout)
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
            Console.Write(" = {");
            first = true;
            foreach (int j in listThing)
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
            Console.Write(" = {");
            first = true;
            foreach (int j in setThing)
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

            Console.WriteLine("testEnum(ONE) = " + enumThing1);
            Console.WriteLine("testEnum(TWO) = " + enumThing2);
            Console.WriteLine("testEnum(THREE) = " + enumThing3);
            Console.WriteLine("testEnum(FIVE) = " + enumThing5);
            Console.WriteLine("testEnum(EIGHT) = " + enumThing8);

            Console.WriteLine("testTypedef(309858235082523) = " + typedefThing);

            Console.Write("testMapMap(1) = {");
            foreach (int key in mapMapThing.Keys)
            {
                Console.Write(key + " => {");
                Dictionary<int, int> m2 = mapMapThing[key];
                foreach (int k2 in m2.Keys)
                {
                    Console.Write(k2 + " => " + m2[k2] + ", ");
                }
                Console.Write("}, ");
            }
            Console.WriteLine("}");

            Console.Write("testInsanity()");
            Console.Write(" = {");
            foreach (long key in insanityThing.Keys)
            {
                Dictionary<Numberz, Insanity> val = insanityThing[key];
                Console.Write(key + " => {");

                foreach (Numberz k2 in val.Keys)
                {
                    Insanity v2 = val[k2];

                    Console.Write(k2 + " => {");
                    Dictionary<Numberz, long> userMap = v2.UserMap;

                    Console.Write("{");
                    if (userMap != null)
                    {
                        foreach (Numberz k3 in userMap.Keys)
                        {
                            Console.Write(k3 + " => " + userMap[k3] + ", ");
                        }
                    }
                    else
                    {
                        Console.Write("null");
                    }
                    Console.Write("}, ");

                    List<Xtruct> xtructs = v2.Xtructs;

                    Console.Write("{");
                    if (xtructs != null)
                    {
                        foreach (Xtruct x in xtructs)
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

            Console.Write("Test Multi(" + arg0 + "," + arg1 + "," + arg2 + "," + multiDict + "," + arg4 + "," + arg5 + ")");
            Console.Write(" = Xtruct(byte_thing:" + multiThing.Byte_thing + ",String_thing:" + multiThing.String_thing
                        + ",i32_thing:" + multiThing.I32_thing + ",i64_thing:" + multiThing.I64_thing + ")\n");

            Console.WriteLine("Test Oneway(1)");

            return true;
        }       
    }
}

