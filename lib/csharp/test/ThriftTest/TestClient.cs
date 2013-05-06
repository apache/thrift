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
using System.Threading;
using Thrift.Collections;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Test;

namespace Test
{
	public class TestClient
	{
		private static int numIterations = 1;

		public static void Execute(string[] args)
		{
			try
			{
				string host = "localhost";
				int port = 9090;
				string url = null;
				int numThreads = 1;
				bool buffered = false, framed = false;

				try
				{
					for (int i = 0; i < args.Length; i++)
					{
						if (args[i] == "-h")
						{
							string[] hostport = args[++i].Split(':');
							host = hostport[0];
							if (hostport.Length > 1)
							{
								port = Convert.ToInt32(hostport[1]);
							}
						}
						else if (args[i] == "-u")
						{
							url = args[++i];
						}
						else if (args[i] == "-n")
						{
							numIterations = Convert.ToInt32(args[++i]);
						}
						else if (args[i] == "-b" || args[i] == "-buffered")
						{
							buffered = true;
							Console.WriteLine("Using buffered sockets");
						}
						else if (args[i] == "-f" || args[i] == "-framed")
						{
							framed = true;
							Console.WriteLine("Using framed transport");
						}
						else if (args[i] == "-t")
						{
							numThreads = Convert.ToInt32(args[++i]);
						}
					}
				}
				catch (Exception e)
				{
					Console.WriteLine(e.StackTrace);
				}



				//issue tests on separate threads simultaneously
				Thread[] threads = new Thread[numThreads];
				DateTime start = DateTime.Now;
				for (int test = 0; test < numThreads; test++)
				{
					Thread t = new Thread(new ParameterizedThreadStart(ClientThread));
					threads[test] = t;
					if (url == null)
					{
						TTransport trans = new TSocket(host, port);
						if (buffered)
							trans = new TBufferedTransport(trans as TStreamTransport);
						if (framed)
							trans = new TFramedTransport(trans);
							
						t.Start(trans);
					}
					else
					{
						THttpClient http = new THttpClient(new Uri(url));
						t.Start(http);
					}
				}

				for (int test = 0; test < numThreads; test++)
				{
					threads[test].Join();
				}
				Console.Write("Total time: " + (DateTime.Now - start));
			}
			catch (Exception outerEx)
			{
				Console.WriteLine(outerEx.Message + " ST: " + outerEx.StackTrace);
			}

			Console.WriteLine();
			Console.WriteLine();
		}

		public static void ClientThread(object obj)
		{
			TTransport transport = (TTransport)obj;
			for (int i = 0; i < numIterations; i++)
			{
				ClientTest(transport);
			}
			transport.Close();
		}

		public static void ClientTest(TTransport transport)
		{
			TBinaryProtocol binaryProtocol = new TBinaryProtocol(transport);

			ThriftTest.Client client = new ThriftTest.Client(binaryProtocol);
			try
			{
				if (!transport.IsOpen)
				{
					transport.Open();
				}
			}
			catch (TTransportException ttx)
			{
				Console.WriteLine("Connect failed: " + ttx.Message);
				return;
			}

			long start = DateTime.Now.ToFileTime();

			Console.Write("testVoid()");
			client.testVoid();
			Console.WriteLine(" = void");

			Console.Write("testString(\"Test\")");
			string s = client.testString("Test");
			Console.WriteLine(" = \"" + s + "\"");

			Console.Write("testByte(1)");
			sbyte i8 = client.testByte((sbyte)1);
			Console.WriteLine(" = " + i8);

			Console.Write("testI32(-1)");
			int i32 = client.testI32(-1);
			Console.WriteLine(" = " + i32);

			Console.Write("testI64(-34359738368)");
			long i64 = client.testI64(-34359738368);
			Console.WriteLine(" = " + i64);

			Console.Write("testDouble(5.325098235)");
			double dub = client.testDouble(5.325098235);
			Console.WriteLine(" = " + dub);

			Console.Write("testStruct({\"Zero\", 1, -3, -5})");
			Xtruct o = new Xtruct();
			o.String_thing = "Zero";
			o.Byte_thing = (sbyte)1;
			o.I32_thing = -3;
			o.I64_thing = -5;
			Xtruct i = client.testStruct(o);
			Console.WriteLine(" = {\"" + i.String_thing + "\", " + i.Byte_thing + ", " + i.I32_thing + ", " + i.I64_thing + "}");

			Console.Write("testNest({1, {\"Zero\", 1, -3, -5}, 5})");
			Xtruct2 o2 = new Xtruct2();
			o2.Byte_thing = (sbyte)1;
			o2.Struct_thing = o;
			o2.I32_thing = 5;
			Xtruct2 i2 = client.testNest(o2);
			i = i2.Struct_thing;
			Console.WriteLine(" = {" + i2.Byte_thing + ", {\"" + i.String_thing + "\", " + i.Byte_thing + ", " + i.I32_thing + ", " + i.I64_thing + "}, " + i2.I32_thing + "}");

			Dictionary<int, int> mapout = new Dictionary<int, int>();
			for (int j = 0; j < 5; j++)
			{
				mapout[j] = j - 10;
			}
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

			Dictionary<int, int> mapin = client.testMap(mapout);

			Console.Write(" = {");
			first = true;
			foreach (int key in mapin.Keys)
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

			List<int> listout = new List<int>();
			for (int j = -2; j < 3; j++)
			{
				listout.Add(j);
			}
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

			List<int> listin = client.testList(listout);

			Console.Write(" = {");
			first = true;
			foreach (int j in listin)
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
			THashSet<int> setout = new THashSet<int>();
			for (int j = -2; j < 3; j++)
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

			THashSet<int> setin = client.testSet(setout);

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
			Numberz ret = client.testEnum(Numberz.ONE);
			Console.WriteLine(" = " + ret);

			Console.Write("testEnum(TWO)");
			ret = client.testEnum(Numberz.TWO);
			Console.WriteLine(" = " + ret);

			Console.Write("testEnum(THREE)");
			ret = client.testEnum(Numberz.THREE);
			Console.WriteLine(" = " + ret);

			Console.Write("testEnum(FIVE)");
			ret = client.testEnum(Numberz.FIVE);
			Console.WriteLine(" = " + ret);

			Console.Write("testEnum(EIGHT)");
			ret = client.testEnum(Numberz.EIGHT);
			Console.WriteLine(" = " + ret);

			Console.Write("testTypedef(309858235082523)");
			long uid = client.testTypedef(309858235082523L);
			Console.WriteLine(" = " + uid);

			Console.Write("testMapMap(1)");
			Dictionary<int, Dictionary<int, int>> mm = client.testMapMap(1);
			Console.Write(" = {");
			foreach (int key in mm.Keys)
			{
				Console.Write(key + " => {");
				Dictionary<int, int> m2 = mm[key];
				foreach (int k2 in m2.Keys)
				{
					Console.Write(k2 + " => " + m2[k2] + ", ");
				}
				Console.Write("}, ");
			}
			Console.WriteLine("}");

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
			Console.Write("testInsanity()");
			Dictionary<long, Dictionary<Numberz, Insanity>> whoa = client.testInsanity(insane);
			Console.Write(" = {");
			foreach (long key in whoa.Keys)
			{
				Dictionary<Numberz, Insanity> val = whoa[key];
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


			sbyte arg0 = 1;
			int arg1 = 2;
			long arg2 = long.MaxValue;
			Dictionary<short, string> multiDict = new Dictionary<short, string>();
			multiDict[1] = "one";
			Numberz arg4 = Numberz.FIVE;
			long arg5 = 5000000;
			Console.Write("Test Multi(" + arg0 + "," + arg1 + "," + arg2 + "," + multiDict + "," + arg4 + "," + arg5 + ")");
			Xtruct multiResponse = client.testMulti(arg0, arg1, arg2, multiDict, arg4, arg5);
			Console.Write(" = Xtruct(byte_thing:" + multiResponse.Byte_thing + ",String_thing:" + multiResponse.String_thing
						+ ",i32_thing:" + multiResponse.I32_thing + ",i64_thing:" + multiResponse.I64_thing + ")\n");

			Console.WriteLine("Test Oneway(1)");
			client.testOneway(1);

			Console.Write("Test Calltime()");
			var startt = DateTime.UtcNow;
			for ( int k=0; k<1000; ++k )
				client.testVoid();
			Console.WriteLine(" = " + (DateTime.UtcNow - startt).TotalSeconds.ToString() + " ms a testVoid() call" );
		}
	}
}
