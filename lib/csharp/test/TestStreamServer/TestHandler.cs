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
using Thrift.Server;
using Thrift.Collections;

namespace Thrift.Test
{
    public class TestHandler : ThriftTest.Iface
    {
        public TServer server;

        public TestHandler() { }

        public void testVoid()
        {

        }

        public string testString(string thing)
        {
            return thing;
        }

        public sbyte testByte(sbyte thing)
        {
            return thing;
        }

        public int testI32(int thing)
        {
            return thing;
        }

        public long testI64(long thing)
        {
            return thing;
        }

        public double testDouble(double thing)
        {
            return thing;
        }

        public byte[] testBinary(byte[] thing)
        {
            string hex = BitConverter.ToString(thing).Replace("-", string.Empty);
            return thing;
        }

        public Xtruct testStruct(Xtruct thing)
        {
            return thing;
        }

        public Xtruct2 testNest(Xtruct2 nest)
        {
            Xtruct thing = nest.Struct_thing;
            return nest;
        }

        public Dictionary<int, int> testMap(Dictionary<int, int> thing)
        {
            bool first = true;
            foreach (int key in thing.Keys)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                }
            }
            return thing;
        }

        public Dictionary<string, string> testStringMap(Dictionary<string, string> thing)
        {
            bool first = true;
            foreach (string key in thing.Keys)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                }
            }
            return thing;
        }

        public THashSet<int> testSet(THashSet<int> thing)
        {
            bool first = true;
            foreach (int elem in thing)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                }
            }
            return thing;
        }

        public List<int> testList(List<int> thing)
        {
            bool first = true;
            foreach (int elem in thing)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                }
            }
            return thing;
        }

        public Numberz testEnum(Numberz thing)
        {
            return thing;
        }

        public long testTypedef(long thing)
        {
            return thing;
        }

        public Dictionary<int, Dictionary<int, int>> testMapMap(int hello)
        {
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

            return mapmap;
        }

        public Dictionary<long, Dictionary<Numberz, Insanity>> testInsanity(Insanity argument)
        {

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

            return insane;
        }

        public Xtruct testMulti(sbyte arg0, int arg1, long arg2, Dictionary<short, string> arg3, Numberz arg4, long arg5)
        {
            Xtruct hello = new Xtruct(); ;
            hello.String_thing = "Hello2";
            hello.Byte_thing = arg0;
            hello.I32_thing = arg1;
            hello.I64_thing = arg2;
            return hello;
        }

        public void testException(string arg)
        {
            if (arg == "Xception")
            {
                Xception x = new Xception();
                x.ErrorCode = 1001;
                x.Message = arg;
                throw x;
            }
            if (arg == "TException")
            {
                throw new Thrift.TException("TException");
            }
            return;
        }

        public Xtruct testMultiException(string arg0, string arg1)
        {
            if (arg0 == "Xception")
            {
                Xception x = new Xception();
                x.ErrorCode = 1001;
                x.Message = "This is an Xception";
                throw x;
            }
            else if (arg0 == "Xception2")
            {
                Xception2 x = new Xception2();
                x.ErrorCode = 2002;
                x.Struct_thing = new Xtruct();
                x.Struct_thing.String_thing = "This is an Xception2";
                throw x;
            }

            Xtruct result = new Xtruct();
            result.String_thing = arg1;
            return result;
        }

        public void testStop()
        {
            if (server != null)
            {
                server.Stop();
            }
        }

        public void testOneway(int arg)
        {
            System.Threading.Thread.Sleep(arg * 1000);
        }
    }

}
