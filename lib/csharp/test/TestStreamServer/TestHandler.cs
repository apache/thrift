﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
            //Console.WriteLine("testVoid()");
        }

        public string testString(string thing)
        {
            //Console.WriteLine("teststring(\"" + thing + "\")");
            return thing;
        }

        public sbyte testByte(sbyte thing)
        {
            //Console.WriteLine("testByte(" + thing + ")");
            return thing;
        }

        public int testI32(int thing)
        {
            //Console.WriteLine("testI32(" + thing + ")");
            return thing;
        }

        public long testI64(long thing)
        {
            //Console.WriteLine("testI64(" + thing + ")");
            return thing;
        }

        public double testDouble(double thing)
        {
            //Console.WriteLine("testDouble(" + thing + ")");
            return thing;
        }

        public byte[] testBinary(byte[] thing)
        {
            string hex = BitConverter.ToString(thing).Replace("-", string.Empty);
            //Console.WriteLine("testBinary(" + hex + ")");
            return thing;
        }

        public Xtruct testStruct(Xtruct thing)
        {
            /*Console.WriteLine("testStruct({" +
                             "\"" + thing.String_thing + "\", " +
                             thing.Byte_thing + ", " +
                             thing.I32_thing + ", " +
                             thing.I64_thing + "})");*/
            return thing;
        }

        public Xtruct2 testNest(Xtruct2 nest)
        {
            Xtruct thing = nest.Struct_thing;
            /*Console.WriteLine("testNest({" +
                             nest.Byte_thing + ", {" +
                             "\"" + thing.String_thing + "\", " +
                             thing.Byte_thing + ", " +
                             thing.I32_thing + ", " +
                             thing.I64_thing + "}, " +
                             nest.I32_thing + "})");*/
            return nest;
        }

        public Dictionary<int, int> testMap(Dictionary<int, int> thing)
        {
            //Console.WriteLine("testMap({");
            bool first = true;
            foreach (int key in thing.Keys)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    //Console.WriteLine(", ");
                }
                //Console.WriteLine(key + " => " + thing[key]);
            }
            //Console.WriteLine("})");
            return thing;
        }

        public Dictionary<string, string> testStringMap(Dictionary<string, string> thing)
        {
            //Console.WriteLine("testStringMap({");
            bool first = true;
            foreach (string key in thing.Keys)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    //Console.WriteLine(", ");
                }
                //Console.WriteLine(key + " => " + thing[key]);
            }
            //Console.WriteLine("})");
            return thing;
        }

        public THashSet<int> testSet(THashSet<int> thing)
        {
            //Console.WriteLine("testSet({");
            bool first = true;
            foreach (int elem in thing)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    //Console.WriteLine(", ");
                }
                //Console.WriteLine(elem);
            }
            //Console.WriteLine("})");
            return thing;
        }

        public List<int> testList(List<int> thing)
        {
            //Console.WriteLine("testList({");
            bool first = true;
            foreach (int elem in thing)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    //Console.WriteLine(", ");
                }
                //Console.WriteLine(elem);
            }
            //Console.WriteLine("})");
            return thing;
        }

        public Numberz testEnum(Numberz thing)
        {
            //Console.WriteLine("testEnum(" + thing + ")");
            return thing;
        }

        public long testTypedef(long thing)
        {
            //Console.WriteLine("testTypedef(" + thing + ")");
            return thing;
        }

        public Dictionary<int, Dictionary<int, int>> testMapMap(int hello)
        {
            //Console.WriteLine("testMapMap(" + hello + ")");
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
            //Console.WriteLine("testInsanity()");

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
            //Console.WriteLine("testMulti()");

            Xtruct hello = new Xtruct(); ;
            hello.String_thing = "Hello2";
            hello.Byte_thing = arg0;
            hello.I32_thing = arg1;
            hello.I64_thing = arg2;
            return hello;
        }

        /**
         * Print 'testException(%s)' with arg as '%s'
         * @param string arg - a string indication what type of exception to throw
         * if arg == "Xception" throw Xception with errorCode = 1001 and message = arg
         * elsen if arg == "TException" throw TException
         * else do not throw anything
         */
        public void testException(string arg)
        {
            //Console.WriteLine("testException(" + arg + ")");
            if (arg == "Xception")
            {
                Xception x = new Xception();
                x.ErrorCode = 1001;
                x.Message = arg;
                throw x;
            }
            if (arg == "TException")
            {
                throw new Thrift.TException();
            }
            return;
        }

        public Xtruct testMultiException(string arg0, string arg1)
        {
            //Console.WriteLine("testMultiException(" + arg0 + ", " + arg1 + ")");
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
            //Console.WriteLine("testOneway(" + arg + "), sleeping...");
            System.Threading.Thread.Sleep(arg * 1000);
            //Console.WriteLine("testOneway finished");
        }
    }

}
