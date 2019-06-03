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
using System.Text;
using ThriftTest;
using Thrift.Collections;

namespace Client.Tests
{
    
    static class TestDataFactory
    {
        public static CrazyNesting CreateCrazyNesting(int count = 10)
        {
            if (count <= 0)
                return null;

            return new CrazyNesting()
            {
                Binary_field = CreateBytesArray(count),
                List_field = CreateListField(count),
                Set_field = CreateSetField(count),
                String_field = string.Format("data level {0}", count)
            };
        }

        private static THashSet<Insanity> CreateSetField(int count)
        {
            var retval = new THashSet<Insanity>();
            for (var i = 0; i < count; ++i)
                retval.Add(CreateInsanity(count));
            return retval;
        }

        private static Insanity CreateInsanity(int count)
        {
            return new Insanity()
            {
                UserMap = CreateUserMap(count),
                Xtructs = CreateXtructs(count)
            };
        }

        private static List<Xtruct> CreateXtructs(int count)
        {
            var retval = new List<Xtruct>();
            for (var i = 0; i < count; ++i)
                retval.Add(CreateXtruct(count));
            return retval;
        }

        private static Xtruct CreateXtruct(int count)
        {
            return new Xtruct()
            {
                Byte_thing = (sbyte)(count % 128),
                I32_thing = count,
                I64_thing = count,
                String_thing = string.Format("data level {0}", count)
            };
        }

        private static Dictionary<Numberz, long> CreateUserMap(int count)
        {
            var retval = new Dictionary<Numberz, long>();
            retval.Add(Numberz.ONE, count);
            retval.Add(Numberz.TWO, count);
            retval.Add(Numberz.THREE, count);
            retval.Add(Numberz.FIVE, count);
            retval.Add(Numberz.SIX, count);
            retval.Add(Numberz.EIGHT, count);
            return retval;
        }

        private static List<Dictionary<THashSet<int>, Dictionary<int, THashSet<List<Dictionary<Insanity, string>>>>>> CreateListField(int count)
        {
            var retval = new List<Dictionary<THashSet<int>, Dictionary<int, THashSet<List<Dictionary<Insanity, string>>>>>>();
            for (var i = 0; i < count; ++i)
                retval.Add(CreateListFieldData(count));
            return retval;
        }

        private static Dictionary<THashSet<int>, Dictionary<int, THashSet<List<Dictionary<Insanity, string>>>>> CreateListFieldData(int count)
        {
            var retval = new Dictionary<THashSet<int>, Dictionary<int, THashSet<List<Dictionary<Insanity, string>>>>>();
            for (var i = 0; i < count; ++i)
                retval.Add( CreateIntHashSet(count), CreateListFieldDataDict(count));
            return retval;
        }

        private static THashSet<int> CreateIntHashSet(int count)
        {
            var retval = new THashSet<int>();
            for (var i = 0; i < count; ++i)
                retval.Add(i);
            return retval;
        }

        private static Dictionary<int, THashSet<List<Dictionary<Insanity, string>>>> CreateListFieldDataDict(int count)
        {
            var retval = new Dictionary<int, THashSet<List<Dictionary<Insanity, string>>>>();
            for (var i = 0; i < count; ++i)
                retval.Add(i, CreateListFieldDataDictValue(count));
            return retval;
        }

        private static THashSet<List<Dictionary<Insanity, string>>> CreateListFieldDataDictValue(int count)
        {
            var retval = new THashSet<List<Dictionary<Insanity, string>>>();
            for (var i = 0; i < count; ++i)
                retval.Add( CreateListFieldDataDictValueList(count));
            return retval;
        }

        private static List<Dictionary<Insanity, string>> CreateListFieldDataDictValueList(int count)
        {
            var retval = new List<Dictionary<Insanity, string>>();
            for (var i = 0; i < count; ++i)
                retval.Add(CreateListFieldDataDictValueListDict(count));
            return retval;
        }

        private static Dictionary<Insanity, string> CreateListFieldDataDictValueListDict(int count)
        {
            var retval = new Dictionary<Insanity, string>();
            retval.Add(CreateInsanity(count), string.Format("data level {0}", count));
            return retval;
        }

        private static byte[] CreateBytesArray(int count)
        {
            var retval = new byte[count];
            for (var i = 0; i < count; ++i)
                retval[i] = (byte)(i % 0xFF);
            return retval;
        }
    }
}
