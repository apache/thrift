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
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using OptReqDefTest;
using Thrift.Collections;

namespace Thrift.Tests.DataModel
{
    // ReSharper disable once InconsistentNaming
    [TestClass]
    public class DeepCopyTests
    {
        [TestMethod]
        public void Test_Complex_DeepCopy()
        {
            var first = InitializeInstance(new RaceDetails());
            VerifyIdenticalContent(first, InitializeInstance(new RaceDetails()));

            var second = first.DeepCopy();
            VerifyIdenticalContent(first, second);
            ModifyInstance(second,0);
            VerifyDifferentContent(first, second);
            VerifyIdenticalContent(first, InitializeInstance(new RaceDetails()));

            var third = second.DeepCopy();
            VerifyIdenticalContent(second, third);
            ModifyInstance(third,0);
            VerifyDifferentContent(second, third);
            VerifyIdenticalContent(first, InitializeInstance(new RaceDetails()));
        }

        private RaceDetails MakeNestedRaceDetails(int nesting)
        {
            if (++nesting > 1)
                return null;

            var instance = new RaceDetails();
            InitializeInstance(instance,nesting);
            return instance;
        }

        private jack MakeNestedUnion(int nesting)
        {
            if (++nesting > 1)
                return null;

            var details = new RaceDetails();
            InitializeInstance(details,nesting);
            return new jack.nested_struct(details);
        }


        private RaceDetails InitializeInstance(RaceDetails instance, int nesting = 0)
        {
            // at init, we intentionally leave all non-required fields unset
            Assert.IsFalse(instance.__isset.opt_one);
            Assert.IsFalse(instance.__isset.opt_two);
            Assert.IsFalse(instance.__isset.opt_three);
            Assert.IsFalse(instance.__isset.opt_four);
            Assert.IsFalse(instance.__isset.opt_five);
            Assert.IsFalse(instance.__isset.opt_six);
            Assert.IsFalse(instance.__isset.opt_seven);
            Assert.IsFalse(instance.__isset.opt_eight);

            // set all required to null/default
            instance.Req_one = default;
            instance.Req_two = default;
            instance.Req_three = default;
            instance.Req_four = default;
            instance.Req_five = default;
            instance.Req_six = default; 
            instance.Req_seven = default;;
            instance.Req_eight = default; 

            // leave non-required fields unset again
            Assert.IsFalse(instance.__isset.def_one);
            Assert.IsFalse(instance.__isset.def_two);
            Assert.IsFalse(instance.__isset.def_three);
            Assert.IsFalse(instance.__isset.def_four);
            Assert.IsFalse(instance.__isset.def_five);
            Assert.IsFalse(instance.__isset.def_six);
            Assert.IsFalse(instance.__isset.def_seven);
            Assert.IsFalse(instance.__isset.def_eight);

            // these should have IDL defaults set

            Assert.IsTrue(instance.__isset.opt_one_with_value);
            Assert.IsTrue(instance.__isset.opt_two_with_value);
            Assert.IsTrue(instance.__isset.opt_three_with_value);
            Assert.IsTrue(instance.__isset.opt_four_with_value);
            Assert.IsTrue(instance.__isset.opt_five_with_value);
            Assert.IsTrue(instance.__isset.opt_six_with_value);
            Assert.IsTrue(instance.__isset.opt_seven_with_value);
            Assert.IsTrue(instance.__isset.opt_eight_with_value);

            Assert.AreEqual(instance.Req_one_with_value, (Distance)1);
            Assert.AreEqual(instance.Req_two_with_value, 2.22);
            Assert.AreEqual(instance.Req_three_with_value, 3);
            Assert.AreEqual(instance.Req_four_with_value, "four");
            Assert.AreEqual("five", Encoding.UTF8.GetString(instance.Req_five_with_value));

            Assert.IsTrue(instance.Req_six_with_value.Count == 1);
            Assert.AreEqual(instance.Req_six_with_value[0], 6 );

            Assert.IsTrue(instance.Req_seven_with_value.Count == 1);
            Assert.IsTrue(instance.Req_seven_with_value.Contains(7));

            Assert.IsTrue(instance.Req_eight_with_value.Count == 1);
            Assert.IsTrue(instance.Req_eight_with_value[8] == 8);

            Assert.IsTrue(instance.__isset.def_one_with_value);
            Assert.IsTrue(instance.__isset.def_two_with_value);
            Assert.IsTrue(instance.__isset.def_three_with_value);
            Assert.IsTrue(instance.__isset.def_four_with_value);
            Assert.IsTrue(instance.__isset.def_five_with_value);
            Assert.IsTrue(instance.__isset.def_six_with_value);
            Assert.IsTrue(instance.__isset.def_seven_with_value);
            Assert.IsTrue(instance.__isset.def_eight_with_value);

            instance.Last_of_the_mohicans = true;

            if (nesting < 2)
            {
                instance.Far_list = new List<Distance>() { Distance.foo, Distance.bar, Distance.baz };
                instance.Far_set = new THashSet<Distance>() { Distance.foo, Distance.bar, Distance.baz };
                instance.Far_map = new Dictionary<Distance, Distance>() { [Distance.foo] = Distance.foo, [Distance.bar] = Distance.bar, [Distance.baz] = Distance.baz };

                instance.Far_set_list = new THashSet<List<Distance>>() { new List<Distance>() { Distance.foo } };
                instance.Far_list_map_set = new List<Dictionary<sbyte, THashSet<Distance>>>() { new Dictionary<sbyte, THashSet<Distance>>() { [1] = new THashSet<Distance>() { Distance.baz } } };
                instance.Far_map_dist_to_rds = new Dictionary<Distance, List<RaceDetails>>() { [Distance.bar] = new List<RaceDetails>() { MakeNestedRaceDetails(nesting) } };

                instance.Req_nested = MakeNestedRaceDetails(nesting);
                Assert.IsFalse(instance.__isset.opt_nested);
                Assert.IsFalse(instance.__isset.def_nested);

                instance.Req_union = MakeNestedUnion(nesting);
                Assert.IsFalse(instance.__isset.opt_union);
                Assert.IsFalse(instance.__isset.def_union);
            }

            instance.Triplesix = (Distance)666;

            return instance;
        }

        private void ModifyInstance(RaceDetails instance, int level)
        {
            if ((instance == null) || (++level > 4))
                return;

            instance.Opt_one = ModifyValue(instance.Opt_one);
            instance.Opt_two = ModifyValue(instance.Opt_two);
            instance.Opt_three = ModifyValue(instance.Opt_three);
            instance.Opt_four = ModifyValue(instance.Opt_four);
            instance.Opt_five = ModifyValue(instance.Opt_five);
            instance.Opt_six = ModifyValue(instance.Opt_six);
            instance.Opt_seven = ModifyValue(instance.Opt_seven);
            instance.Opt_eight = ModifyValue(instance.Opt_eight);

            instance.Req_one = ModifyValue(instance.Req_one);
            instance.Req_two = ModifyValue(instance.Req_two);
            instance.Req_three = ModifyValue(instance.Req_three);
            instance.Req_four = ModifyValue(instance.Req_four);
            instance.Req_five = ModifyValue(instance.Req_five);
            instance.Req_six = ModifyValue(instance.Req_six);
            instance.Req_seven = ModifyValue(instance.Req_seven);
            instance.Req_eight = ModifyValue(instance.Req_eight);

            instance.Def_one = ModifyValue(instance.Def_one);
            instance.Def_two = ModifyValue(instance.Def_two);
            instance.Def_three = ModifyValue(instance.Def_three);
            instance.Def_four = ModifyValue(instance.Def_four);
            instance.Def_five = ModifyValue(instance.Def_five);
            instance.Def_six = ModifyValue(instance.Def_six);
            instance.Def_seven = ModifyValue(instance.Def_seven);
            instance.Def_eight = ModifyValue(instance.Def_eight);

            instance.Opt_one_with_value = ModifyValue(instance.Opt_one_with_value);
            instance.Opt_two_with_value = ModifyValue(instance.Opt_two_with_value);
            instance.Opt_three_with_value = ModifyValue(instance.Opt_three_with_value);
            instance.Opt_four_with_value = ModifyValue(instance.Opt_four_with_value);
            instance.Opt_five_with_value = ModifyValue(instance.Opt_five_with_value);
            instance.Opt_six_with_value = ModifyValue(instance.Opt_six_with_value);
            instance.Opt_seven_with_value = ModifyValue(instance.Opt_seven_with_value);
            instance.Opt_eight_with_value = ModifyValue(instance.Opt_eight_with_value);

            instance.Req_one_with_value = ModifyValue(instance.Req_one_with_value);
            instance.Req_two_with_value = ModifyValue(instance.Req_two_with_value);
            instance.Req_three_with_value = ModifyValue(instance.Req_three_with_value);
            instance.Req_four_with_value = ModifyValue(instance.Req_four_with_value);
            instance.Req_five_with_value = ModifyValue(instance.Req_five_with_value);
            instance.Req_six_with_value = ModifyValue(instance.Req_six_with_value);
            instance.Req_seven_with_value = ModifyValue(instance.Req_seven_with_value);
            instance.Req_eight_with_value = ModifyValue(instance.Req_eight_with_value);

            instance.Def_one_with_value = ModifyValue(instance.Def_one_with_value);
            instance.Def_two_with_value = ModifyValue(instance.Def_two_with_value);
            instance.Def_three_with_value = ModifyValue(instance.Def_three_with_value);
            instance.Def_four_with_value = ModifyValue(instance.Def_four_with_value);
            instance.Def_five_with_value = ModifyValue(instance.Def_five_with_value);
            instance.Def_six_with_value = ModifyValue(instance.Def_six_with_value);
            instance.Def_seven_with_value = ModifyValue(instance.Def_seven_with_value);
            instance.Def_eight_with_value = ModifyValue(instance.Def_eight_with_value);

            instance.Last_of_the_mohicans = ModifyValue(instance.Last_of_the_mohicans);

            instance.Far_list = ModifyValue(instance.Far_list);
            instance.Far_set = ModifyValue(instance.Far_set);
            instance.Far_map = ModifyValue(instance.Far_map);

            instance.Far_set_list = ModifyValue(instance.Far_set_list);
            instance.Far_list_map_set = ModifyValue(instance.Far_list_map_set);
            instance.Far_map_dist_to_rds = ModifyValue(instance.Far_map_dist_to_rds, level);

            instance.Req_nested = ModifyValue(instance.Req_nested, level);
            instance.Opt_nested = ModifyValue(instance.Opt_nested, level);
            instance.Def_nested = ModifyValue(instance.Def_nested, level);

            instance.Req_union = ModifyValue(instance.Req_union, level);
            instance.Opt_union = ModifyValue(instance.Opt_union, level);
            instance.Def_union = ModifyValue(instance.Def_union, level);

            instance.Triplesix = ModifyValue(instance.Triplesix);
        }

        private jack ModifyValue(jack value, int level)
        {
            if (++level > 4)
                return value;

            if (value == null)
                value = MakeNestedUnion(0);
            Debug.Assert(value.As_nested_struct != null);
            ModifyInstance(value.As_nested_struct, level);
            return value;
        }

        private RaceDetails ModifyValue(RaceDetails value, int level)
        {
            if (++level > 4)
                return value;

            if (value == null)
                value = new RaceDetails();
            ModifyInstance(value,level);
            return value;
        }

        private Dictionary<Distance, List<RaceDetails>> ModifyValue(Dictionary<Distance, List<RaceDetails>> value, int level)
        {
            if (value == null)
                value = new Dictionary<Distance, List<RaceDetails>>();

            if (++level > 4)
                return value;

            var details = new RaceDetails();
            InitializeInstance(details);
            value[Distance.foo] = new List<RaceDetails>() { details };

            if (value.TryGetValue(Distance.bar, out var list) && (list.Count > 0))
            {
                ModifyInstance(list[0], level);
                list.Add(null);
            }

            value[Distance.baz] = null;

            return value;
        }

        private List<Dictionary<sbyte, THashSet<Distance>>> ModifyValue(List<Dictionary<sbyte, THashSet<Distance>>> value)
        {
            if (value == null)
                value = new List<Dictionary<sbyte, THashSet<Distance>>>();

            if (value.Count == 0)
                value.Add(new Dictionary<sbyte, THashSet<Distance>>());
            else
                value.Add(null);

            sbyte key = (sbyte)(value[0].Count + 10);
            if (value[0].Count == 0)
                value[0].Add(key, new THashSet<Distance>());
            else
                value[0].Add(key, null);

            foreach (var entry in value)
            {
                if (entry != null)
                {
                    foreach (var pair in entry)
                    {
                        if (pair.Value != null)
                        {
                            if (pair.Value.Contains(Distance.baz))
                                pair.Value.Remove(Distance.baz);
                            else
                                pair.Value.Add(Distance.baz);
                        }
                    }
                }
            }

            return value;
        }

        private THashSet<List<Distance>> ModifyValue(THashSet<List<Distance>> value)
        {
            if (value == null)
                value = new THashSet<List<Distance>>();

            if (value.Count == 0)
                value.Add(new List<Distance>());
            else
                value.Add(null);

            foreach (var entry in value)
                if( entry != null)
                    entry.Add(Distance.baz);

            return value;
        }

        private Dictionary<Distance, Distance> ModifyValue(Dictionary<Distance, Distance> value)
        {
            if (value == null)
                value = new Dictionary<Distance, Distance>();
            value[Distance.foo] = value.ContainsKey(Distance.foo) ? ++value[Distance.foo] : Distance.foo;
            value[Distance.bar] = value.ContainsKey(Distance.bar) ? ++value[Distance.bar] : Distance.bar;
            value[Distance.baz] = value.ContainsKey(Distance.baz) ? ++value[Distance.baz] : Distance.baz;
            return value;
        }

        private THashSet<Distance> ModifyValue(THashSet<Distance> value)
        {
            if (value == null)
                value = new THashSet<Distance>();

            if (value.Contains(Distance.foo))
                value.Remove(Distance.foo);
            else
                value.Add(Distance.foo);

            if (value.Contains(Distance.bar))
                value.Remove(Distance.bar);
            else
                value.Add(Distance.bar);

            if (value.Contains(Distance.baz))
                value.Remove(Distance.baz);
            else
                value.Add(Distance.baz);

            return value;
        }

        private List<Distance> ModifyValue(List<Distance> value)
        {
            if (value == null)
                value = new List<Distance>();
            value.Add(Distance.foo);
            value.Add(Distance.bar);
            value.Add(Distance.baz);
            return value;
        }

        private bool ModifyValue(bool value)
        {
            return !value;
        }

        private Dictionary<sbyte, short> ModifyValue(Dictionary<sbyte, short> value)
        {
            if (value == null)
                value = new Dictionary<sbyte, short>();
            value.Add((sbyte)(value.Count + 10), (short)value.Count);
            return value;
        }

        private THashSet<long> ModifyValue(THashSet<long> value)
        {
            if (value == null)
                value = new THashSet<long>();
            value.Add(value.Count+100);
            return value;
        }

        private List<int> ModifyValue(List<int> value)
        {
            if (value == null)
                value = new List<int>();
            value.Add(value.Count);
            return value;
        }

        private byte[] ModifyValue(byte[] value)
        {
            if (value == null)
                value = new byte[1] { 0 };
            if (value.Length > 0)
                value[0] = (value[0] < 0xFF) ? ++value[0] : (byte)0;
            return value;
        }

        private string ModifyValue(string value)
        {
            return value + "1";
        }

        private double ModifyValue(double value)
        {
            return value + 1.1;
        }

        private short ModifyValue(short value)
        {
            return ++value;
        }

        private Distance ModifyValue(Distance value)
        {
            return ++value;
        }

        private void VerifyDifferentContent(RaceDetails first, RaceDetails second)
        {
            Assert.AreNotEqual(first, second);

            Assert.AreNotEqual(first.Opt_two, second.Opt_two);
            Assert.AreNotEqual(first.Opt_three, second.Opt_three);
            Assert.AreNotEqual(first.Opt_four, second.Opt_four);
            Assert.IsFalse(TCollections.Equals(first.Opt_five, second.Opt_five));
            Assert.IsFalse(TCollections.Equals(first.Opt_six, second.Opt_six));
            Assert.IsFalse(TCollections.Equals(first.Opt_seven, second.Opt_seven));
            Assert.IsFalse(TCollections.Equals(first.Opt_eight, second.Opt_eight));

            Assert.AreNotEqual(first.Req_one, second.Req_one);
            Assert.AreNotEqual(first.Req_two, second.Req_two);
            Assert.AreNotEqual(first.Req_three, second.Req_three);
            Assert.AreNotEqual(first.Req_four, second.Req_four);
            Assert.IsFalse(TCollections.Equals(first.Req_five, second.Req_five));
            Assert.IsFalse(TCollections.Equals(first.Req_six, second.Req_six));
            Assert.IsFalse(TCollections.Equals(first.Req_seven, second.Req_seven));
            Assert.IsFalse(TCollections.Equals(first.Req_eight, second.Req_eight));

            Assert.AreNotEqual(first.Def_one, second.Def_one);
            Assert.AreNotEqual(first.Def_two, second.Def_two);
            Assert.AreNotEqual(first.Def_three, second.Def_three);
            Assert.AreNotEqual(first.Def_four, second.Def_four);
            Assert.IsFalse(TCollections.Equals(first.Def_five, second.Def_five));
            Assert.IsFalse(TCollections.Equals(first.Def_six, second.Def_six));
            Assert.IsFalse(TCollections.Equals(first.Def_seven, second.Def_seven));
            Assert.IsFalse(TCollections.Equals(first.Def_eight, second.Def_eight));

            Assert.AreNotEqual(first.Opt_one_with_value, second.Opt_one_with_value);
            Assert.AreNotEqual(first.Opt_two_with_value, second.Opt_two_with_value);
            Assert.AreNotEqual(first.Opt_three_with_value, second.Opt_three_with_value);
            Assert.AreNotEqual(first.Opt_four_with_value, second.Opt_four_with_value);
            Assert.IsFalse(TCollections.Equals(first.Opt_five_with_value, second.Opt_five_with_value));
            Assert.IsFalse(TCollections.Equals(first.Opt_six_with_value, second.Opt_six_with_value));
            Assert.IsFalse(TCollections.Equals(first.Opt_seven_with_value, second.Opt_seven_with_value));
            Assert.IsFalse(TCollections.Equals(first.Opt_eight_with_value, second.Opt_eight_with_value));

            Assert.AreNotEqual(first.Req_one_with_value, second.Req_one_with_value);
            Assert.AreNotEqual(first.Req_two_with_value, second.Req_two_with_value);
            Assert.AreNotEqual(first.Req_three_with_value, second.Req_three_with_value);
            Assert.AreNotEqual(first.Req_four_with_value, second.Req_four_with_value);
            Assert.IsFalse(TCollections.Equals(first.Req_five_with_value, second.Req_five_with_value));
            Assert.IsFalse(TCollections.Equals(first.Req_six_with_value, second.Req_six_with_value));
            Assert.IsFalse(TCollections.Equals(first.Req_seven_with_value, second.Req_seven_with_value));
            Assert.IsFalse(TCollections.Equals(first.Req_eight_with_value, second.Req_eight_with_value));

            Assert.AreNotEqual(first.Def_one_with_value, second.Def_one_with_value);
            Assert.AreNotEqual(first.Def_two_with_value, second.Def_two_with_value);
            Assert.AreNotEqual(first.Def_three_with_value, second.Def_three_with_value);
            Assert.AreNotEqual(first.Def_four_with_value, second.Def_four_with_value);
            Assert.IsFalse(TCollections.Equals(first.Def_five_with_value, second.Def_five_with_value));
            Assert.IsFalse(TCollections.Equals(first.Def_six_with_value, second.Def_six_with_value));
            Assert.IsFalse(TCollections.Equals(first.Def_seven_with_value, second.Def_seven_with_value));
            Assert.IsFalse(TCollections.Equals(first.Def_eight_with_value, second.Def_eight_with_value));

            Assert.AreNotEqual(first.Last_of_the_mohicans, second.Last_of_the_mohicans);

            Assert.IsFalse(TCollections.Equals(first.Far_list, second.Far_list));
            Assert.IsFalse(TCollections.Equals(first.Far_set, second.Far_set));
            Assert.IsFalse(TCollections.Equals(first.Far_map, second.Far_map));

            Assert.IsFalse(TCollections.Equals(first.Far_set_list, second.Far_set_list));
            Assert.IsFalse(TCollections.Equals(first.Far_list_map_set, second.Far_list_map_set));
            Assert.IsFalse(TCollections.Equals(first.Far_map_dist_to_rds, second.Far_map_dist_to_rds));

            Assert.AreNotEqual(first.Req_nested, second.Req_nested);
            Assert.AreNotEqual(first.Opt_nested, second.Opt_nested);
            Assert.AreNotEqual(first.Def_nested, second.Def_nested);

            Assert.AreNotEqual(first.Req_union, second.Req_union);
            Assert.AreNotEqual(first.Opt_union, second.Opt_union);
            Assert.AreNotEqual(first.Def_union, second.Def_union);

            Assert.AreNotEqual(first.Triplesix, second.Triplesix);
        }

        private void VerifyIdenticalContent(RaceDetails first, RaceDetails second)
        {
            Assert.AreEqual(first, second);

            Assert.AreEqual(first.Opt_two, second.Opt_two);
            Assert.AreEqual(first.Opt_three, second.Opt_three);
            Assert.AreEqual(first.Opt_four, second.Opt_four);
            Assert.IsTrue(TCollections.Equals(first.Opt_five, second.Opt_five));
            Assert.IsTrue(TCollections.Equals(first.Opt_six, second.Opt_six));
            Assert.IsTrue(TCollections.Equals(first.Opt_seven, second.Opt_seven));
            Assert.IsTrue(TCollections.Equals(first.Opt_eight, second.Opt_eight));

            Assert.AreEqual(first.Req_one, second.Req_one);
            Assert.AreEqual(first.Req_two, second.Req_two);
            Assert.AreEqual(first.Req_three, second.Req_three);
            Assert.AreEqual(first.Req_four, second.Req_four);
            Assert.IsTrue(TCollections.Equals(first.Req_five, second.Req_five));
            Assert.IsTrue(TCollections.Equals(first.Req_six, second.Req_six));
            Assert.IsTrue(TCollections.Equals(first.Req_seven, second.Req_seven));
            Assert.IsTrue(TCollections.Equals(first.Req_eight, second.Req_eight));

            Assert.AreEqual(first.Def_one, second.Def_one);
            Assert.AreEqual(first.Def_two, second.Def_two);
            Assert.AreEqual(first.Def_three, second.Def_three);
            Assert.AreEqual(first.Def_four, second.Def_four);
            Assert.IsTrue(TCollections.Equals(first.Def_five, second.Def_five));
            Assert.IsTrue(TCollections.Equals(first.Def_six, second.Def_six));
            Assert.IsTrue(TCollections.Equals(first.Def_seven, second.Def_seven));
            Assert.IsTrue(TCollections.Equals(first.Def_eight, second.Def_eight));

            Assert.AreEqual(first.Opt_one_with_value, second.Opt_one_with_value);
            Assert.AreEqual(first.Opt_two_with_value, second.Opt_two_with_value);
            Assert.AreEqual(first.Opt_three_with_value, second.Opt_three_with_value);
            Assert.AreEqual(first.Opt_four_with_value, second.Opt_four_with_value);
            Assert.IsTrue(TCollections.Equals(first.Opt_five_with_value, second.Opt_five_with_value));
            Assert.IsTrue(TCollections.Equals(first.Opt_six_with_value, second.Opt_six_with_value));
            Assert.IsTrue(TCollections.Equals(first.Opt_seven_with_value, second.Opt_seven_with_value));
            Assert.IsTrue(TCollections.Equals(first.Opt_eight_with_value, second.Opt_eight_with_value));

            Assert.AreEqual(first.Req_one_with_value, second.Req_one_with_value);
            Assert.AreEqual(first.Req_two_with_value, second.Req_two_with_value);
            Assert.AreEqual(first.Req_three_with_value, second.Req_three_with_value);
            Assert.AreEqual(first.Req_four_with_value, second.Req_four_with_value);
            Assert.IsTrue(TCollections.Equals(first.Req_five_with_value, second.Req_five_with_value));
            Assert.IsTrue(TCollections.Equals(first.Req_six_with_value, second.Req_six_with_value));
            Assert.IsTrue(TCollections.Equals(first.Req_seven_with_value, second.Req_seven_with_value));
            Assert.IsTrue(TCollections.Equals(first.Req_eight_with_value, second.Req_eight_with_value));

            Assert.AreEqual(first.Def_one_with_value, second.Def_one_with_value);
            Assert.AreEqual(first.Def_two_with_value, second.Def_two_with_value);
            Assert.AreEqual(first.Def_three_with_value, second.Def_three_with_value);
            Assert.AreEqual(first.Def_four_with_value, second.Def_four_with_value);
            Assert.IsTrue(TCollections.Equals(first.Def_five_with_value, second.Def_five_with_value));
            Assert.IsTrue(TCollections.Equals(first.Def_six_with_value, second.Def_six_with_value));
            Assert.IsTrue(TCollections.Equals(first.Def_seven_with_value, second.Def_seven_with_value));
            Assert.IsTrue(TCollections.Equals(first.Def_eight_with_value, second.Def_eight_with_value));

            Assert.AreEqual(first.Last_of_the_mohicans, second.Last_of_the_mohicans);

            Assert.IsTrue(TCollections.Equals(first.Far_list, second.Far_list));
            Assert.IsTrue(TCollections.Equals(first.Far_set, second.Far_set));
            Assert.IsTrue(TCollections.Equals(first.Far_map, second.Far_map));

            Assert.IsTrue(TCollections.Equals(first.Far_set_list, second.Far_set_list));
            Assert.IsTrue(TCollections.Equals(first.Far_list_map_set, second.Far_list_map_set));
            Assert.IsTrue(TCollections.Equals(first.Far_map_dist_to_rds, second.Far_map_dist_to_rds));

            Assert.AreEqual(first.Req_nested, second.Req_nested);
            Assert.AreEqual(first.Opt_nested, second.Opt_nested);
            Assert.AreEqual(first.Def_nested, second.Def_nested);

            Assert.AreEqual(first.Req_union, second.Req_union);
            Assert.AreEqual(first.Opt_union, second.Opt_union);
            Assert.AreEqual(first.Def_union, second.Def_union);

            Assert.AreEqual(first.Triplesix, second.Triplesix);
        }

    }
}
