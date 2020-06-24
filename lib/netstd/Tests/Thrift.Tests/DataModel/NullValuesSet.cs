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
using Microsoft.VisualStudio.TestPlatform.ObjectModel;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using OptReqDefTest;
using Thrift.Collections;

namespace Thrift.Tests.DataModel
{
    // ReSharper disable once InconsistentNaming
    [TestClass]
    public class Thrift_5238
    {
        private void CheckInstance(RaceDetails instance)
        {
            // object
            Assert.IsTrue(instance.__isset.def_nested);
            Assert.IsTrue(instance.__isset.opt_nested);
            Assert.IsNull(instance.Def_nested);
            Assert.IsNull(instance.Opt_nested);

            // string
            Assert.IsTrue(instance.__isset.def_four);
            Assert.IsTrue(instance.__isset.opt_four);
            Assert.IsNull(instance.Req_four);
            Assert.IsNull(instance.Def_four);
            Assert.IsNull(instance.Opt_four);

            // byte[]
            Assert.IsTrue(instance.__isset.def_five);
            Assert.IsTrue(instance.__isset.opt_five);
            Assert.IsNull(instance.Req_five);
            Assert.IsNull(instance.Def_five);
            Assert.IsNull(instance.Opt_five);

            // list<>
            Assert.IsTrue(instance.__isset.def_six);
            Assert.IsTrue(instance.__isset.opt_six);
            Assert.IsNull(instance.Req_six);
            Assert.IsNull(instance.Opt_six);
            Assert.IsNull(instance.Def_six);
        }

        [TestMethod]
        public void Thrift_5238_ProperNullChecks()
        {
            var instance = new OptReqDefTest.RaceDetails();

            // object
            instance.Def_nested = null;
            instance.Opt_nested = null;

            // string
            instance.Req_four = null;
            instance.Def_four = null;
            instance.Opt_four = null;

            // byte[]
            instance.Req_five = null;
            instance.Def_five = null;
            instance.Opt_five = null;

            // list<>
            instance.Req_six = null;
            instance.Opt_six = null;
            instance.Def_six = null;

            // test the setup
            CheckInstance(instance);

            // validate proper null checks , any of these throws if not
            instance.ToString();
            instance.GetHashCode();

            // validate proper null checks , any of these throws if not
            var copy = instance.DeepCopy();
            CheckInstance(copy);
        }

    }
}
