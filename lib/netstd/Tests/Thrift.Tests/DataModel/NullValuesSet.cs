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

#pragma warning disable IDE0079 // net20 - unneeded suppression
#pragma warning disable IDE0017  // init can be simplified - we don't want that here

namespace Thrift.Tests.DataModel
{
    // ReSharper disable once InconsistentNaming
    [TestClass]
    public class Thrift_5238
    {
        private static void CheckInstance(RaceDetails instance)
        {
            // object
            Assert.IsTrue(instance.__isset.def_nested);
            Assert.IsTrue(instance.__isset.opt_nested);
            Assert.IsNull(instance.Def_nested);
            Assert.IsNull(instance.Opt_nested);

            // string
            Assert.IsTrue(instance.__isset.def_four);
            Assert.IsTrue(instance.__isset.opt_four);
            Assert.IsTrue(string.IsNullOrEmpty(instance.Req_four));
            Assert.IsNull(instance.Def_four);
            Assert.IsNull(instance.Opt_four);

            // list<>
            Assert.IsTrue(instance.__isset.def_six);
            Assert.IsTrue(instance.__isset.opt_six);
            Assert.IsNull(instance.Req_six);
            Assert.IsNull(instance.Opt_six);
            Assert.IsNull(instance.Def_six);

            // byte[]
            Assert.IsTrue(instance.__isset.def_nine);
            Assert.IsTrue(instance.__isset.opt_nine);
            Assert.IsTrue((instance.Req_nine == null) || (instance.Req_nine.Length == 0));
            Assert.IsNull(instance.Def_nine);
            Assert.IsNull(instance.Opt_nine);
        }

        [TestMethod]
        public void Thrift_5238_ProperNullChecks()
        {
            var instance = new OptReqDefTest.RaceDetails();

            // the following code INTENTIONALLY assigns null to non.nullable reftypes
            #pragma warning disable CS8625

            // object
            instance.Def_nested = null;
            instance.Opt_nested = null;

            // string
            instance.Req_four = null;
            instance.Def_four = null;
            instance.Opt_four = null;

            // list<>
            instance.Req_six = null;
            instance.Opt_six = null;
            instance.Def_six = null;

            // byte[]
            instance.Req_nine = null;
            instance.Def_nine = null;
            instance.Opt_nine = null;

            // back to normal
            #pragma warning restore CS8625

            // test the setup
            CheckInstance(instance);

            // validate proper null checks, any of these throws if not
            instance.ToString();
            instance.GetHashCode();

            // validate proper null checks, any of these throws if not
            var copy = instance.DeepCopy();
            CheckInstance(copy);
        }

    }
}
