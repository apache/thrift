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
using System.Linq;
using System.Reflection;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Transport;

namespace Thrift.Tests.Protocols
{
    /// <summary>
    /// Pins the public shape of generated clients. The default generator output (Cassandra is generated without
    /// the <c>isolated</c> option) must keep its shared-protocol API, while the <c>isolated</c> output must
    /// expose the call-context API only.
    /// </summary>
    [TestClass]
    public class TGeneratedClientShapeTests
    {
        [TestMethod]
        public void Default_Generated_Client_Keeps_Legacy_Constructors_And_Signatures()
        {
            var client = typeof(Apache.Cassandra.Test.Cassandra.Client);

            var ctors = client.GetConstructors().Select(c => c.GetParameters().Select(p => p.ParameterType).ToArray()).ToList();
            Assert.AreEqual(2, ctors.Count);
            Assert.IsTrue(ctors.Any(p => p.SequenceEqual(new[] { typeof(TProtocol) })));
            Assert.IsTrue(ctors.Any(p => p.SequenceEqual(new[] { typeof(TProtocol), typeof(TProtocol) })));

            foreach (var m in client.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly))
            {
                Assert.IsFalse(m.GetParameters().Any(p => p.ParameterType == typeof(TCallContext)),
                    $"{m.Name} must not take a TCallContext in default mode");
            }
        }

        private static MethodInfo[] Overloads(string name) =>
            typeof(IsolatedCall.IsolatedService.Client).GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly)
                .Where(m => m.Name == name).ToArray();

        [TestMethod]
        public void Isolated_Generated_Client_Is_Dual_Mode()
        {
            var ctors = typeof(IsolatedCall.IsolatedService.Client).GetConstructors()
                .Select(c => c.GetParameters().Select(p => p.ParameterType).ToArray()).ToList();

            Assert.AreEqual(3, ctors.Count);
            Assert.IsTrue(ctors.Any(p => p.SequenceEqual(new[] { typeof(TProtocol) })));
            Assert.IsTrue(ctors.Any(p => p.SequenceEqual(new[] { typeof(TProtocol), typeof(TProtocol) })));
            Assert.IsTrue(ctors.Any(p => p.SequenceEqual(new[] { typeof(ICallIsolatedTransport), typeof(TProtocolFactory) })));
        }

        [TestMethod]
        public void Split_Operations_Keep_Their_Signature_And_Gain_A_Context_Overload()
        {
            foreach (var name in new[] { "send_add", "recv_add" })
            {
                var overloads = Overloads(name);
                Assert.AreEqual(2, overloads.Length, name);

                var legacy = overloads.Single(m => m.GetParameters().All(p => p.ParameterType != typeof(TCallContext)));
                var withContext = overloads.Single(m => m.GetParameters()[0].ParameterType == typeof(TCallContext));

                // the shared-protocol operation still ends with its cancellation token
                Assert.AreEqual(typeof(System.Threading.CancellationToken), legacy.GetParameters().Last().ParameterType, name);
                // the context carries the cancellation token, so the overload takes none of its own
                Assert.IsTrue(withContext.GetParameters().All(p => p.ParameterType != typeof(System.Threading.CancellationToken)), name);
            }

            // the high-level operations keep their public signature
            var add = typeof(IsolatedCall.IsolatedService.Client).GetMethod("add")!;
            CollectionAssert.AreEqual(
                new[] { typeof(int), typeof(int), typeof(System.Threading.CancellationToken) },
                add.GetParameters().Select(p => p.ParameterType).ToArray());
        }

        [TestMethod]
        public void Oneway_Operation_Has_Both_Send_Overloads_And_No_Receive_Method()
        {
            Assert.AreEqual(2, Overloads("send_fire").Length);
            Assert.AreEqual(0, Overloads("recv_fire").Length);
        }
    }
}
