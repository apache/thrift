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
using System.Threading;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;
using NamesIdl = IsolatedNames;

namespace Thrift.Tests.Protocols
{
    /// <summary>
    /// The IDL of the fixture uses names that look like the names of the generated isolated client: a type
    /// <c>TCallContext</c>, methods <c>IsCallIsolated</c> and <c>CreateCallContextAsync</c>, and parameters
    /// <c>callContext</c>, <c>callContext_</c> and <c>timeoutCause_</c>. Compiling the generated code is the first
    /// half of the test; the second half is that the operations keep the parameter names of the IDL.
    /// </summary>
    [TestClass]
    public class TIsolatedGeneratedNamesTests
    {
        private static readonly Type ClientType = typeof(NamesIdl.Names.Client);

        private static MethodInfo[] Overloads(string name) =>
            ClientType.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly)
                .Where(m => m.Name == name).ToArray();

        [TestMethod]
        public void Shared_Protocol_And_High_Level_Operations_Keep_The_Parameter_Names_Of_The_Idl()
        {
            var expected = new[] { "callContext", "callContext_", "timeoutCause_" };

            var highLevel = Overloads("collide").Single();
            CollectionAssert.AreEqual(expected.Concat(new[] { "cancellationToken" }).ToArray(),
                highLevel.GetParameters().Select(p => p.Name).ToArray());

            var legacySend = Overloads("send_collide")
                .Single(m => m.GetParameters().All(p => p.ParameterType != typeof(TCallContext)));
            CollectionAssert.AreEqual(expected.Concat(new[] { "cancellationToken" }).ToArray(),
                legacySend.GetParameters().Select(p => p.Name).ToArray());
        }

        [TestMethod]
        public void The_Context_Parameter_Does_Not_Collide_With_A_Parameter_Of_The_Idl()
        {
            var withContext = Overloads("send_collide")
                .Single(m => m.GetParameters().Any(p => p.ParameterType == typeof(TCallContext)));
            var names = withContext.GetParameters().Select(p => p.Name).ToArray();

            Assert.AreEqual(typeof(TCallContext), withContext.GetParameters()[0].ParameterType, "the runtime type, not the type of the IDL");
            Assert.AreEqual(names.Length, names.Distinct().Count(), "duplicate parameter names: " + string.Join(", ", names));
            CollectionAssert.IsSubsetOf(new[] { "callContext", "callContext_", "timeoutCause_" }, names);
        }

        [TestMethod]
        public void Service_Methods_Named_Like_Client_Members_Do_Not_Replace_Them()
        {
            using var client = new NamesIdl.Names.Client(new TMemoryBufferTransport(null), new TBinaryProtocol.Factory());

            // the property of the client base class still works, next to the methods of the service
            Assert.IsTrue(((TBaseClient)client).IsCallIsolated);
            Assert.IsNotNull(ClientType.GetMethod("IsCallIsolated", new[] { typeof(int), typeof(CancellationToken) }));
            Assert.IsNotNull(ClientType.GetMethod("CreateCallContextAsync", new[] { typeof(int), typeof(CancellationToken) }));
        }

        [TestMethod]
        public async System.Threading.Tasks.Task A_Child_Client_Does_Not_Bind_To_Members_Of_The_Parent_Client()
        {
            // "base" of the child client is the parent client, which declares methods with these names; compiling
            // the child is the first half, and the child must still start calls through the client base class
            using var client = new NamesIdl.Child.Client(new TMemoryBufferTransport(null), new TBinaryProtocol.Factory());
            Assert.IsTrue(((TBaseClient)client).IsCallIsolated);

            using var cts = new CancellationTokenSource();
            cts.Cancel();
            await Assert.ThrowsAsync<OperationCanceledException>(() => client.ping(1, cts.Token));
        }
    }
}
