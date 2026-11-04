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
using System.Collections;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift4833;
using ThriftTest;
using ErrorData = Thrift5320.exceptions.ErrorData;
using TaskException = Thrift5320.exceptions.Task;

namespace Thrift.Tests.DataModel
{
    // THRIFT-4833: a field whose name is that of an inherited member hides that member. The generated
    // class returns the field, while the inherited member stays what it is, seen through the base class.
    [TestClass]
    public class InheritedMemberNamesTests
    {
        [TestMethod]
        public void FieldNamedMessage_HidesExceptionMessage()
        {
            var xception = new Xception { ErrorCode = 1001, Message = "This is an Xception" };
            Assert.AreEqual("This is an Xception", xception.Message);

            Exception exception = xception;
            Assert.AreEqual(((Exception)new Xception()).Message, exception.Message);
            Assert.AreNotEqual(xception.Message, exception.Message);
        }

        [TestMethod]
        public void FieldNamedData_HidesExceptionData()
        {
            var data = new ErrorData { Messitsch = "details" };
            var task = new TaskException { Data = data };
            Assert.AreSame(data, task.Data);

            Exception exception = task;
            Assert.IsInstanceOfType<IDictionary>(exception.Data);
        }

        [TestMethod]
        public void FieldsNamedAfterInheritedMembers_HideThem()
        {
            var names = new InheritedNames
            {
                Message = "message",
                Data = "data",
                Source = "source",
                HelpLink = "helpLink",
                StackTrace = "stackTrace",
                InnerException = "innerException",
                HResult = 42,
                TargetSite = "targetSite",
                GetBaseException = "getBaseException",
                GetObjectData = "getObjectData",
                GetType = "getType",
                SerializeObjectState = "serializeObjectState",
                MemberwiseClone = "memberwiseClone",
                ReferenceEquals = "referenceEquals",
                Finalize = "finalize",
            };

            Exception thrown;
            try
            {
                throw names;
            }
            catch (InheritedNames caught)
            {
                thrown = caught;
            }
            Assert.AreSame(names, thrown);

            // the fields
            Assert.AreEqual("message", names.Message);
            Assert.AreEqual("data", names.Data);
            Assert.AreEqual("source", names.Source);
            Assert.AreEqual("helpLink", names.HelpLink);
            Assert.AreEqual("stackTrace", names.StackTrace);
            Assert.AreEqual("innerException", names.InnerException);
            Assert.AreEqual(42, names.HResult);
            Assert.AreEqual("targetSite", names.TargetSite);
            Assert.AreEqual("getBaseException", names.GetBaseException);
            Assert.AreEqual("getObjectData", names.GetObjectData);
            Assert.AreEqual("getType", names.GetType);
            Assert.AreEqual("serializeObjectState", names.SerializeObjectState);
            Assert.AreEqual("memberwiseClone", names.MemberwiseClone);
            Assert.AreEqual("referenceEquals", names.ReferenceEquals);
            Assert.AreEqual("finalize", names.Finalize);

            // the inherited members
            Assert.AreEqual(((Exception)new InheritedNames()).Message, thrown.Message);
            Assert.IsInstanceOfType<IDictionary>(thrown.Data);
            Assert.AreEqual(typeof(InheritedMemberNamesTests).Assembly.GetName().Name, thrown.Source);
            Assert.IsNull(thrown.HelpLink);
            Assert.IsNotNull(thrown.StackTrace);
            Assert.Contains(nameof(FieldsNamedAfterInheritedMembers_HideThem), thrown.StackTrace);
            Assert.IsNull(thrown.InnerException);
            Assert.AreEqual(new Exception().HResult, thrown.HResult);
            Assert.AreEqual(nameof(FieldsNamedAfterInheritedMembers_HideThem), thrown.TargetSite!.Name);
            Assert.AreSame(thrown, thrown.GetBaseException());
            Assert.AreEqual(typeof(InheritedNames), thrown.GetType());
        }

        [TestMethod]
        public void StructFieldsNamedAfterObjectMembers_HideThem()
        {
            var names = new ObjectNames { GetType = "getType", MemberwiseClone = "memberwiseClone", ReferenceEquals = "referenceEquals" };
            Assert.AreEqual("getType", names.GetType);
            Assert.AreEqual("memberwiseClone", names.MemberwiseClone);
            Assert.AreEqual("referenceEquals", names.ReferenceEquals);
            Assert.AreEqual(typeof(ObjectNames), ((object)names).GetType());
        }
    }
}
