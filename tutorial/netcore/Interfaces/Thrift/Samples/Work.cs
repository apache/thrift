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
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Thrift;
using Thrift.Collections;
using System.ServiceModel;
using System.Runtime.Serialization;
using Thrift.Protocols;
using Thrift.Protocols.Entities;
using Thrift.Protocols.Utilities;
using Thrift.Transports;
using Thrift.Transports.Client;
using Thrift.Transports.Server;


namespace Thrift.Samples
{
    /// <summary>
    ///     Structs are the basic complex data structures. They are comprised of fields
    ///     which each have an integer identifier, a type, a symbolic name, and an
    ///     optional default value.
    ///     Fields can be declared "optional", which ensures they will not be included
    ///     in the serialized output if they aren't set.  Note that this requires some
    ///     manual management in some languages.
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class Work : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private string _Comment;
        private int _Num1;
        private int _Num2;
        private Operation _Op;

        public Work()
        {
            this._Num1 = 0;
            this.__isset.Num1 = true;
        }

        [DataMember(Order = 0)]
        public int Num1
        {
            get { return _Num1; }
            set
            {
                __isset.Num1 = true;
                this._Num1 = value;
            }
        }

        [DataMember(Order = 0)]
        public int Num2
        {
            get { return _Num2; }
            set
            {
                __isset.Num2 = true;
                this._Num2 = value;
            }
        }

        /// <summary>
        ///     <seealso cref="Operation" />
        /// </summary>
        [DataMember(Order = 0)]
        public Operation Op
        {
            get { return _Op; }
            set
            {
                __isset.Op = true;
                this._Op = value;
            }
        }

        [DataMember(Order = 0)]
        public string Comment
        {
            get { return _Comment; }
            set
            {
                __isset.Comment = true;
                this._Comment = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                TField field;
                await iprot.ReadStructBeginAsync(cancellationToken);
                while (true)
                {
                    field = await iprot.ReadFieldBeginAsync(cancellationToken);
                    if (field.Type == TType.Stop)
                    {
                        break;
                    }

                    switch (field.ID)
                    {
                        case 1:
                            if (field.Type == TType.I32)
                            {
                                Num1 = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.I32)
                            {
                                Num2 = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.I32)
                            {
                                Op = (Operation) await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.String)
                            {
                                Comment = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        default:
                            await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            break;
                    }

                    await iprot.ReadFieldEndAsync(cancellationToken);
                }

                await iprot.ReadStructEndAsync(cancellationToken);
            }
            finally
            {
                iprot.DecrementRecursionDepth();
            }
        }

        public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
        {
            oprot.IncrementRecursionDepth();
            try
            {
                var struc = new TStruct("Work");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (__isset.Num1)
                {
                    field.Name = "Num1";
                    field.Type = TType.I32;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Num1, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.Num2)
                {
                    field.Name = "Num2";
                    field.Type = TType.I32;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Num2, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.Op)
                {
                    field.Name = "Op";
                    field.Type = TType.I32;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Op, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Comment != null && __isset.Comment)
                {
                    field.Name = "Comment";
                    field.Type = TType.String;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Comment, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                await oprot.WriteFieldStopAsync(cancellationToken);
                await oprot.WriteStructEndAsync(cancellationToken);
            }
            finally
            {
                oprot.DecrementRecursionDepth();
            }
        }

        public override string ToString()
        {
            var sb = new StringBuilder("Work(");
            bool __first = true;
            if (__isset.Num1)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Num1: ");
                sb.Append(Num1);
            }
            if (__isset.Num2)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Num2: ");
                sb.Append(Num2);
            }
            if (__isset.Op)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Op: ");
                sb.Append(Op);
            }
            if (Comment != null && __isset.Comment)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Comment: ");
                sb.Append(Comment);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool Num1;
            [DataMember] public bool Num2;
            [DataMember] public bool Op;
            [DataMember] public bool Comment;
        }

        #region XmlSerializer support

        public bool ShouldSerializeNum1()
        {
            return __isset.Num1;
        }

        public bool ShouldSerializeNum2()
        {
            return __isset.Num2;
        }

        public bool ShouldSerializeOp()
        {
            return __isset.Op;
        }

        public bool ShouldSerializeComment()
        {
            return __isset.Comment;
        }

        #endregion XmlSerializer support
    }
}