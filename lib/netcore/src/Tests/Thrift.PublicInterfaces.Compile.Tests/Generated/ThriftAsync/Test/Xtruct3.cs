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


namespace ThriftAsync.Test
{
    [DataContract(Namespace = "")]
    public partial class Xtruct3 : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private int _changed;
        private int _i32_thing;
        private long _i64_thing;
        private string _string_thing;

        public Xtruct3()
        {
        }

        [DataMember(Order = 0)]
        public string String_thing
        {
            get { return _string_thing; }
            set
            {
                __isset.string_thing = true;
                this._string_thing = value;
            }
        }

        [DataMember(Order = 0)]
        public int Changed
        {
            get { return _changed; }
            set
            {
                __isset.changed = true;
                this._changed = value;
            }
        }

        [DataMember(Order = 0)]
        public int I32_thing
        {
            get { return _i32_thing; }
            set
            {
                __isset.i32_thing = true;
                this._i32_thing = value;
            }
        }

        [DataMember(Order = 0)]
        public long I64_thing
        {
            get { return _i64_thing; }
            set
            {
                __isset.i64_thing = true;
                this._i64_thing = value;
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
                            if (field.Type == TType.String)
                            {
                                String_thing = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.I32)
                            {
                                Changed = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 9:
                            if (field.Type == TType.I32)
                            {
                                I32_thing = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 11:
                            if (field.Type == TType.I64)
                            {
                                I64_thing = await iprot.ReadI64Async(cancellationToken);
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
                var struc = new TStruct("Xtruct3");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (String_thing != null && __isset.string_thing)
                {
                    field.Name = "string_thing";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(String_thing, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.changed)
                {
                    field.Name = "changed";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Changed, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.i32_thing)
                {
                    field.Name = "i32_thing";
                    field.Type = TType.I32;
                    field.ID = 9;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(I32_thing, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.i64_thing)
                {
                    field.Name = "i64_thing";
                    field.Type = TType.I64;
                    field.ID = 11;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI64Async(I64_thing, cancellationToken);
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
            var sb = new StringBuilder("Xtruct3(");
            bool __first = true;
            if (String_thing != null && __isset.string_thing)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("String_thing: ");
                sb.Append(String_thing);
            }
            if (__isset.changed)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Changed: ");
                sb.Append(Changed);
            }
            if (__isset.i32_thing)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("I32_thing: ");
                sb.Append(I32_thing);
            }
            if (__isset.i64_thing)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("I64_thing: ");
                sb.Append(I64_thing);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool string_thing;
            [DataMember] public bool changed;
            [DataMember] public bool i32_thing;
            [DataMember] public bool i64_thing;
        }

        #region XmlSerializer support

        public bool ShouldSerializeString_thing()
        {
            return __isset.string_thing;
        }

        public bool ShouldSerializeChanged()
        {
            return __isset.changed;
        }

        public bool ShouldSerializeI32_thing()
        {
            return __isset.i32_thing;
        }

        public bool ShouldSerializeI64_thing()
        {
            return __isset.i64_thing;
        }

        #endregion XmlSerializer support
    }
}