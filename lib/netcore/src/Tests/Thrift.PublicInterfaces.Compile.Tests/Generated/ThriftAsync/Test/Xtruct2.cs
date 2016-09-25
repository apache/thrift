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
    public partial class Xtruct2 : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private sbyte _byte_thing;
        private int _i32_thing;
        private Xtruct _struct_thing;

        public Xtruct2()
        {
        }

        [DataMember(Order = 0)]
        public sbyte Byte_thing
        {
            get { return _byte_thing; }
            set
            {
                __isset.byte_thing = true;
                this._byte_thing = value;
            }
        }

        [DataMember(Order = 0)]
        public Xtruct Struct_thing
        {
            get { return _struct_thing; }
            set
            {
                __isset.struct_thing = true;
                this._struct_thing = value;
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
                            if (field.Type == TType.Byte)
                            {
                                Byte_thing = await iprot.ReadByteAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.Struct)
                            {
                                Struct_thing = new Xtruct();
                                await Struct_thing.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.I32)
                            {
                                I32_thing = await iprot.ReadI32Async(cancellationToken);
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
                var struc = new TStruct("Xtruct2");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (__isset.byte_thing)
                {
                    field.Name = "byte_thing";
                    field.Type = TType.Byte;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteByteAsync(Byte_thing, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Struct_thing != null && __isset.struct_thing)
                {
                    field.Name = "struct_thing";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Struct_thing.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.i32_thing)
                {
                    field.Name = "i32_thing";
                    field.Type = TType.I32;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(I32_thing, cancellationToken);
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
            var sb = new StringBuilder("Xtruct2(");
            bool __first = true;
            if (__isset.byte_thing)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Byte_thing: ");
                sb.Append(Byte_thing);
            }
            if (Struct_thing != null && __isset.struct_thing)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Struct_thing: ");
                sb.Append(Struct_thing == null ? "<null>" : Struct_thing.ToString());
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
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool byte_thing;
            [DataMember] public bool struct_thing;
            [DataMember] public bool i32_thing;
        }

        #region XmlSerializer support

        public bool ShouldSerializeByte_thing()
        {
            return __isset.byte_thing;
        }

        public bool ShouldSerializeStruct_thing()
        {
            return __isset.struct_thing;
        }

        public bool ShouldSerializeI32_thing()
        {
            return __isset.i32_thing;
        }

        #endregion XmlSerializer support
    }
}