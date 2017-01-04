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
    public partial class VersioningTestV1 : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private int _begin_in_both;
        private int _end_in_both;
        private string _old_string;

        public VersioningTestV1()
        {
        }

        [DataMember(Order = 0)]
        public int Begin_in_both
        {
            get { return _begin_in_both; }
            set
            {
                __isset.begin_in_both = true;
                this._begin_in_both = value;
            }
        }

        [DataMember(Order = 0)]
        public string Old_string
        {
            get { return _old_string; }
            set
            {
                __isset.old_string = true;
                this._old_string = value;
            }
        }

        [DataMember(Order = 0)]
        public int End_in_both
        {
            get { return _end_in_both; }
            set
            {
                __isset.end_in_both = true;
                this._end_in_both = value;
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
                                Begin_in_both = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.String)
                            {
                                Old_string = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 12:
                            if (field.Type == TType.I32)
                            {
                                End_in_both = await iprot.ReadI32Async(cancellationToken);
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
                var struc = new TStruct("VersioningTestV1");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (__isset.begin_in_both)
                {
                    field.Name = "begin_in_both";
                    field.Type = TType.I32;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Begin_in_both, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Old_string != null && __isset.old_string)
                {
                    field.Name = "old_string";
                    field.Type = TType.String;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Old_string, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.end_in_both)
                {
                    field.Name = "end_in_both";
                    field.Type = TType.I32;
                    field.ID = 12;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(End_in_both, cancellationToken);
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
            var sb = new StringBuilder("VersioningTestV1(");
            bool __first = true;
            if (__isset.begin_in_both)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Begin_in_both: ");
                sb.Append(Begin_in_both);
            }
            if (Old_string != null && __isset.old_string)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Old_string: ");
                sb.Append(Old_string);
            }
            if (__isset.end_in_both)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("End_in_both: ");
                sb.Append(End_in_both);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool begin_in_both;
            [DataMember] public bool old_string;
            [DataMember] public bool end_in_both;
        }

        #region XmlSerializer support

        public bool ShouldSerializeBegin_in_both()
        {
            return __isset.begin_in_both;
        }

        public bool ShouldSerializeOld_string()
        {
            return __isset.old_string;
        }

        public bool ShouldSerializeEnd_in_both()
        {
            return __isset.end_in_both;
        }

        #endregion XmlSerializer support
    }
}