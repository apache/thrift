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


namespace Apache.Cassandra.Test
{
    [DataContract(Namespace = "")]
    public partial class CqlMetadata : TBase
    {
        public CqlMetadata()
        {
        }

        public CqlMetadata(Dictionary<byte[], string> name_types, Dictionary<byte[], string> value_types,
            string default_name_type, string default_value_type) : this()
        {
            this.Name_types = name_types;
            this.Value_types = value_types;
            this.Default_name_type = default_name_type;
            this.Default_value_type = default_value_type;
        }

        [DataMember(Order = 0)]
        public Dictionary<byte[], string> Name_types { get; set; }

        [DataMember(Order = 0)]
        public Dictionary<byte[], string> Value_types { get; set; }

        [DataMember(Order = 0)]
        public string Default_name_type { get; set; }

        [DataMember(Order = 0)]
        public string Default_value_type { get; set; }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_name_types = false;
                bool isset_value_types = false;
                bool isset_default_name_type = false;
                bool isset_default_value_type = false;
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
                            if (field.Type == TType.Map)
                            {
                                {
                                    Name_types = new Dictionary<byte[], string>();
                                    TMap _map69 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i70 = 0; _i70 < _map69.Count; ++_i70)
                                    {
                                        byte[] _key71;
                                        string _val72;
                                        _key71 = await iprot.ReadBinaryAsync(cancellationToken);
                                        _val72 = await iprot.ReadStringAsync(cancellationToken);
                                        Name_types[_key71] = _val72;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
                                isset_name_types = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.Map)
                            {
                                {
                                    Value_types = new Dictionary<byte[], string>();
                                    TMap _map73 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i74 = 0; _i74 < _map73.Count; ++_i74)
                                    {
                                        byte[] _key75;
                                        string _val76;
                                        _key75 = await iprot.ReadBinaryAsync(cancellationToken);
                                        _val76 = await iprot.ReadStringAsync(cancellationToken);
                                        Value_types[_key75] = _val76;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
                                isset_value_types = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.String)
                            {
                                Default_name_type = await iprot.ReadStringAsync(cancellationToken);
                                isset_default_name_type = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.String)
                            {
                                Default_value_type = await iprot.ReadStringAsync(cancellationToken);
                                isset_default_value_type = true;
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
                if (!isset_name_types)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_value_types)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_default_name_type)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_default_value_type)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
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
                var struc = new TStruct("CqlMetadata");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "name_types";
                field.Type = TType.Map;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                {
                    await
                        oprot.WriteMapBeginAsync(new TMap(TType.String, TType.String, Name_types.Count),
                            cancellationToken);
                    foreach (byte[] _iter77 in Name_types.Keys)
                    {
                        await oprot.WriteBinaryAsync(_iter77, cancellationToken);
                        await oprot.WriteStringAsync(Name_types[_iter77], cancellationToken);
                    }
                    await oprot.WriteMapEndAsync(cancellationToken);
                }
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "value_types";
                field.Type = TType.Map;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                {
                    await
                        oprot.WriteMapBeginAsync(new TMap(TType.String, TType.String, Value_types.Count),
                            cancellationToken);
                    foreach (byte[] _iter78 in Value_types.Keys)
                    {
                        await oprot.WriteBinaryAsync(_iter78, cancellationToken);
                        await oprot.WriteStringAsync(Value_types[_iter78], cancellationToken);
                    }
                    await oprot.WriteMapEndAsync(cancellationToken);
                }
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "default_name_type";
                field.Type = TType.String;
                field.ID = 3;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Default_name_type, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "default_value_type";
                field.Type = TType.String;
                field.ID = 4;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Default_value_type, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
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
            var sb = new StringBuilder("CqlMetadata(");
            sb.Append(", Name_types: ");
            sb.Append(Name_types);
            sb.Append(", Value_types: ");
            sb.Append(Value_types);
            sb.Append(", Default_name_type: ");
            sb.Append(Default_name_type);
            sb.Append(", Default_value_type: ");
            sb.Append(Default_value_type);
            sb.Append(")");
            return sb.ToString();
        }
    }
}