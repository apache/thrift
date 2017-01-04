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
    public partial class GuessProtocolStruct : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private Dictionary<string, string> _map_field;

        public GuessProtocolStruct()
        {
        }

        [DataMember(Order = 0)]
        public Dictionary<string, string> Map_field
        {
            get { return _map_field; }
            set
            {
                __isset.map_field = true;
                this._map_field = value;
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
                        case 7:
                            if (field.Type == TType.Map)
                            {
                                {
                                    Map_field = new Dictionary<string, string>();
                                    TMap _map65 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i66 = 0; _i66 < _map65.Count; ++_i66)
                                    {
                                        string _key67;
                                        string _val68;
                                        _key67 = await iprot.ReadStringAsync(cancellationToken);
                                        _val68 = await iprot.ReadStringAsync(cancellationToken);
                                        Map_field[_key67] = _val68;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
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
                var struc = new TStruct("GuessProtocolStruct");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Map_field != null && __isset.map_field)
                {
                    field.Name = "map_field";
                    field.Type = TType.Map;
                    field.ID = 7;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteMapBeginAsync(new TMap(TType.String, TType.String, Map_field.Count),
                                cancellationToken);
                        foreach (string _iter69 in Map_field.Keys)
                        {
                            await oprot.WriteStringAsync(_iter69, cancellationToken);
                            await oprot.WriteStringAsync(Map_field[_iter69], cancellationToken);
                        }
                        await oprot.WriteMapEndAsync(cancellationToken);
                    }
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

        #region XmlSerializer support

        public bool ShouldSerializeMap_field()
        {
            return __isset.map_field;
        }

        #endregion XmlSerializer support

        public override string ToString()
        {
            var sb = new StringBuilder("GuessProtocolStruct(");
            bool __first = true;
            if (Map_field != null && __isset.map_field)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Map_field: ");
                sb.Append(Map_field);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool map_field;
        }
    }
}