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
    public partial class ListBonks : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private List<Bonk> _bonk;

        public ListBonks()
        {
        }

        [DataMember(Order = 0)]
        public List<Bonk> Bonk
        {
            get { return _bonk; }
            set
            {
                __isset.bonk = true;
                this._bonk = value;
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
                            if (field.Type == TType.List)
                            {
                                {
                                    Bonk = new List<Bonk>();
                                    TList _list128 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i129 = 0; _i129 < _list128.Count; ++_i129)
                                    {
                                        Bonk _elem130;
                                        _elem130 = new Bonk();
                                        await _elem130.ReadAsync(iprot, cancellationToken);
                                        Bonk.Add(_elem130);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
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
                var struc = new TStruct("ListBonks");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Bonk != null && __isset.bonk)
                {
                    field.Name = "bonk";
                    field.Type = TType.List;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.Struct, Bonk.Count), cancellationToken);
                        foreach (Bonk _iter131 in Bonk)
                        {
                            await _iter131.WriteAsync(oprot, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
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

        public bool ShouldSerializeBonk()
        {
            return __isset.bonk;
        }

        #endregion XmlSerializer support

        public override string ToString()
        {
            var sb = new StringBuilder("ListBonks(");
            bool __first = true;
            if (Bonk != null && __isset.bonk)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Bonk: ");
                sb.Append(Bonk);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool bonk;
        }
    }
}