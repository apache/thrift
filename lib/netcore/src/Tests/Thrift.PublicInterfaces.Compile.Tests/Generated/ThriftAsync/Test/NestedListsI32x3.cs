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
    public partial class NestedListsI32x3 : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private List<List<List<int>>> _integerlist;

        public NestedListsI32x3()
        {
        }

        [DataMember(Order = 0)]
        public List<List<List<int>>> Integerlist
        {
            get { return _integerlist; }
            set
            {
                __isset.integerlist = true;
                this._integerlist = value;
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
                                    Integerlist = new List<List<List<int>>>();
                                    TList _list86 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i87 = 0; _i87 < _list86.Count; ++_i87)
                                    {
                                        List<List<int>> _elem88;
                                        {
                                            _elem88 = new List<List<int>>();
                                            TList _list89 = await iprot.ReadListBeginAsync(cancellationToken);
                                            for (int _i90 = 0; _i90 < _list89.Count; ++_i90)
                                            {
                                                List<int> _elem91;
                                                {
                                                    _elem91 = new List<int>();
                                                    TList _list92 = await iprot.ReadListBeginAsync(cancellationToken);
                                                    for (int _i93 = 0; _i93 < _list92.Count; ++_i93)
                                                    {
                                                        int _elem94;
                                                        _elem94 = await iprot.ReadI32Async(cancellationToken);
                                                        _elem91.Add(_elem94);
                                                    }
                                                    await iprot.ReadListEndAsync(cancellationToken);
                                                }
                                                _elem88.Add(_elem91);
                                            }
                                            await iprot.ReadListEndAsync(cancellationToken);
                                        }
                                        Integerlist.Add(_elem88);
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
                var struc = new TStruct("NestedListsI32x3");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Integerlist != null && __isset.integerlist)
                {
                    field.Name = "integerlist";
                    field.Type = TType.List;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.List, Integerlist.Count), cancellationToken);
                        foreach (List<List<int>> _iter95 in Integerlist)
                        {
                            {
                                await oprot.WriteListBeginAsync(new TList(TType.List, _iter95.Count), cancellationToken);
                                foreach (List<int> _iter96 in _iter95)
                                {
                                    {
                                        await
                                            oprot.WriteListBeginAsync(new TList(TType.I32, _iter96.Count),
                                                cancellationToken);
                                        foreach (int _iter97 in _iter96)
                                        {
                                            await oprot.WriteI32Async(_iter97, cancellationToken);
                                        }
                                        await oprot.WriteListEndAsync(cancellationToken);
                                    }
                                }
                                await oprot.WriteListEndAsync(cancellationToken);
                            }
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

        public bool ShouldSerializeIntegerlist()
        {
            return __isset.integerlist;
        }

        #endregion XmlSerializer support

        public override string ToString()
        {
            var sb = new StringBuilder("NestedListsI32x3(");
            bool __first = true;
            if (Integerlist != null && __isset.integerlist)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Integerlist: ");
                sb.Append(Integerlist);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool integerlist;
        }
    }
}