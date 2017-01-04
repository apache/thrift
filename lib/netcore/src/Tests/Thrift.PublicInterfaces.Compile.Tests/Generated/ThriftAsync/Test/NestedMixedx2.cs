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
    public partial class NestedMixedx2 : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private List<THashSet<int>> _int_set_list;
        private Dictionary<int, THashSet<string>> _map_int_strset;
        private List<Dictionary<int, THashSet<string>>> _map_int_strset_list;

        public NestedMixedx2()
        {
        }

        [DataMember(Order = 0)]
        public List<THashSet<int>> Int_set_list
        {
            get { return _int_set_list; }
            set
            {
                __isset.int_set_list = true;
                this._int_set_list = value;
            }
        }

        [DataMember(Order = 0)]
        public Dictionary<int, THashSet<string>> Map_int_strset
        {
            get { return _map_int_strset; }
            set
            {
                __isset.map_int_strset = true;
                this._map_int_strset = value;
            }
        }

        [DataMember(Order = 0)]
        public List<Dictionary<int, THashSet<string>>> Map_int_strset_list
        {
            get { return _map_int_strset_list; }
            set
            {
                __isset.map_int_strset_list = true;
                this._map_int_strset_list = value;
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
                                    Int_set_list = new List<THashSet<int>>();
                                    TList _list98 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i99 = 0; _i99 < _list98.Count; ++_i99)
                                    {
                                        THashSet<int> _elem100;
                                        {
                                            _elem100 = new THashSet<int>();
                                            TSet _set101 = await iprot.ReadSetBeginAsync(cancellationToken);
                                            for (int _i102 = 0; _i102 < _set101.Count; ++_i102)
                                            {
                                                int _elem103;
                                                _elem103 = await iprot.ReadI32Async(cancellationToken);
                                                _elem100.Add(_elem103);
                                            }
                                            await iprot.ReadSetEndAsync(cancellationToken);
                                        }
                                        Int_set_list.Add(_elem100);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
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
                                    Map_int_strset = new Dictionary<int, THashSet<string>>();
                                    TMap _map104 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i105 = 0; _i105 < _map104.Count; ++_i105)
                                    {
                                        int _key106;
                                        THashSet<string> _val107;
                                        _key106 = await iprot.ReadI32Async(cancellationToken);
                                        {
                                            _val107 = new THashSet<string>();
                                            TSet _set108 = await iprot.ReadSetBeginAsync(cancellationToken);
                                            for (int _i109 = 0; _i109 < _set108.Count; ++_i109)
                                            {
                                                string _elem110;
                                                _elem110 = await iprot.ReadStringAsync(cancellationToken);
                                                _val107.Add(_elem110);
                                            }
                                            await iprot.ReadSetEndAsync(cancellationToken);
                                        }
                                        Map_int_strset[_key106] = _val107;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.List)
                            {
                                {
                                    Map_int_strset_list = new List<Dictionary<int, THashSet<string>>>();
                                    TList _list111 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i112 = 0; _i112 < _list111.Count; ++_i112)
                                    {
                                        Dictionary<int, THashSet<string>> _elem113;
                                        {
                                            _elem113 = new Dictionary<int, THashSet<string>>();
                                            TMap _map114 = await iprot.ReadMapBeginAsync(cancellationToken);
                                            for (int _i115 = 0; _i115 < _map114.Count; ++_i115)
                                            {
                                                int _key116;
                                                THashSet<string> _val117;
                                                _key116 = await iprot.ReadI32Async(cancellationToken);
                                                {
                                                    _val117 = new THashSet<string>();
                                                    TSet _set118 = await iprot.ReadSetBeginAsync(cancellationToken);
                                                    for (int _i119 = 0; _i119 < _set118.Count; ++_i119)
                                                    {
                                                        string _elem120;
                                                        _elem120 = await iprot.ReadStringAsync(cancellationToken);
                                                        _val117.Add(_elem120);
                                                    }
                                                    await iprot.ReadSetEndAsync(cancellationToken);
                                                }
                                                _elem113[_key116] = _val117;
                                            }
                                            await iprot.ReadMapEndAsync(cancellationToken);
                                        }
                                        Map_int_strset_list.Add(_elem113);
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
                var struc = new TStruct("NestedMixedx2");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Int_set_list != null && __isset.int_set_list)
                {
                    field.Name = "int_set_list";
                    field.Type = TType.List;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.Set, Int_set_list.Count), cancellationToken);
                        foreach (THashSet<int> _iter121 in Int_set_list)
                        {
                            {
                                await oprot.WriteSetBeginAsync(new TSet(TType.I32, _iter121.Count), cancellationToken);
                                foreach (int _iter122 in _iter121)
                                {
                                    await oprot.WriteI32Async(_iter122, cancellationToken);
                                }
                                await oprot.WriteSetEndAsync(cancellationToken);
                            }
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Map_int_strset != null && __isset.map_int_strset)
                {
                    field.Name = "map_int_strset";
                    field.Type = TType.Map;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteMapBeginAsync(new TMap(TType.I32, TType.Set, Map_int_strset.Count),
                                cancellationToken);
                        foreach (int _iter123 in Map_int_strset.Keys)
                        {
                            await oprot.WriteI32Async(_iter123, cancellationToken);
                            {
                                await
                                    oprot.WriteSetBeginAsync(new TSet(TType.String, Map_int_strset[_iter123].Count),
                                        cancellationToken);
                                foreach (string _iter124 in Map_int_strset[_iter123])
                                {
                                    await oprot.WriteStringAsync(_iter124, cancellationToken);
                                }
                                await oprot.WriteSetEndAsync(cancellationToken);
                            }
                        }
                        await oprot.WriteMapEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Map_int_strset_list != null && __isset.map_int_strset_list)
                {
                    field.Name = "map_int_strset_list";
                    field.Type = TType.List;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteListBeginAsync(new TList(TType.Map, Map_int_strset_list.Count), cancellationToken);
                        foreach (Dictionary<int, THashSet<string>> _iter125 in Map_int_strset_list)
                        {
                            {
                                await
                                    oprot.WriteMapBeginAsync(new TMap(TType.I32, TType.Set, _iter125.Count),
                                        cancellationToken);
                                foreach (int _iter126 in _iter125.Keys)
                                {
                                    await oprot.WriteI32Async(_iter126, cancellationToken);
                                    {
                                        await
                                            oprot.WriteSetBeginAsync(new TSet(TType.String, _iter125[_iter126].Count),
                                                cancellationToken);
                                        foreach (string _iter127 in _iter125[_iter126])
                                        {
                                            await oprot.WriteStringAsync(_iter127, cancellationToken);
                                        }
                                        await oprot.WriteSetEndAsync(cancellationToken);
                                    }
                                }
                                await oprot.WriteMapEndAsync(cancellationToken);
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

        public override string ToString()
        {
            var sb = new StringBuilder("NestedMixedx2(");
            bool __first = true;
            if (Int_set_list != null && __isset.int_set_list)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Int_set_list: ");
                sb.Append(Int_set_list);
            }
            if (Map_int_strset != null && __isset.map_int_strset)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Map_int_strset: ");
                sb.Append(Map_int_strset);
            }
            if (Map_int_strset_list != null && __isset.map_int_strset_list)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Map_int_strset_list: ");
                sb.Append(Map_int_strset_list);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool int_set_list;
            [DataMember] public bool map_int_strset;
            [DataMember] public bool map_int_strset_list;
        }

        #region XmlSerializer support

        public bool ShouldSerializeInt_set_list()
        {
            return __isset.int_set_list;
        }

        public bool ShouldSerializeMap_int_strset()
        {
            return __isset.map_int_strset;
        }

        public bool ShouldSerializeMap_int_strset_list()
        {
            return __isset.map_int_strset_list;
        }

        #endregion XmlSerializer support
    }
}