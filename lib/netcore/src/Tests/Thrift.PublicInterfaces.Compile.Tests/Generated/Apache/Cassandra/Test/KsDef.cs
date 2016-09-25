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
    public partial class KsDef : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private bool _durable_writes;
        private int _replication_factor;
        private Dictionary<string, string> _strategy_options;

        public KsDef()
        {
            this._durable_writes = true;
            this.__isset.durable_writes = true;
        }

        public KsDef(string name, string strategy_class, List<CfDef> cf_defs) : this()
        {
            this.Name = name;
            this.Strategy_class = strategy_class;
            this.Cf_defs = cf_defs;
        }

        [DataMember(Order = 0)]
        public string Name { get; set; }

        [DataMember(Order = 0)]
        public string Strategy_class { get; set; }

        [DataMember(Order = 0)]
        public Dictionary<string, string> Strategy_options
        {
            get { return _strategy_options; }
            set
            {
                __isset.strategy_options = true;
                this._strategy_options = value;
            }
        }

        /// <summary>
        ///     @deprecated
        /// </summary>
        [DataMember(Order = 0)]
        public int Replication_factor
        {
            get { return _replication_factor; }
            set
            {
                __isset.replication_factor = true;
                this._replication_factor = value;
            }
        }

        [DataMember(Order = 0)]
        public List<CfDef> Cf_defs { get; set; }

        [DataMember(Order = 0)]
        public bool Durable_writes
        {
            get { return _durable_writes; }
            set
            {
                __isset.durable_writes = true;
                this._durable_writes = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_name = false;
                bool isset_strategy_class = false;
                bool isset_cf_defs = false;
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
                                Name = await iprot.ReadStringAsync(cancellationToken);
                                isset_name = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.String)
                            {
                                Strategy_class = await iprot.ReadStringAsync(cancellationToken);
                                isset_strategy_class = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.Map)
                            {
                                {
                                    Strategy_options = new Dictionary<string, string>();
                                    TMap _map56 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i57 = 0; _i57 < _map56.Count; ++_i57)
                                    {
                                        string _key58;
                                        string _val59;
                                        _key58 = await iprot.ReadStringAsync(cancellationToken);
                                        _val59 = await iprot.ReadStringAsync(cancellationToken);
                                        Strategy_options[_key58] = _val59;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.I32)
                            {
                                Replication_factor = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 5:
                            if (field.Type == TType.List)
                            {
                                {
                                    Cf_defs = new List<CfDef>();
                                    TList _list60 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i61 = 0; _i61 < _list60.Count; ++_i61)
                                    {
                                        CfDef _elem62;
                                        _elem62 = new CfDef();
                                        await _elem62.ReadAsync(iprot, cancellationToken);
                                        Cf_defs.Add(_elem62);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                                isset_cf_defs = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 6:
                            if (field.Type == TType.Bool)
                            {
                                Durable_writes = await iprot.ReadBoolAsync(cancellationToken);
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
                if (!isset_name)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_strategy_class)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_cf_defs)
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
                var struc = new TStruct("KsDef");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "name";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Name, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "strategy_class";
                field.Type = TType.String;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Strategy_class, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                if (Strategy_options != null && __isset.strategy_options)
                {
                    field.Name = "strategy_options";
                    field.Type = TType.Map;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteMapBeginAsync(new TMap(TType.String, TType.String, Strategy_options.Count),
                                cancellationToken);
                        foreach (string _iter63 in Strategy_options.Keys)
                        {
                            await oprot.WriteStringAsync(_iter63, cancellationToken);
                            await oprot.WriteStringAsync(Strategy_options[_iter63], cancellationToken);
                        }
                        await oprot.WriteMapEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.replication_factor)
                {
                    field.Name = "replication_factor";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Replication_factor, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                field.Name = "cf_defs";
                field.Type = TType.List;
                field.ID = 5;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                {
                    await oprot.WriteListBeginAsync(new TList(TType.Struct, Cf_defs.Count), cancellationToken);
                    foreach (CfDef _iter64 in Cf_defs)
                    {
                        await _iter64.WriteAsync(oprot, cancellationToken);
                    }
                    await oprot.WriteListEndAsync(cancellationToken);
                }
                await oprot.WriteFieldEndAsync(cancellationToken);
                if (__isset.durable_writes)
                {
                    field.Name = "durable_writes";
                    field.Type = TType.Bool;
                    field.ID = 6;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBoolAsync(Durable_writes, cancellationToken);
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
            var sb = new StringBuilder("KsDef(");
            sb.Append(", Name: ");
            sb.Append(Name);
            sb.Append(", Strategy_class: ");
            sb.Append(Strategy_class);
            if (Strategy_options != null && __isset.strategy_options)
            {
                sb.Append(", Strategy_options: ");
                sb.Append(Strategy_options);
            }
            if (__isset.replication_factor)
            {
                sb.Append(", Replication_factor: ");
                sb.Append(Replication_factor);
            }
            sb.Append(", Cf_defs: ");
            sb.Append(Cf_defs);
            if (__isset.durable_writes)
            {
                sb.Append(", Durable_writes: ");
                sb.Append(Durable_writes);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool strategy_options;
            [DataMember] public bool replication_factor;
            [DataMember] public bool durable_writes;
        }

        #region XmlSerializer support

        public bool ShouldSerializeStrategy_options()
        {
            return __isset.strategy_options;
        }

        public bool ShouldSerializeReplication_factor()
        {
            return __isset.replication_factor;
        }

        public bool ShouldSerializeDurable_writes()
        {
            return __isset.durable_writes;
        }

        #endregion XmlSerializer support
    }
}