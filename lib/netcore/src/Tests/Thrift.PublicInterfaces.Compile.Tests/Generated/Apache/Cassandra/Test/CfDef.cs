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
    public partial class CfDef : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private double _bloom_filter_fp_chance;
        private List<ColumnDef> _column_metadata;
        private string _column_type;
        private string _comment;
        private string _compaction_strategy;
        private Dictionary<string, string> _compaction_strategy_options;
        private string _comparator_type;
        private Dictionary<string, string> _compression_options;
        private string _default_validation_class;
        private int _gc_grace_seconds;
        private int _id;
        private byte[] _key_alias;
        private string _key_validation_class;
        private int _max_compaction_threshold;
        private double _merge_shards_chance;
        private int _min_compaction_threshold;
        private double _read_repair_chance;
        private bool _replicate_on_write;
        private string _subcomparator_type;

        public CfDef()
        {
            this._column_type = "Standard";
            this.__isset.column_type = true;
            this._comparator_type = "BytesType";
            this.__isset.comparator_type = true;
            this._read_repair_chance = 1;
            this.__isset.read_repair_chance = true;
        }

        public CfDef(string keyspace, string name) : this()
        {
            this.Keyspace = keyspace;
            this.Name = name;
        }

        [DataMember(Order = 0)]
        public string Keyspace { get; set; }

        [DataMember(Order = 0)]
        public string Name { get; set; }

        [DataMember(Order = 0)]
        public string Column_type
        {
            get { return _column_type; }
            set
            {
                __isset.column_type = true;
                this._column_type = value;
            }
        }

        [DataMember(Order = 0)]
        public string Comparator_type
        {
            get { return _comparator_type; }
            set
            {
                __isset.comparator_type = true;
                this._comparator_type = value;
            }
        }

        [DataMember(Order = 0)]
        public string Subcomparator_type
        {
            get { return _subcomparator_type; }
            set
            {
                __isset.subcomparator_type = true;
                this._subcomparator_type = value;
            }
        }

        [DataMember(Order = 0)]
        public string Comment
        {
            get { return _comment; }
            set
            {
                __isset.comment = true;
                this._comment = value;
            }
        }

        [DataMember(Order = 0)]
        public double Read_repair_chance
        {
            get { return _read_repair_chance; }
            set
            {
                __isset.read_repair_chance = true;
                this._read_repair_chance = value;
            }
        }

        [DataMember(Order = 0)]
        public List<ColumnDef> Column_metadata
        {
            get { return _column_metadata; }
            set
            {
                __isset.column_metadata = true;
                this._column_metadata = value;
            }
        }

        [DataMember(Order = 0)]
        public int Gc_grace_seconds
        {
            get { return _gc_grace_seconds; }
            set
            {
                __isset.gc_grace_seconds = true;
                this._gc_grace_seconds = value;
            }
        }

        [DataMember(Order = 0)]
        public string Default_validation_class
        {
            get { return _default_validation_class; }
            set
            {
                __isset.default_validation_class = true;
                this._default_validation_class = value;
            }
        }

        [DataMember(Order = 0)]
        public int Id
        {
            get { return _id; }
            set
            {
                __isset.id = true;
                this._id = value;
            }
        }

        [DataMember(Order = 0)]
        public int Min_compaction_threshold
        {
            get { return _min_compaction_threshold; }
            set
            {
                __isset.min_compaction_threshold = true;
                this._min_compaction_threshold = value;
            }
        }

        [DataMember(Order = 0)]
        public int Max_compaction_threshold
        {
            get { return _max_compaction_threshold; }
            set
            {
                __isset.max_compaction_threshold = true;
                this._max_compaction_threshold = value;
            }
        }

        [DataMember(Order = 0)]
        public bool Replicate_on_write
        {
            get { return _replicate_on_write; }
            set
            {
                __isset.replicate_on_write = true;
                this._replicate_on_write = value;
            }
        }

        [DataMember(Order = 0)]
        public double Merge_shards_chance
        {
            get { return _merge_shards_chance; }
            set
            {
                __isset.merge_shards_chance = true;
                this._merge_shards_chance = value;
            }
        }

        [DataMember(Order = 0)]
        public string Key_validation_class
        {
            get { return _key_validation_class; }
            set
            {
                __isset.key_validation_class = true;
                this._key_validation_class = value;
            }
        }

        [DataMember(Order = 0)]
        public byte[] Key_alias
        {
            get { return _key_alias; }
            set
            {
                __isset.key_alias = true;
                this._key_alias = value;
            }
        }

        [DataMember(Order = 0)]
        public string Compaction_strategy
        {
            get { return _compaction_strategy; }
            set
            {
                __isset.compaction_strategy = true;
                this._compaction_strategy = value;
            }
        }

        [DataMember(Order = 0)]
        public Dictionary<string, string> Compaction_strategy_options
        {
            get { return _compaction_strategy_options; }
            set
            {
                __isset.compaction_strategy_options = true;
                this._compaction_strategy_options = value;
            }
        }

        [DataMember(Order = 0)]
        public Dictionary<string, string> Compression_options
        {
            get { return _compression_options; }
            set
            {
                __isset.compression_options = true;
                this._compression_options = value;
            }
        }

        [DataMember(Order = 0)]
        public double Bloom_filter_fp_chance
        {
            get { return _bloom_filter_fp_chance; }
            set
            {
                __isset.bloom_filter_fp_chance = true;
                this._bloom_filter_fp_chance = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_keyspace = false;
                bool isset_name = false;
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
                                Keyspace = await iprot.ReadStringAsync(cancellationToken);
                                isset_keyspace = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
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
                        case 3:
                            if (field.Type == TType.String)
                            {
                                Column_type = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 5:
                            if (field.Type == TType.String)
                            {
                                Comparator_type = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 6:
                            if (field.Type == TType.String)
                            {
                                Subcomparator_type = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 8:
                            if (field.Type == TType.String)
                            {
                                Comment = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 12:
                            if (field.Type == TType.Double)
                            {
                                Read_repair_chance = await iprot.ReadDoubleAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 13:
                            if (field.Type == TType.List)
                            {
                                {
                                    Column_metadata = new List<ColumnDef>();
                                    TList _list42 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i43 = 0; _i43 < _list42.Count; ++_i43)
                                    {
                                        ColumnDef _elem44;
                                        _elem44 = new ColumnDef();
                                        await _elem44.ReadAsync(iprot, cancellationToken);
                                        Column_metadata.Add(_elem44);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 14:
                            if (field.Type == TType.I32)
                            {
                                Gc_grace_seconds = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 15:
                            if (field.Type == TType.String)
                            {
                                Default_validation_class = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 16:
                            if (field.Type == TType.I32)
                            {
                                Id = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 17:
                            if (field.Type == TType.I32)
                            {
                                Min_compaction_threshold = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 18:
                            if (field.Type == TType.I32)
                            {
                                Max_compaction_threshold = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 24:
                            if (field.Type == TType.Bool)
                            {
                                Replicate_on_write = await iprot.ReadBoolAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 25:
                            if (field.Type == TType.Double)
                            {
                                Merge_shards_chance = await iprot.ReadDoubleAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 26:
                            if (field.Type == TType.String)
                            {
                                Key_validation_class = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 28:
                            if (field.Type == TType.String)
                            {
                                Key_alias = await iprot.ReadBinaryAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 29:
                            if (field.Type == TType.String)
                            {
                                Compaction_strategy = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 30:
                            if (field.Type == TType.Map)
                            {
                                {
                                    Compaction_strategy_options = new Dictionary<string, string>();
                                    TMap _map45 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i46 = 0; _i46 < _map45.Count; ++_i46)
                                    {
                                        string _key47;
                                        string _val48;
                                        _key47 = await iprot.ReadStringAsync(cancellationToken);
                                        _val48 = await iprot.ReadStringAsync(cancellationToken);
                                        Compaction_strategy_options[_key47] = _val48;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 32:
                            if (field.Type == TType.Map)
                            {
                                {
                                    Compression_options = new Dictionary<string, string>();
                                    TMap _map49 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i50 = 0; _i50 < _map49.Count; ++_i50)
                                    {
                                        string _key51;
                                        string _val52;
                                        _key51 = await iprot.ReadStringAsync(cancellationToken);
                                        _val52 = await iprot.ReadStringAsync(cancellationToken);
                                        Compression_options[_key51] = _val52;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 33:
                            if (field.Type == TType.Double)
                            {
                                Bloom_filter_fp_chance = await iprot.ReadDoubleAsync(cancellationToken);
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
                if (!isset_keyspace)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_name)
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
                var struc = new TStruct("CfDef");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "keyspace";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Keyspace, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "name";
                field.Type = TType.String;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Name, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                if (Column_type != null && __isset.column_type)
                {
                    field.Name = "column_type";
                    field.Type = TType.String;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Column_type, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Comparator_type != null && __isset.comparator_type)
                {
                    field.Name = "comparator_type";
                    field.Type = TType.String;
                    field.ID = 5;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Comparator_type, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Subcomparator_type != null && __isset.subcomparator_type)
                {
                    field.Name = "subcomparator_type";
                    field.Type = TType.String;
                    field.ID = 6;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Subcomparator_type, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Comment != null && __isset.comment)
                {
                    field.Name = "comment";
                    field.Type = TType.String;
                    field.ID = 8;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Comment, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.read_repair_chance)
                {
                    field.Name = "read_repair_chance";
                    field.Type = TType.Double;
                    field.ID = 12;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteDoubleAsync(Read_repair_chance, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Column_metadata != null && __isset.column_metadata)
                {
                    field.Name = "column_metadata";
                    field.Type = TType.List;
                    field.ID = 13;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteListBeginAsync(new TList(TType.Struct, Column_metadata.Count), cancellationToken);
                        foreach (ColumnDef _iter53 in Column_metadata)
                        {
                            await _iter53.WriteAsync(oprot, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.gc_grace_seconds)
                {
                    field.Name = "gc_grace_seconds";
                    field.Type = TType.I32;
                    field.ID = 14;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Gc_grace_seconds, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Default_validation_class != null && __isset.default_validation_class)
                {
                    field.Name = "default_validation_class";
                    field.Type = TType.String;
                    field.ID = 15;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Default_validation_class, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.id)
                {
                    field.Name = "id";
                    field.Type = TType.I32;
                    field.ID = 16;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Id, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.min_compaction_threshold)
                {
                    field.Name = "min_compaction_threshold";
                    field.Type = TType.I32;
                    field.ID = 17;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Min_compaction_threshold, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.max_compaction_threshold)
                {
                    field.Name = "max_compaction_threshold";
                    field.Type = TType.I32;
                    field.ID = 18;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Max_compaction_threshold, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.replicate_on_write)
                {
                    field.Name = "replicate_on_write";
                    field.Type = TType.Bool;
                    field.ID = 24;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBoolAsync(Replicate_on_write, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.merge_shards_chance)
                {
                    field.Name = "merge_shards_chance";
                    field.Type = TType.Double;
                    field.ID = 25;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteDoubleAsync(Merge_shards_chance, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Key_validation_class != null && __isset.key_validation_class)
                {
                    field.Name = "key_validation_class";
                    field.Type = TType.String;
                    field.ID = 26;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Key_validation_class, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Key_alias != null && __isset.key_alias)
                {
                    field.Name = "key_alias";
                    field.Type = TType.String;
                    field.ID = 28;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Key_alias, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Compaction_strategy != null && __isset.compaction_strategy)
                {
                    field.Name = "compaction_strategy";
                    field.Type = TType.String;
                    field.ID = 29;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Compaction_strategy, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Compaction_strategy_options != null && __isset.compaction_strategy_options)
                {
                    field.Name = "compaction_strategy_options";
                    field.Type = TType.Map;
                    field.ID = 30;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteMapBeginAsync(
                                new TMap(TType.String, TType.String, Compaction_strategy_options.Count),
                                cancellationToken);
                        foreach (string _iter54 in Compaction_strategy_options.Keys)
                        {
                            await oprot.WriteStringAsync(_iter54, cancellationToken);
                            await oprot.WriteStringAsync(Compaction_strategy_options[_iter54], cancellationToken);
                        }
                        await oprot.WriteMapEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Compression_options != null && __isset.compression_options)
                {
                    field.Name = "compression_options";
                    field.Type = TType.Map;
                    field.ID = 32;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteMapBeginAsync(new TMap(TType.String, TType.String, Compression_options.Count),
                                cancellationToken);
                        foreach (string _iter55 in Compression_options.Keys)
                        {
                            await oprot.WriteStringAsync(_iter55, cancellationToken);
                            await oprot.WriteStringAsync(Compression_options[_iter55], cancellationToken);
                        }
                        await oprot.WriteMapEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.bloom_filter_fp_chance)
                {
                    field.Name = "bloom_filter_fp_chance";
                    field.Type = TType.Double;
                    field.ID = 33;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteDoubleAsync(Bloom_filter_fp_chance, cancellationToken);
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
            var sb = new StringBuilder("CfDef(");
            sb.Append(", Keyspace: ");
            sb.Append(Keyspace);
            sb.Append(", Name: ");
            sb.Append(Name);
            if (Column_type != null && __isset.column_type)
            {
                sb.Append(", Column_type: ");
                sb.Append(Column_type);
            }
            if (Comparator_type != null && __isset.comparator_type)
            {
                sb.Append(", Comparator_type: ");
                sb.Append(Comparator_type);
            }
            if (Subcomparator_type != null && __isset.subcomparator_type)
            {
                sb.Append(", Subcomparator_type: ");
                sb.Append(Subcomparator_type);
            }
            if (Comment != null && __isset.comment)
            {
                sb.Append(", Comment: ");
                sb.Append(Comment);
            }
            if (__isset.read_repair_chance)
            {
                sb.Append(", Read_repair_chance: ");
                sb.Append(Read_repair_chance);
            }
            if (Column_metadata != null && __isset.column_metadata)
            {
                sb.Append(", Column_metadata: ");
                sb.Append(Column_metadata);
            }
            if (__isset.gc_grace_seconds)
            {
                sb.Append(", Gc_grace_seconds: ");
                sb.Append(Gc_grace_seconds);
            }
            if (Default_validation_class != null && __isset.default_validation_class)
            {
                sb.Append(", Default_validation_class: ");
                sb.Append(Default_validation_class);
            }
            if (__isset.id)
            {
                sb.Append(", Id: ");
                sb.Append(Id);
            }
            if (__isset.min_compaction_threshold)
            {
                sb.Append(", Min_compaction_threshold: ");
                sb.Append(Min_compaction_threshold);
            }
            if (__isset.max_compaction_threshold)
            {
                sb.Append(", Max_compaction_threshold: ");
                sb.Append(Max_compaction_threshold);
            }
            if (__isset.replicate_on_write)
            {
                sb.Append(", Replicate_on_write: ");
                sb.Append(Replicate_on_write);
            }
            if (__isset.merge_shards_chance)
            {
                sb.Append(", Merge_shards_chance: ");
                sb.Append(Merge_shards_chance);
            }
            if (Key_validation_class != null && __isset.key_validation_class)
            {
                sb.Append(", Key_validation_class: ");
                sb.Append(Key_validation_class);
            }
            if (Key_alias != null && __isset.key_alias)
            {
                sb.Append(", Key_alias: ");
                sb.Append(Key_alias);
            }
            if (Compaction_strategy != null && __isset.compaction_strategy)
            {
                sb.Append(", Compaction_strategy: ");
                sb.Append(Compaction_strategy);
            }
            if (Compaction_strategy_options != null && __isset.compaction_strategy_options)
            {
                sb.Append(", Compaction_strategy_options: ");
                sb.Append(Compaction_strategy_options);
            }
            if (Compression_options != null && __isset.compression_options)
            {
                sb.Append(", Compression_options: ");
                sb.Append(Compression_options);
            }
            if (__isset.bloom_filter_fp_chance)
            {
                sb.Append(", Bloom_filter_fp_chance: ");
                sb.Append(Bloom_filter_fp_chance);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool column_type;
            [DataMember] public bool comparator_type;
            [DataMember] public bool subcomparator_type;
            [DataMember] public bool comment;
            [DataMember] public bool read_repair_chance;
            [DataMember] public bool column_metadata;
            [DataMember] public bool gc_grace_seconds;
            [DataMember] public bool default_validation_class;
            [DataMember] public bool id;
            [DataMember] public bool min_compaction_threshold;
            [DataMember] public bool max_compaction_threshold;
            [DataMember] public bool replicate_on_write;
            [DataMember] public bool merge_shards_chance;
            [DataMember] public bool key_validation_class;
            [DataMember] public bool key_alias;
            [DataMember] public bool compaction_strategy;
            [DataMember] public bool compaction_strategy_options;
            [DataMember] public bool compression_options;
            [DataMember] public bool bloom_filter_fp_chance;
        }

        #region XmlSerializer support

        public bool ShouldSerializeColumn_type()
        {
            return __isset.column_type;
        }

        public bool ShouldSerializeComparator_type()
        {
            return __isset.comparator_type;
        }

        public bool ShouldSerializeSubcomparator_type()
        {
            return __isset.subcomparator_type;
        }

        public bool ShouldSerializeComment()
        {
            return __isset.comment;
        }

        public bool ShouldSerializeRead_repair_chance()
        {
            return __isset.read_repair_chance;
        }

        public bool ShouldSerializeColumn_metadata()
        {
            return __isset.column_metadata;
        }

        public bool ShouldSerializeGc_grace_seconds()
        {
            return __isset.gc_grace_seconds;
        }

        public bool ShouldSerializeDefault_validation_class()
        {
            return __isset.default_validation_class;
        }

        public bool ShouldSerializeId()
        {
            return __isset.id;
        }

        public bool ShouldSerializeMin_compaction_threshold()
        {
            return __isset.min_compaction_threshold;
        }

        public bool ShouldSerializeMax_compaction_threshold()
        {
            return __isset.max_compaction_threshold;
        }

        public bool ShouldSerializeReplicate_on_write()
        {
            return __isset.replicate_on_write;
        }

        public bool ShouldSerializeMerge_shards_chance()
        {
            return __isset.merge_shards_chance;
        }

        public bool ShouldSerializeKey_validation_class()
        {
            return __isset.key_validation_class;
        }

        public bool ShouldSerializeKey_alias()
        {
            return __isset.key_alias;
        }

        public bool ShouldSerializeCompaction_strategy()
        {
            return __isset.compaction_strategy;
        }

        public bool ShouldSerializeCompaction_strategy_options()
        {
            return __isset.compaction_strategy_options;
        }

        public bool ShouldSerializeCompression_options()
        {
            return __isset.compression_options;
        }

        public bool ShouldSerializeBloom_filter_fp_chance()
        {
            return __isset.bloom_filter_fp_chance;
        }

        #endregion XmlSerializer support
    }
}