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
    public partial class ColumnDef : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private string _index_name;
        private Dictionary<string, string> _index_options;
        private IndexType _index_type;

        public ColumnDef()
        {
        }

        public ColumnDef(byte[] name, string validation_class) : this()
        {
            this.Name = name;
            this.Validation_class = validation_class;
        }

        [DataMember(Order = 0)]
        public byte[] Name { get; set; }

        [DataMember(Order = 0)]
        public string Validation_class { get; set; }

        /// <summary>
        ///     <seealso cref="IndexType" />
        /// </summary>
        [DataMember(Order = 0)]
        public IndexType Index_type
        {
            get { return _index_type; }
            set
            {
                __isset.index_type = true;
                this._index_type = value;
            }
        }

        [DataMember(Order = 0)]
        public string Index_name
        {
            get { return _index_name; }
            set
            {
                __isset.index_name = true;
                this._index_name = value;
            }
        }

        [DataMember(Order = 0)]
        public Dictionary<string, string> Index_options
        {
            get { return _index_options; }
            set
            {
                __isset.index_options = true;
                this._index_options = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_name = false;
                bool isset_validation_class = false;
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
                                Name = await iprot.ReadBinaryAsync(cancellationToken);
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
                                Validation_class = await iprot.ReadStringAsync(cancellationToken);
                                isset_validation_class = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.I32)
                            {
                                Index_type = (IndexType) await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.String)
                            {
                                Index_name = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 5:
                            if (field.Type == TType.Map)
                            {
                                {
                                    Index_options = new Dictionary<string, string>();
                                    TMap _map37 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i38 = 0; _i38 < _map37.Count; ++_i38)
                                    {
                                        string _key39;
                                        string _val40;
                                        _key39 = await iprot.ReadStringAsync(cancellationToken);
                                        _val40 = await iprot.ReadStringAsync(cancellationToken);
                                        Index_options[_key39] = _val40;
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
                if (!isset_name)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_validation_class)
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
                var struc = new TStruct("ColumnDef");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "name";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Name, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "validation_class";
                field.Type = TType.String;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Validation_class, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                if (__isset.index_type)
                {
                    field.Name = "index_type";
                    field.Type = TType.I32;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Index_type, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Index_name != null && __isset.index_name)
                {
                    field.Name = "index_name";
                    field.Type = TType.String;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Index_name, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Index_options != null && __isset.index_options)
                {
                    field.Name = "index_options";
                    field.Type = TType.Map;
                    field.ID = 5;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteMapBeginAsync(new TMap(TType.String, TType.String, Index_options.Count),
                                cancellationToken);
                        foreach (string _iter41 in Index_options.Keys)
                        {
                            await oprot.WriteStringAsync(_iter41, cancellationToken);
                            await oprot.WriteStringAsync(Index_options[_iter41], cancellationToken);
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

        public override string ToString()
        {
            var sb = new StringBuilder("ColumnDef(");
            sb.Append(", Name: ");
            sb.Append(Name);
            sb.Append(", Validation_class: ");
            sb.Append(Validation_class);
            if (__isset.index_type)
            {
                sb.Append(", Index_type: ");
                sb.Append(Index_type);
            }
            if (Index_name != null && __isset.index_name)
            {
                sb.Append(", Index_name: ");
                sb.Append(Index_name);
            }
            if (Index_options != null && __isset.index_options)
            {
                sb.Append(", Index_options: ");
                sb.Append(Index_options);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool index_type;
            [DataMember] public bool index_name;
            [DataMember] public bool index_options;
        }

        #region XmlSerializer support

        public bool ShouldSerializeIndex_type()
        {
            return __isset.index_type;
        }

        public bool ShouldSerializeIndex_name()
        {
            return __isset.index_name;
        }

        public bool ShouldSerializeIndex_options()
        {
            return __isset.index_options;
        }

        #endregion XmlSerializer support
    }
}