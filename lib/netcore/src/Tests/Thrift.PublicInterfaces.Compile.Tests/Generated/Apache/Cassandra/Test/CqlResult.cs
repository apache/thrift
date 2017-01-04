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
    public partial class CqlResult : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private int _num;
        private List<CqlRow> _rows;
        private CqlMetadata _schema;

        public CqlResult()
        {
        }

        public CqlResult(CqlResultType type) : this()
        {
            this.Type = type;
        }

        /// <summary>
        ///     <seealso cref="CqlResultType" />
        /// </summary>
        [DataMember(Order = 0)]
        public CqlResultType Type { get; set; }

        [DataMember(Order = 0)]
        public List<CqlRow> Rows
        {
            get { return _rows; }
            set
            {
                __isset.rows = true;
                this._rows = value;
            }
        }

        [DataMember(Order = 0)]
        public int Num
        {
            get { return _num; }
            set
            {
                __isset.num = true;
                this._num = value;
            }
        }

        [DataMember(Order = 0)]
        public CqlMetadata Schema
        {
            get { return _schema; }
            set
            {
                __isset.schema = true;
                this._schema = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_type = false;
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
                                Type = (CqlResultType) await iprot.ReadI32Async(cancellationToken);
                                isset_type = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.List)
                            {
                                {
                                    Rows = new List<CqlRow>();
                                    TList _list79 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i80 = 0; _i80 < _list79.Count; ++_i80)
                                    {
                                        CqlRow _elem81;
                                        _elem81 = new CqlRow();
                                        await _elem81.ReadAsync(iprot, cancellationToken);
                                        Rows.Add(_elem81);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.I32)
                            {
                                Num = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.Struct)
                            {
                                Schema = new CqlMetadata();
                                await Schema.ReadAsync(iprot, cancellationToken);
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
                if (!isset_type)
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
                var struc = new TStruct("CqlResult");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "type";
                field.Type = TType.I32;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteI32Async((int) Type, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                if (Rows != null && __isset.rows)
                {
                    field.Name = "rows";
                    field.Type = TType.List;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.Struct, Rows.Count), cancellationToken);
                        foreach (CqlRow _iter82 in Rows)
                        {
                            await _iter82.WriteAsync(oprot, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.num)
                {
                    field.Name = "num";
                    field.Type = TType.I32;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Num, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Schema != null && __isset.schema)
                {
                    field.Name = "schema";
                    field.Type = TType.Struct;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Schema.WriteAsync(oprot, cancellationToken);
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
            var sb = new StringBuilder("CqlResult(");
            sb.Append(", Type: ");
            sb.Append(Type);
            if (Rows != null && __isset.rows)
            {
                sb.Append(", Rows: ");
                sb.Append(Rows);
            }
            if (__isset.num)
            {
                sb.Append(", Num: ");
                sb.Append(Num);
            }
            if (Schema != null && __isset.schema)
            {
                sb.Append(", Schema: ");
                sb.Append(Schema == null ? "<null>" : Schema.ToString());
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool rows;
            [DataMember] public bool num;
            [DataMember] public bool schema;
        }

        #region XmlSerializer support

        public bool ShouldSerializeRows()
        {
            return __isset.rows;
        }

        public bool ShouldSerializeNum()
        {
            return __isset.num;
        }

        public bool ShouldSerializeSchema()
        {
            return __isset.schema;
        }

        #endregion XmlSerializer support
    }
}