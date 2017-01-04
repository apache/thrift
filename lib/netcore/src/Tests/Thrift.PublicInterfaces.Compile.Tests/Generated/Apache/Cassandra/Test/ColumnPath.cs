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
    /// <summary>
    ///     The ColumnPath is the path to a single column in Cassandra. It might make sense to think of ColumnPath and
    ///     ColumnParent in terms of a directory structure.
    ///     ColumnPath is used to looking up a single column.
    ///     @param column_family. The name of the CF of the column being looked up.
    ///     @param super_column. The super column name.
    ///     @param column. The column name.
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class ColumnPath : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private byte[] _column;
        private byte[] _super_column;

        public ColumnPath()
        {
        }

        public ColumnPath(string column_family) : this()
        {
            this.Column_family = column_family;
        }

        [DataMember(Order = 0)]
        public string Column_family { get; set; }

        [DataMember(Order = 0)]
        public byte[] Super_column
        {
            get { return _super_column; }
            set
            {
                __isset.super_column = true;
                this._super_column = value;
            }
        }

        [DataMember(Order = 0)]
        public byte[] Column
        {
            get { return _column; }
            set
            {
                __isset.column = true;
                this._column = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_column_family = false;
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
                        case 3:
                            if (field.Type == TType.String)
                            {
                                Column_family = await iprot.ReadStringAsync(cancellationToken);
                                isset_column_family = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.String)
                            {
                                Super_column = await iprot.ReadBinaryAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 5:
                            if (field.Type == TType.String)
                            {
                                Column = await iprot.ReadBinaryAsync(cancellationToken);
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
                if (!isset_column_family)
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
                var struc = new TStruct("ColumnPath");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "column_family";
                field.Type = TType.String;
                field.ID = 3;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Column_family, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                if (Super_column != null && __isset.super_column)
                {
                    field.Name = "super_column";
                    field.Type = TType.String;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Super_column, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Column != null && __isset.column)
                {
                    field.Name = "column";
                    field.Type = TType.String;
                    field.ID = 5;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Column, cancellationToken);
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
            var sb = new StringBuilder("ColumnPath(");
            sb.Append(", Column_family: ");
            sb.Append(Column_family);
            if (Super_column != null && __isset.super_column)
            {
                sb.Append(", Super_column: ");
                sb.Append(Super_column);
            }
            if (Column != null && __isset.column)
            {
                sb.Append(", Column: ");
                sb.Append(Column);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool super_column;
            [DataMember] public bool column;
        }

        #region XmlSerializer support

        public bool ShouldSerializeSuper_column()
        {
            return __isset.super_column;
        }

        public bool ShouldSerializeColumn()
        {
            return __isset.column;
        }

        #endregion XmlSerializer support
    }
}