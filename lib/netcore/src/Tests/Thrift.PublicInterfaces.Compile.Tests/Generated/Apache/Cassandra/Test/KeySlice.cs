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
    ///     A KeySlice is key followed by the data it maps to. A collection of KeySlice is returned by the get_range_slice
    ///     operation.
    ///     @param key. a row key
    ///     @param columns. List of data represented by the key. Typically, the list is pared down to only the columns
    ///     specified by
    ///     a SlicePredicate.
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class KeySlice : TBase
    {
        public KeySlice()
        {
        }

        public KeySlice(byte[] key, List<ColumnOrSuperColumn> columns) : this()
        {
            this.Key = key;
            this.Columns = columns;
        }

        [DataMember(Order = 0)]
        public byte[] Key { get; set; }

        [DataMember(Order = 0)]
        public List<ColumnOrSuperColumn> Columns { get; set; }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_key = false;
                bool isset_columns = false;
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
                                Key = await iprot.ReadBinaryAsync(cancellationToken);
                                isset_key = true;
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
                                    Columns = new List<ColumnOrSuperColumn>();
                                    TList _list16 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i17 = 0; _i17 < _list16.Count; ++_i17)
                                    {
                                        ColumnOrSuperColumn _elem18;
                                        _elem18 = new ColumnOrSuperColumn();
                                        await _elem18.ReadAsync(iprot, cancellationToken);
                                        Columns.Add(_elem18);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                                isset_columns = true;
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
                if (!isset_key)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_columns)
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
                var struc = new TStruct("KeySlice");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "key";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Key, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "columns";
                field.Type = TType.List;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                {
                    await oprot.WriteListBeginAsync(new TList(TType.Struct, Columns.Count), cancellationToken);
                    foreach (ColumnOrSuperColumn _iter19 in Columns)
                    {
                        await _iter19.WriteAsync(oprot, cancellationToken);
                    }
                    await oprot.WriteListEndAsync(cancellationToken);
                }
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
            var sb = new StringBuilder("KeySlice(");
            sb.Append(", Key: ");
            sb.Append(Key);
            sb.Append(", Columns: ");
            sb.Append(Columns);
            sb.Append(")");
            return sb.ToString();
        }
    }
}