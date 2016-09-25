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
    ///     A named list of columns.
    ///     @param name. see Column.name.
    ///     @param columns. A collection of standard Columns.  The columns within a super column are defined in an adhoc
    ///     manner.
    ///     Columns within a super column do not have to have matching structures (similarly named child columns).
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class SuperColumn : TBase
    {
        public SuperColumn()
        {
        }

        public SuperColumn(byte[] name, List<Column> columns) : this()
        {
            this.Name = name;
            this.Columns = columns;
        }

        [DataMember(Order = 0)]
        public byte[] Name { get; set; }

        [DataMember(Order = 0)]
        public List<Column> Columns { get; set; }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_name = false;
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
                                Name = await iprot.ReadBinaryAsync(cancellationToken);
                                isset_name = true;
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
                                    Columns = new List<Column>();
                                    TList _list0 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i1 = 0; _i1 < _list0.Count; ++_i1)
                                    {
                                        Column _elem2;
                                        _elem2 = new Column();
                                        await _elem2.ReadAsync(iprot, cancellationToken);
                                        Columns.Add(_elem2);
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
                if (!isset_name)
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
                var struc = new TStruct("SuperColumn");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "name";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Name, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "columns";
                field.Type = TType.List;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                {
                    await oprot.WriteListBeginAsync(new TList(TType.Struct, Columns.Count), cancellationToken);
                    foreach (Column _iter3 in Columns)
                    {
                        await _iter3.WriteAsync(oprot, cancellationToken);
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
            var sb = new StringBuilder("SuperColumn(");
            sb.Append(", Name: ");
            sb.Append(Name);
            sb.Append(", Columns: ");
            sb.Append(Columns);
            sb.Append(")");
            return sb.ToString();
        }
    }
}