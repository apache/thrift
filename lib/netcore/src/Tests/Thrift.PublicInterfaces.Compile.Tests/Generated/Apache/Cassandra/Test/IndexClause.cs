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
    public partial class IndexClause : TBase
    {
        public IndexClause()
        {
            this.Count = 100;
        }

        public IndexClause(List<IndexExpression> expressions, byte[] start_key, int count) : this()
        {
            this.Expressions = expressions;
            this.Start_key = start_key;
            this.Count = count;
        }

        [DataMember(Order = 0)]
        public List<IndexExpression> Expressions { get; set; }

        [DataMember(Order = 0)]
        public byte[] Start_key { get; set; }

        [DataMember(Order = 0)]
        public int Count { get; set; }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_expressions = false;
                bool isset_start_key = false;
                bool isset_count = false;
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
                                    Expressions = new List<IndexExpression>();
                                    TList _list12 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i13 = 0; _i13 < _list12.Count; ++_i13)
                                    {
                                        IndexExpression _elem14;
                                        _elem14 = new IndexExpression();
                                        await _elem14.ReadAsync(iprot, cancellationToken);
                                        Expressions.Add(_elem14);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                                isset_expressions = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.String)
                            {
                                Start_key = await iprot.ReadBinaryAsync(cancellationToken);
                                isset_start_key = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.I32)
                            {
                                Count = await iprot.ReadI32Async(cancellationToken);
                                isset_count = true;
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
                if (!isset_expressions)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_start_key)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_count)
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
                var struc = new TStruct("IndexClause");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "expressions";
                field.Type = TType.List;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                {
                    await oprot.WriteListBeginAsync(new TList(TType.Struct, Expressions.Count), cancellationToken);
                    foreach (IndexExpression _iter15 in Expressions)
                    {
                        await _iter15.WriteAsync(oprot, cancellationToken);
                    }
                    await oprot.WriteListEndAsync(cancellationToken);
                }
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "start_key";
                field.Type = TType.String;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Start_key, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "count";
                field.Type = TType.I32;
                field.ID = 3;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteI32Async(Count, cancellationToken);
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
            var sb = new StringBuilder("IndexClause(");
            sb.Append(", Expressions: ");
            sb.Append(Expressions);
            sb.Append(", Start_key: ");
            sb.Append(Start_key);
            sb.Append(", Count: ");
            sb.Append(Count);
            sb.Append(")");
            return sb.ToString();
        }
    }
}