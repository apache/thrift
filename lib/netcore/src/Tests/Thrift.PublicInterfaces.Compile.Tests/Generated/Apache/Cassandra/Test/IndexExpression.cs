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
    public partial class IndexExpression : TBase
    {
        public IndexExpression()
        {
        }

        public IndexExpression(byte[] column_name, IndexOperator op, byte[] value) : this()
        {
            this.Column_name = column_name;
            this.Op = op;
            this.Value = value;
        }

        [DataMember(Order = 0)]
        public byte[] Column_name { get; set; }

        /// <summary>
        ///     <seealso cref="IndexOperator" />
        /// </summary>
        [DataMember(Order = 0)]
        public IndexOperator Op { get; set; }

        [DataMember(Order = 0)]
        public byte[] Value { get; set; }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_column_name = false;
                bool isset_op = false;
                bool isset_value = false;
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
                                Column_name = await iprot.ReadBinaryAsync(cancellationToken);
                                isset_column_name = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.I32)
                            {
                                Op = (IndexOperator) await iprot.ReadI32Async(cancellationToken);
                                isset_op = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.String)
                            {
                                Value = await iprot.ReadBinaryAsync(cancellationToken);
                                isset_value = true;
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
                if (!isset_column_name)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_op)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_value)
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
                var struc = new TStruct("IndexExpression");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "column_name";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Column_name, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "op";
                field.Type = TType.I32;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteI32Async((int) Op, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "value";
                field.Type = TType.String;
                field.ID = 3;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Value, cancellationToken);
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
            var sb = new StringBuilder("IndexExpression(");
            sb.Append(", Column_name: ");
            sb.Append(Column_name);
            sb.Append(", Op: ");
            sb.Append(Op);
            sb.Append(", Value: ");
            sb.Append(Value);
            sb.Append(")");
            return sb.ToString();
        }
    }
}