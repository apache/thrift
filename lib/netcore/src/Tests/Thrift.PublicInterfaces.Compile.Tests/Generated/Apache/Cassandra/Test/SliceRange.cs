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
    ///     A slice range is a structure that stores basic range, ordering and limit information for a query that will return
    ///     multiple columns. It could be thought of as Cassandra's version of LIMIT and ORDER BY
    ///     @param start. The column name to start the slice with. This attribute is not required, though there is no default
    ///     value,
    ///     and can be safely set to '', i.e., an empty byte array, to start with the first column name. Otherwise, it
    ///     must a valid value under the rules of the Comparator defined for the given ColumnFamily.
    ///     @param finish. The column name to stop the slice at. This attribute is not required, though there is no default
    ///     value,
    ///     and can be safely set to an empty byte array to not stop until 'count' results are seen. Otherwise, it
    ///     must also be a valid value to the ColumnFamily Comparator.
    ///     @param reversed. Whether the results should be ordered in reversed order. Similar to ORDER BY blah DESC in SQL.
    ///     @param count. How many columns to return. Similar to LIMIT in SQL. May be arbitrarily large, but Thrift will
    ///     materialize the whole result into memory before returning it to the client, so be aware that you may
    ///     be better served by iterating through slices by passing the last value of one call in as the 'start'
    ///     of the next instead of increasing 'count' arbitrarily large.
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class SliceRange : TBase
    {
        public SliceRange()
        {
            this.Reversed = false;
            this.Count = 100;
        }

        public SliceRange(byte[] start, byte[] finish, bool reversed, int count) : this()
        {
            this.Start = start;
            this.Finish = finish;
            this.Reversed = reversed;
            this.Count = count;
        }

        [DataMember(Order = 0)]
        public byte[] Start { get; set; }

        [DataMember(Order = 0)]
        public byte[] Finish { get; set; }

        [DataMember(Order = 0)]
        public bool Reversed { get; set; }

        [DataMember(Order = 0)]
        public int Count { get; set; }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_start = false;
                bool isset_finish = false;
                bool isset_reversed = false;
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
                            if (field.Type == TType.String)
                            {
                                Start = await iprot.ReadBinaryAsync(cancellationToken);
                                isset_start = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.String)
                            {
                                Finish = await iprot.ReadBinaryAsync(cancellationToken);
                                isset_finish = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.Bool)
                            {
                                Reversed = await iprot.ReadBoolAsync(cancellationToken);
                                isset_reversed = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
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
                if (!isset_start)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_finish)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_reversed)
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
                var struc = new TStruct("SliceRange");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "start";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Start, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "finish";
                field.Type = TType.String;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Finish, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "reversed";
                field.Type = TType.Bool;
                field.ID = 3;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBoolAsync(Reversed, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "count";
                field.Type = TType.I32;
                field.ID = 4;
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
            var sb = new StringBuilder("SliceRange(");
            sb.Append(", Start: ");
            sb.Append(Start);
            sb.Append(", Finish: ");
            sb.Append(Finish);
            sb.Append(", Reversed: ");
            sb.Append(Reversed);
            sb.Append(", Count: ");
            sb.Append(Count);
            sb.Append(")");
            return sb.ToString();
        }
    }
}