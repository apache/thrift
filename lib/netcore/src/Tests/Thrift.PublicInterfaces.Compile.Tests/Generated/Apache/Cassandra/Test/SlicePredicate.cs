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
    ///     A SlicePredicate is similar to a mathematic predicate (see
    ///     http://en.wikipedia.org/wiki/Predicate_(mathematical_logic)),
    ///     which is described as "a property that the elements of a set have in common."
    ///     SlicePredicate's in Cassandra are described with either a list of column_names or a SliceRange.  If column_names is
    ///     specified, slice_range is ignored.
    ///     @param column_name. A list of column names to retrieve. This can be used similar to Memcached's "multi-get" feature
    ///     to fetch N known column names. For instance, if you know you wish to fetch columns 'Joe', 'Jack',
    ///     and 'Jim' you can pass those column names as a list to fetch all three at once.
    ///     @param slice_range. A SliceRange describing how to range, order, and/or limit the slice.
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class SlicePredicate : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private List<byte[]> _column_names;
        private SliceRange _slice_range;

        public SlicePredicate()
        {
        }

        [DataMember(Order = 0)]
        public List<byte[]> Column_names
        {
            get { return _column_names; }
            set
            {
                __isset.column_names = true;
                this._column_names = value;
            }
        }

        [DataMember(Order = 0)]
        public SliceRange Slice_range
        {
            get { return _slice_range; }
            set
            {
                __isset.slice_range = true;
                this._slice_range = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
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
                                    Column_names = new List<byte[]>();
                                    TList _list8 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i9 = 0; _i9 < _list8.Count; ++_i9)
                                    {
                                        byte[] _elem10;
                                        _elem10 = await iprot.ReadBinaryAsync(cancellationToken);
                                        Column_names.Add(_elem10);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.Struct)
                            {
                                Slice_range = new SliceRange();
                                await Slice_range.ReadAsync(iprot, cancellationToken);
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
                var struc = new TStruct("SlicePredicate");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Column_names != null && __isset.column_names)
                {
                    field.Name = "column_names";
                    field.Type = TType.List;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.String, Column_names.Count), cancellationToken);
                        foreach (byte[] _iter11 in Column_names)
                        {
                            await oprot.WriteBinaryAsync(_iter11, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Slice_range != null && __isset.slice_range)
                {
                    field.Name = "slice_range";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Slice_range.WriteAsync(oprot, cancellationToken);
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
            var sb = new StringBuilder("SlicePredicate(");
            bool __first = true;
            if (Column_names != null && __isset.column_names)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Column_names: ");
                sb.Append(Column_names);
            }
            if (Slice_range != null && __isset.slice_range)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Slice_range: ");
                sb.Append(Slice_range == null ? "<null>" : Slice_range.ToString());
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool column_names;
            [DataMember] public bool slice_range;
        }

        #region XmlSerializer support

        public bool ShouldSerializeColumn_names()
        {
            return __isset.column_names;
        }

        public bool ShouldSerializeSlice_range()
        {
            return __isset.slice_range;
        }

        #endregion XmlSerializer support
    }
}