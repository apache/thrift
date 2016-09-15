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
    ///     Methods for fetching rows/records from Cassandra will return either a single instance of ColumnOrSuperColumn or a
    ///     list
    ///     of ColumnOrSuperColumns (get_slice()). If you're looking up a SuperColumn (or list of SuperColumns) then the
    ///     resulting
    ///     instances of ColumnOrSuperColumn will have the requested SuperColumn in the attribute super_column. For queries
    ///     resulting
    ///     in Columns, those values will be in the attribute column. This change was made between 0.3 and 0.4 to standardize
    ///     on
    ///     single query methods that may return either a SuperColumn or Column.
    ///     If the query was on a counter column family, you will either get a counter_column (instead of a column) or a
    ///     counter_super_column (instead of a super_column)
    ///     @param column. The Column returned by get() or get_slice().
    ///     @param super_column. The SuperColumn returned by get() or get_slice().
    ///     @param counter_column. The Counterolumn returned by get() or get_slice().
    ///     @param counter_super_column. The CounterSuperColumn returned by get() or get_slice().
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class ColumnOrSuperColumn : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private Column _column;
        private CounterColumn _counter_column;
        private CounterSuperColumn _counter_super_column;
        private SuperColumn _super_column;

        public ColumnOrSuperColumn()
        {
        }

        [DataMember(Order = 0)]
        public Column Column
        {
            get { return _column; }
            set
            {
                __isset.column = true;
                this._column = value;
            }
        }

        [DataMember(Order = 0)]
        public SuperColumn Super_column
        {
            get { return _super_column; }
            set
            {
                __isset.super_column = true;
                this._super_column = value;
            }
        }

        [DataMember(Order = 0)]
        public CounterColumn Counter_column
        {
            get { return _counter_column; }
            set
            {
                __isset.counter_column = true;
                this._counter_column = value;
            }
        }

        [DataMember(Order = 0)]
        public CounterSuperColumn Counter_super_column
        {
            get { return _counter_super_column; }
            set
            {
                __isset.counter_super_column = true;
                this._counter_super_column = value;
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
                            if (field.Type == TType.Struct)
                            {
                                Column = new Column();
                                await Column.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.Struct)
                            {
                                Super_column = new SuperColumn();
                                await Super_column.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.Struct)
                            {
                                Counter_column = new CounterColumn();
                                await Counter_column.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.Struct)
                            {
                                Counter_super_column = new CounterSuperColumn();
                                await Counter_super_column.ReadAsync(iprot, cancellationToken);
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
                var struc = new TStruct("ColumnOrSuperColumn");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Column != null && __isset.column)
                {
                    field.Name = "column";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Super_column != null && __isset.super_column)
                {
                    field.Name = "super_column";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Super_column.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Counter_column != null && __isset.counter_column)
                {
                    field.Name = "counter_column";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Counter_column.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Counter_super_column != null && __isset.counter_super_column)
                {
                    field.Name = "counter_super_column";
                    field.Type = TType.Struct;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Counter_super_column.WriteAsync(oprot, cancellationToken);
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
            var sb = new StringBuilder("ColumnOrSuperColumn(");
            bool __first = true;
            if (Column != null && __isset.column)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Column: ");
                sb.Append(Column == null ? "<null>" : Column.ToString());
            }
            if (Super_column != null && __isset.super_column)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Super_column: ");
                sb.Append(Super_column == null ? "<null>" : Super_column.ToString());
            }
            if (Counter_column != null && __isset.counter_column)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Counter_column: ");
                sb.Append(Counter_column == null ? "<null>" : Counter_column.ToString());
            }
            if (Counter_super_column != null && __isset.counter_super_column)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Counter_super_column: ");
                sb.Append(Counter_super_column == null ? "<null>" : Counter_super_column.ToString());
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool column;
            [DataMember] public bool super_column;
            [DataMember] public bool counter_column;
            [DataMember] public bool counter_super_column;
        }

        #region XmlSerializer support

        public bool ShouldSerializeColumn()
        {
            return __isset.column;
        }

        public bool ShouldSerializeSuper_column()
        {
            return __isset.super_column;
        }

        public bool ShouldSerializeCounter_column()
        {
            return __isset.counter_column;
        }

        public bool ShouldSerializeCounter_super_column()
        {
            return __isset.counter_super_column;
        }

        #endregion XmlSerializer support
    }
}