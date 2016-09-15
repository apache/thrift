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
    ///     A Mutation is either an insert (represented by filling column_or_supercolumn) or a deletion (represented by filling
    ///     the deletion attribute).
    ///     @param column_or_supercolumn. An insert to a column or supercolumn (possibly counter column or supercolumn)
    ///     @param deletion. A deletion of a column or supercolumn
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class Mutation : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private ColumnOrSuperColumn _column_or_supercolumn;
        private Deletion _deletion;

        public Mutation()
        {
        }

        [DataMember(Order = 0)]
        public ColumnOrSuperColumn Column_or_supercolumn
        {
            get { return _column_or_supercolumn; }
            set
            {
                __isset.column_or_supercolumn = true;
                this._column_or_supercolumn = value;
            }
        }

        [DataMember(Order = 0)]
        public Deletion Deletion
        {
            get { return _deletion; }
            set
            {
                __isset.deletion = true;
                this._deletion = value;
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
                                Column_or_supercolumn = new ColumnOrSuperColumn();
                                await Column_or_supercolumn.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.Struct)
                            {
                                Deletion = new Deletion();
                                await Deletion.ReadAsync(iprot, cancellationToken);
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
                var struc = new TStruct("Mutation");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Column_or_supercolumn != null && __isset.column_or_supercolumn)
                {
                    field.Name = "column_or_supercolumn";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_or_supercolumn.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Deletion != null && __isset.deletion)
                {
                    field.Name = "deletion";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Deletion.WriteAsync(oprot, cancellationToken);
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
            var sb = new StringBuilder("Mutation(");
            bool __first = true;
            if (Column_or_supercolumn != null && __isset.column_or_supercolumn)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Column_or_supercolumn: ");
                sb.Append(Column_or_supercolumn == null ? "<null>" : Column_or_supercolumn.ToString());
            }
            if (Deletion != null && __isset.deletion)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Deletion: ");
                sb.Append(Deletion == null ? "<null>" : Deletion.ToString());
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool column_or_supercolumn;
            [DataMember] public bool deletion;
        }

        #region XmlSerializer support

        public bool ShouldSerializeColumn_or_supercolumn()
        {
            return __isset.column_or_supercolumn;
        }

        public bool ShouldSerializeDeletion()
        {
            return __isset.deletion;
        }

        #endregion XmlSerializer support
    }
}