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
    ///     Note that the timestamp is only optional in case of counter deletion.
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class Deletion : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private SlicePredicate _predicate;
        private byte[] _super_column;
        private long _timestamp;

        public Deletion()
        {
        }

        [DataMember(Order = 0)]
        public long Timestamp
        {
            get { return _timestamp; }
            set
            {
                __isset.timestamp = true;
                this._timestamp = value;
            }
        }

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
        public SlicePredicate Predicate
        {
            get { return _predicate; }
            set
            {
                __isset.predicate = true;
                this._predicate = value;
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
                            if (field.Type == TType.I64)
                            {
                                Timestamp = await iprot.ReadI64Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.String)
                            {
                                Super_column = await iprot.ReadBinaryAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.Struct)
                            {
                                Predicate = new SlicePredicate();
                                await Predicate.ReadAsync(iprot, cancellationToken);
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
                var struc = new TStruct("Deletion");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (__isset.timestamp)
                {
                    field.Name = "timestamp";
                    field.Type = TType.I64;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI64Async(Timestamp, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Super_column != null && __isset.super_column)
                {
                    field.Name = "super_column";
                    field.Type = TType.String;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Super_column, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Predicate != null && __isset.predicate)
                {
                    field.Name = "predicate";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Predicate.WriteAsync(oprot, cancellationToken);
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
            var sb = new StringBuilder("Deletion(");
            bool __first = true;
            if (__isset.timestamp)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Timestamp: ");
                sb.Append(Timestamp);
            }
            if (Super_column != null && __isset.super_column)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Super_column: ");
                sb.Append(Super_column);
            }
            if (Predicate != null && __isset.predicate)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Predicate: ");
                sb.Append(Predicate == null ? "<null>" : Predicate.ToString());
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool timestamp;
            [DataMember] public bool super_column;
            [DataMember] public bool predicate;
        }

        #region XmlSerializer support

        public bool ShouldSerializeTimestamp()
        {
            return __isset.timestamp;
        }

        public bool ShouldSerializeSuper_column()
        {
            return __isset.super_column;
        }

        public bool ShouldSerializePredicate()
        {
            return __isset.predicate;
        }

        #endregion XmlSerializer support
    }
}