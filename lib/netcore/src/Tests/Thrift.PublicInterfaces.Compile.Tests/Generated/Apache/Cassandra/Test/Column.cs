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
    ///     Basic unit of data within a ColumnFamily.
    ///     @param name, the name by which this column is set and retrieved.  Maximum 64KB long.
    ///     @param value. The data associated with the name.  Maximum 2GB long, but in practice you should limit it to small
    ///     numbers of MB (since Thrift must read the full value into memory to operate on it).
    ///     @param timestamp. The timestamp is used for conflict detection/resolution when two columns with same name need to
    ///     be compared.
    ///     @param ttl. An optional, positive delay (in seconds) after which the column will be automatically deleted.
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class Column : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private long _timestamp;
        private int _ttl;
        private byte[] _value;

        public Column()
        {
        }

        public Column(byte[] name) : this()
        {
            this.Name = name;
        }

        [DataMember(Order = 0)]
        public byte[] Name { get; set; }

        [DataMember(Order = 0)]
        public byte[] Value
        {
            get { return _value; }
            set
            {
                __isset.@value = true;
                this._value = value;
            }
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
        public int Ttl
        {
            get { return _ttl; }
            set
            {
                __isset.ttl = true;
                this._ttl = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_name = false;
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
                            if (field.Type == TType.String)
                            {
                                Value = await iprot.ReadBinaryAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.I64)
                            {
                                Timestamp = await iprot.ReadI64Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.I32)
                            {
                                Ttl = await iprot.ReadI32Async(cancellationToken);
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
                var struc = new TStruct("Column");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "name";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteBinaryAsync(Name, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                if (Value != null && __isset.@value)
                {
                    field.Name = "value";
                    field.Type = TType.String;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Value, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.timestamp)
                {
                    field.Name = "timestamp";
                    field.Type = TType.I64;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI64Async(Timestamp, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.ttl)
                {
                    field.Name = "ttl";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Ttl, cancellationToken);
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
            var sb = new StringBuilder("Column(");
            sb.Append(", Name: ");
            sb.Append(Name);
            if (Value != null && __isset.@value)
            {
                sb.Append(", Value: ");
                sb.Append(Value);
            }
            if (__isset.timestamp)
            {
                sb.Append(", Timestamp: ");
                sb.Append(Timestamp);
            }
            if (__isset.ttl)
            {
                sb.Append(", Ttl: ");
                sb.Append(Ttl);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool @value;
            [DataMember] public bool timestamp;
            [DataMember] public bool ttl;
        }

        #region XmlSerializer support

        public bool ShouldSerializeValue()
        {
            return __isset.@value;
        }

        public bool ShouldSerializeTimestamp()
        {
            return __isset.timestamp;
        }

        public bool ShouldSerializeTtl()
        {
            return __isset.ttl;
        }

        #endregion XmlSerializer support
    }
}