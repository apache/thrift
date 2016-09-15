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
    ///     The semantics of start keys and tokens are slightly different.
    ///     Keys are start-inclusive; tokens are start-exclusive.  Token
    ///     ranges may also wrap -- that is, the end token may be less
    ///     than the start one.  Thus, a range from keyX to keyX is a
    ///     one-element range, but a range from tokenY to tokenY is the
    ///     full ring.
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class KeyRange : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private byte[] _end_key;
        private string _end_token;
        private byte[] _start_key;
        private string _start_token;

        public KeyRange()
        {
            this.Count = 100;
        }

        public KeyRange(int count) : this()
        {
            this.Count = count;
        }

        [DataMember(Order = 0)]
        public byte[] Start_key
        {
            get { return _start_key; }
            set
            {
                __isset.start_key = true;
                this._start_key = value;
            }
        }

        [DataMember(Order = 0)]
        public byte[] End_key
        {
            get { return _end_key; }
            set
            {
                __isset.end_key = true;
                this._end_key = value;
            }
        }

        [DataMember(Order = 0)]
        public string Start_token
        {
            get { return _start_token; }
            set
            {
                __isset.start_token = true;
                this._start_token = value;
            }
        }

        [DataMember(Order = 0)]
        public string End_token
        {
            get { return _end_token; }
            set
            {
                __isset.end_token = true;
                this._end_token = value;
            }
        }

        [DataMember(Order = 0)]
        public int Count { get; set; }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
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
                                Start_key = await iprot.ReadBinaryAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.String)
                            {
                                End_key = await iprot.ReadBinaryAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.String)
                            {
                                Start_token = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.String)
                            {
                                End_token = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 5:
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
                var struc = new TStruct("KeyRange");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Start_key != null && __isset.start_key)
                {
                    field.Name = "start_key";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Start_key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (End_key != null && __isset.end_key)
                {
                    field.Name = "end_key";
                    field.Type = TType.String;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(End_key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Start_token != null && __isset.start_token)
                {
                    field.Name = "start_token";
                    field.Type = TType.String;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Start_token, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (End_token != null && __isset.end_token)
                {
                    field.Name = "end_token";
                    field.Type = TType.String;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(End_token, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                field.Name = "count";
                field.Type = TType.I32;
                field.ID = 5;
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
            var sb = new StringBuilder("KeyRange(");
            bool __first = true;
            if (Start_key != null && __isset.start_key)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Start_key: ");
                sb.Append(Start_key);
            }
            if (End_key != null && __isset.end_key)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("End_key: ");
                sb.Append(End_key);
            }
            if (Start_token != null && __isset.start_token)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Start_token: ");
                sb.Append(Start_token);
            }
            if (End_token != null && __isset.end_token)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("End_token: ");
                sb.Append(End_token);
            }
            if (!__first)
            {
                sb.Append(", ");
            }
            sb.Append("Count: ");
            sb.Append(Count);
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool start_key;
            [DataMember] public bool end_key;
            [DataMember] public bool start_token;
            [DataMember] public bool end_token;
        }

        #region XmlSerializer support

        public bool ShouldSerializeStart_key()
        {
            return __isset.start_key;
        }

        public bool ShouldSerializeEnd_key()
        {
            return __isset.end_key;
        }

        public bool ShouldSerializeStart_token()
        {
            return __isset.start_token;
        }

        public bool ShouldSerializeEnd_token()
        {
            return __isset.end_token;
        }

        #endregion XmlSerializer support
    }
}