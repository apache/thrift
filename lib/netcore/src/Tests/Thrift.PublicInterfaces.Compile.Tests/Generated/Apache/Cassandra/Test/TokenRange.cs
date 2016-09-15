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
    ///     A TokenRange describes part of the Cassandra ring, it is a mapping from a range to
    ///     endpoints responsible for that range.
    ///     @param start_token The first token in the range
    ///     @param end_token The last token in the range
    ///     @param endpoints The endpoints responsible for the range (listed by their configured listen_address)
    ///     @param rpc_endpoints The endpoints responsible for the range (listed by their configured rpc_address)
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class TokenRange : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private List<EndpointDetails> _endpoint_details;
        private List<string> _rpc_endpoints;

        public TokenRange()
        {
        }

        public TokenRange(string start_token, string end_token, List<string> endpoints) : this()
        {
            this.Start_token = start_token;
            this.End_token = end_token;
            this.Endpoints = endpoints;
        }

        [DataMember(Order = 0)]
        public string Start_token { get; set; }

        [DataMember(Order = 0)]
        public string End_token { get; set; }

        [DataMember(Order = 0)]
        public List<string> Endpoints { get; set; }

        [DataMember(Order = 0)]
        public List<string> Rpc_endpoints
        {
            get { return _rpc_endpoints; }
            set
            {
                __isset.rpc_endpoints = true;
                this._rpc_endpoints = value;
            }
        }

        [DataMember(Order = 0)]
        public List<EndpointDetails> Endpoint_details
        {
            get { return _endpoint_details; }
            set
            {
                __isset.endpoint_details = true;
                this._endpoint_details = value;
            }
        }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_start_token = false;
                bool isset_end_token = false;
                bool isset_endpoints = false;
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
                                Start_token = await iprot.ReadStringAsync(cancellationToken);
                                isset_start_token = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.String)
                            {
                                End_token = await iprot.ReadStringAsync(cancellationToken);
                                isset_end_token = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.List)
                            {
                                {
                                    Endpoints = new List<string>();
                                    TList _list20 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i21 = 0; _i21 < _list20.Count; ++_i21)
                                    {
                                        string _elem22;
                                        _elem22 = await iprot.ReadStringAsync(cancellationToken);
                                        Endpoints.Add(_elem22);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                                isset_endpoints = true;
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.List)
                            {
                                {
                                    Rpc_endpoints = new List<string>();
                                    TList _list23 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i24 = 0; _i24 < _list23.Count; ++_i24)
                                    {
                                        string _elem25;
                                        _elem25 = await iprot.ReadStringAsync(cancellationToken);
                                        Rpc_endpoints.Add(_elem25);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 5:
                            if (field.Type == TType.List)
                            {
                                {
                                    Endpoint_details = new List<EndpointDetails>();
                                    TList _list26 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i27 = 0; _i27 < _list26.Count; ++_i27)
                                    {
                                        EndpointDetails _elem28;
                                        _elem28 = new EndpointDetails();
                                        await _elem28.ReadAsync(iprot, cancellationToken);
                                        Endpoint_details.Add(_elem28);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
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
                if (!isset_start_token)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_end_token)
                {
                    throw new TProtocolException(TProtocolException.INVALID_DATA);
                }
                if (!isset_endpoints)
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
                var struc = new TStruct("TokenRange");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "start_token";
                field.Type = TType.String;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(Start_token, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "end_token";
                field.Type = TType.String;
                field.ID = 2;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                await oprot.WriteStringAsync(End_token, cancellationToken);
                await oprot.WriteFieldEndAsync(cancellationToken);
                field.Name = "endpoints";
                field.Type = TType.List;
                field.ID = 3;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                {
                    await oprot.WriteListBeginAsync(new TList(TType.String, Endpoints.Count), cancellationToken);
                    foreach (string _iter29 in Endpoints)
                    {
                        await oprot.WriteStringAsync(_iter29, cancellationToken);
                    }
                    await oprot.WriteListEndAsync(cancellationToken);
                }
                await oprot.WriteFieldEndAsync(cancellationToken);
                if (Rpc_endpoints != null && __isset.rpc_endpoints)
                {
                    field.Name = "rpc_endpoints";
                    field.Type = TType.List;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.String, Rpc_endpoints.Count), cancellationToken);
                        foreach (string _iter30 in Rpc_endpoints)
                        {
                            await oprot.WriteStringAsync(_iter30, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Endpoint_details != null && __isset.endpoint_details)
                {
                    field.Name = "endpoint_details";
                    field.Type = TType.List;
                    field.ID = 5;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteListBeginAsync(new TList(TType.Struct, Endpoint_details.Count), cancellationToken);
                        foreach (EndpointDetails _iter31 in Endpoint_details)
                        {
                            await _iter31.WriteAsync(oprot, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
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
            var sb = new StringBuilder("TokenRange(");
            sb.Append(", Start_token: ");
            sb.Append(Start_token);
            sb.Append(", End_token: ");
            sb.Append(End_token);
            sb.Append(", Endpoints: ");
            sb.Append(Endpoints);
            if (Rpc_endpoints != null && __isset.rpc_endpoints)
            {
                sb.Append(", Rpc_endpoints: ");
                sb.Append(Rpc_endpoints);
            }
            if (Endpoint_details != null && __isset.endpoint_details)
            {
                sb.Append(", Endpoint_details: ");
                sb.Append(Endpoint_details);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool rpc_endpoints;
            [DataMember] public bool endpoint_details;
        }

        #region XmlSerializer support

        public bool ShouldSerializeRpc_endpoints()
        {
            return __isset.rpc_endpoints;
        }

        public bool ShouldSerializeEndpoint_details()
        {
            return __isset.endpoint_details;
        }

        #endregion XmlSerializer support
    }
}