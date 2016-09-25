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
    ///     Authentication requests can contain any data, dependent on the IAuthenticator used
    /// </summary>
    [DataContract(Namespace = "")]
    public partial class AuthenticationRequest : TBase
    {
        public AuthenticationRequest()
        {
        }

        public AuthenticationRequest(Dictionary<string, string> credentials) : this()
        {
            this.Credentials = credentials;
        }

        [DataMember(Order = 0)]
        public Dictionary<string, string> Credentials { get; set; }

        public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
        {
            iprot.IncrementRecursionDepth();
            try
            {
                bool isset_credentials = false;
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
                            if (field.Type == TType.Map)
                            {
                                {
                                    Credentials = new Dictionary<string, string>();
                                    TMap _map32 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i33 = 0; _i33 < _map32.Count; ++_i33)
                                    {
                                        string _key34;
                                        string _val35;
                                        _key34 = await iprot.ReadStringAsync(cancellationToken);
                                        _val35 = await iprot.ReadStringAsync(cancellationToken);
                                        Credentials[_key34] = _val35;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
                                isset_credentials = true;
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
                if (!isset_credentials)
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
                var struc = new TStruct("AuthenticationRequest");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                field.Name = "credentials";
                field.Type = TType.Map;
                field.ID = 1;
                await oprot.WriteFieldBeginAsync(field, cancellationToken);
                {
                    await
                        oprot.WriteMapBeginAsync(new TMap(TType.String, TType.String, Credentials.Count),
                            cancellationToken);
                    foreach (string _iter36 in Credentials.Keys)
                    {
                        await oprot.WriteStringAsync(_iter36, cancellationToken);
                        await oprot.WriteStringAsync(Credentials[_iter36], cancellationToken);
                    }
                    await oprot.WriteMapEndAsync(cancellationToken);
                }
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
            var sb = new StringBuilder("AuthenticationRequest(");
            sb.Append(", Credentials: ");
            sb.Append(Credentials);
            sb.Append(")");
            return sb.ToString();
        }
    }
}