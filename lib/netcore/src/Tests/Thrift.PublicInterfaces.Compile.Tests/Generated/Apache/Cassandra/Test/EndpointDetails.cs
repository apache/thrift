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
    public partial class EndpointDetails : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private string _datacenter;
        private string _host;
        private string _rack;

        public EndpointDetails()
        {
        }

        [DataMember(Order = 0)]
        public string Host
        {
            get { return _host; }
            set
            {
                __isset.host = true;
                this._host = value;
            }
        }

        [DataMember(Order = 0)]
        public string Datacenter
        {
            get { return _datacenter; }
            set
            {
                __isset.datacenter = true;
                this._datacenter = value;
            }
        }

        [DataMember(Order = 0)]
        public string Rack
        {
            get { return _rack; }
            set
            {
                __isset.rack = true;
                this._rack = value;
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
                            if (field.Type == TType.String)
                            {
                                Host = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.String)
                            {
                                Datacenter = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.String)
                            {
                                Rack = await iprot.ReadStringAsync(cancellationToken);
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
                var struc = new TStruct("EndpointDetails");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Host != null && __isset.host)
                {
                    field.Name = "host";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Host, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Datacenter != null && __isset.datacenter)
                {
                    field.Name = "datacenter";
                    field.Type = TType.String;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Datacenter, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Rack != null && __isset.rack)
                {
                    field.Name = "rack";
                    field.Type = TType.String;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Rack, cancellationToken);
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
            var sb = new StringBuilder("EndpointDetails(");
            bool __first = true;
            if (Host != null && __isset.host)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Host: ");
                sb.Append(Host);
            }
            if (Datacenter != null && __isset.datacenter)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Datacenter: ");
                sb.Append(Datacenter);
            }
            if (Rack != null && __isset.rack)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Rack: ");
                sb.Append(Rack);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool host;
            [DataMember] public bool datacenter;
            [DataMember] public bool rack;
        }

        #region XmlSerializer support

        public bool ShouldSerializeHost()
        {
            return __isset.host;
        }

        public bool ShouldSerializeDatacenter()
        {
            return __isset.datacenter;
        }

        public bool ShouldSerializeRack()
        {
            return __isset.rack;
        }

        #endregion XmlSerializer support
    }
}