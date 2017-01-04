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


namespace ThriftAsync.Test
{
    [DataContract(Namespace = "")]
    public partial class ListTypeVersioningV2 : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private string _hello;
        private List<string> _strings;

        public ListTypeVersioningV2()
        {
        }

        [DataMember(Order = 0)]
        public List<string> Strings
        {
            get { return _strings; }
            set
            {
                __isset.strings = true;
                this._strings = value;
            }
        }

        [DataMember(Order = 0)]
        public string Hello
        {
            get { return _hello; }
            set
            {
                __isset.hello = true;
                this._hello = value;
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
                                    Strings = new List<string>();
                                    TList _list61 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i62 = 0; _i62 < _list61.Count; ++_i62)
                                    {
                                        string _elem63;
                                        _elem63 = await iprot.ReadStringAsync(cancellationToken);
                                        Strings.Add(_elem63);
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
                            if (field.Type == TType.String)
                            {
                                Hello = await iprot.ReadStringAsync(cancellationToken);
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
                var struc = new TStruct("ListTypeVersioningV2");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Strings != null && __isset.strings)
                {
                    field.Name = "strings";
                    field.Type = TType.List;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.String, Strings.Count), cancellationToken);
                        foreach (string _iter64 in Strings)
                        {
                            await oprot.WriteStringAsync(_iter64, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Hello != null && __isset.hello)
                {
                    field.Name = "hello";
                    field.Type = TType.String;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Hello, cancellationToken);
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
            var sb = new StringBuilder("ListTypeVersioningV2(");
            bool __first = true;
            if (Strings != null && __isset.strings)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Strings: ");
                sb.Append(Strings);
            }
            if (Hello != null && __isset.hello)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Hello: ");
                sb.Append(Hello);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool strings;
            [DataMember] public bool hello;
        }

        #region XmlSerializer support

        public bool ShouldSerializeStrings()
        {
            return __isset.strings;
        }

        public bool ShouldSerializeHello()
        {
            return __isset.hello;
        }

        #endregion XmlSerializer support
    }
}