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
    public partial class OneField : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private EmptyStruct _field;

        public OneField()
        {
        }

        [DataMember(Order = 0)]
        public EmptyStruct Field
        {
            get { return _field; }
            set
            {
                __isset.field = true;
                this._field = value;
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
                                Field = new EmptyStruct();
                                await Field.ReadAsync(iprot, cancellationToken);
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
                var struc = new TStruct("OneField");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (Field != null && __isset.field)
                {
                    field.Name = "field";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Field.WriteAsync(oprot, cancellationToken);
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

        #region XmlSerializer support

        public bool ShouldSerializeField()
        {
            return __isset.field;
        }

        #endregion XmlSerializer support

        public override string ToString()
        {
            var sb = new StringBuilder("OneField(");
            bool __first = true;
            if (Field != null && __isset.field)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Field: ");
                sb.Append(Field == null ? "<null>" : Field.ToString());
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool field;
        }
    }
}