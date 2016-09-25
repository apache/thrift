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


namespace Thrift.Samples
{
    /// <summary>
    ///     Structs can also be exceptions, if they are nasty.
    /// </summary>
    public partial class InvalidOperation : TException, TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private int _WhatOp;
        private string _Why;

        public InvalidOperation()
        {
        }

        [DataMember(Order = 0)]
        public int WhatOp
        {
            get { return _WhatOp; }
            set
            {
                __isset.WhatOp = true;
                this._WhatOp = value;
            }
        }

        [DataMember(Order = 0)]
        public string Why
        {
            get { return _Why; }
            set
            {
                __isset.Why = true;
                this._Why = value;
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
                            if (field.Type == TType.I32)
                            {
                                WhatOp = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.String)
                            {
                                Why = await iprot.ReadStringAsync(cancellationToken);
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
                var struc = new TStruct("InvalidOperation");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (__isset.WhatOp)
                {
                    field.Name = "WhatOp";
                    field.Type = TType.I32;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(WhatOp, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Why != null && __isset.Why)
                {
                    field.Name = "Why";
                    field.Type = TType.String;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Why, cancellationToken);
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
            var sb = new StringBuilder("InvalidOperation(");
            bool __first = true;
            if (__isset.WhatOp)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("WhatOp: ");
                sb.Append(WhatOp);
            }
            if (Why != null && __isset.Why)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Why: ");
                sb.Append(Why);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool WhatOp;
            [DataMember] public bool Why;
        }

        #region XmlSerializer support

        public bool ShouldSerializeWhatOp()
        {
            return __isset.WhatOp;
        }

        public bool ShouldSerializeWhy()
        {
            return __isset.Why;
        }

        #endregion XmlSerializer support
    }


    [DataContract]
    public partial class InvalidOperationFault
    {
        private int _WhatOp;
        private string _Why;

        [DataMember(Order = 0)]
        public int WhatOp
        {
            get { return _WhatOp; }
            set { this._WhatOp = value; }
        }

        [DataMember(Order = 0)]
        public string Why
        {
            get { return _Why; }
            set { this._Why = value; }
        }
    }
}