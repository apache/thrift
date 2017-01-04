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
    public partial class LargeDeltas : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private THashSet<string> _a_set2500;
        private Bools _b1;
        private Bools _b10;
        private Bools _b100;
        private Bools _b1000;
        private List<int> _big_numbers;
        private bool _check_false;
        private bool _check_true;
        private VersioningTestV2 _vertwo2000;
        private VersioningTestV2 _vertwo3000;

        public LargeDeltas()
        {
        }

        [DataMember(Order = 0)]
        public Bools B1
        {
            get { return _b1; }
            set
            {
                __isset.b1 = true;
                this._b1 = value;
            }
        }

        [DataMember(Order = 0)]
        public Bools B10
        {
            get { return _b10; }
            set
            {
                __isset.b10 = true;
                this._b10 = value;
            }
        }

        [DataMember(Order = 0)]
        public Bools B100
        {
            get { return _b100; }
            set
            {
                __isset.b100 = true;
                this._b100 = value;
            }
        }

        [DataMember(Order = 0)]
        public bool Check_true
        {
            get { return _check_true; }
            set
            {
                __isset.check_true = true;
                this._check_true = value;
            }
        }

        [DataMember(Order = 0)]
        public Bools B1000
        {
            get { return _b1000; }
            set
            {
                __isset.b1000 = true;
                this._b1000 = value;
            }
        }

        [DataMember(Order = 0)]
        public bool Check_false
        {
            get { return _check_false; }
            set
            {
                __isset.check_false = true;
                this._check_false = value;
            }
        }

        [DataMember(Order = 0)]
        public VersioningTestV2 Vertwo2000
        {
            get { return _vertwo2000; }
            set
            {
                __isset.vertwo2000 = true;
                this._vertwo2000 = value;
            }
        }

        [DataMember(Order = 0)]
        public THashSet<string> A_set2500
        {
            get { return _a_set2500; }
            set
            {
                __isset.a_set2500 = true;
                this._a_set2500 = value;
            }
        }

        [DataMember(Order = 0)]
        public VersioningTestV2 Vertwo3000
        {
            get { return _vertwo3000; }
            set
            {
                __isset.vertwo3000 = true;
                this._vertwo3000 = value;
            }
        }

        [DataMember(Order = 0)]
        public List<int> Big_numbers
        {
            get { return _big_numbers; }
            set
            {
                __isset.big_numbers = true;
                this._big_numbers = value;
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
                                B1 = new Bools();
                                await B1.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 10:
                            if (field.Type == TType.Struct)
                            {
                                B10 = new Bools();
                                await B10.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 100:
                            if (field.Type == TType.Struct)
                            {
                                B100 = new Bools();
                                await B100.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 500:
                            if (field.Type == TType.Bool)
                            {
                                Check_true = await iprot.ReadBoolAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 1000:
                            if (field.Type == TType.Struct)
                            {
                                B1000 = new Bools();
                                await B1000.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 1500:
                            if (field.Type == TType.Bool)
                            {
                                Check_false = await iprot.ReadBoolAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2000:
                            if (field.Type == TType.Struct)
                            {
                                Vertwo2000 = new VersioningTestV2();
                                await Vertwo2000.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2500:
                            if (field.Type == TType.Set)
                            {
                                {
                                    A_set2500 = new THashSet<string>();
                                    TSet _set70 = await iprot.ReadSetBeginAsync(cancellationToken);
                                    for (int _i71 = 0; _i71 < _set70.Count; ++_i71)
                                    {
                                        string _elem72;
                                        _elem72 = await iprot.ReadStringAsync(cancellationToken);
                                        A_set2500.Add(_elem72);
                                    }
                                    await iprot.ReadSetEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3000:
                            if (field.Type == TType.Struct)
                            {
                                Vertwo3000 = new VersioningTestV2();
                                await Vertwo3000.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4000:
                            if (field.Type == TType.List)
                            {
                                {
                                    Big_numbers = new List<int>();
                                    TList _list73 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i74 = 0; _i74 < _list73.Count; ++_i74)
                                    {
                                        int _elem75;
                                        _elem75 = await iprot.ReadI32Async(cancellationToken);
                                        Big_numbers.Add(_elem75);
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
                var struc = new TStruct("LargeDeltas");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (B1 != null && __isset.b1)
                {
                    field.Name = "b1";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await B1.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (B10 != null && __isset.b10)
                {
                    field.Name = "b10";
                    field.Type = TType.Struct;
                    field.ID = 10;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await B10.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (B100 != null && __isset.b100)
                {
                    field.Name = "b100";
                    field.Type = TType.Struct;
                    field.ID = 100;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await B100.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.check_true)
                {
                    field.Name = "check_true";
                    field.Type = TType.Bool;
                    field.ID = 500;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBoolAsync(Check_true, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (B1000 != null && __isset.b1000)
                {
                    field.Name = "b1000";
                    field.Type = TType.Struct;
                    field.ID = 1000;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await B1000.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.check_false)
                {
                    field.Name = "check_false";
                    field.Type = TType.Bool;
                    field.ID = 1500;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBoolAsync(Check_false, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Vertwo2000 != null && __isset.vertwo2000)
                {
                    field.Name = "vertwo2000";
                    field.Type = TType.Struct;
                    field.ID = 2000;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Vertwo2000.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (A_set2500 != null && __isset.a_set2500)
                {
                    field.Name = "a_set2500";
                    field.Type = TType.Set;
                    field.ID = 2500;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteSetBeginAsync(new TSet(TType.String, A_set2500.Count), cancellationToken);
                        foreach (string _iter76 in A_set2500)
                        {
                            await oprot.WriteStringAsync(_iter76, cancellationToken);
                        }
                        await oprot.WriteSetEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Vertwo3000 != null && __isset.vertwo3000)
                {
                    field.Name = "vertwo3000";
                    field.Type = TType.Struct;
                    field.ID = 3000;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Vertwo3000.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Big_numbers != null && __isset.big_numbers)
                {
                    field.Name = "big_numbers";
                    field.Type = TType.List;
                    field.ID = 4000;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.I32, Big_numbers.Count), cancellationToken);
                        foreach (int _iter77 in Big_numbers)
                        {
                            await oprot.WriteI32Async(_iter77, cancellationToken);
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
            var sb = new StringBuilder("LargeDeltas(");
            bool __first = true;
            if (B1 != null && __isset.b1)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("B1: ");
                sb.Append(B1 == null ? "<null>" : B1.ToString());
            }
            if (B10 != null && __isset.b10)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("B10: ");
                sb.Append(B10 == null ? "<null>" : B10.ToString());
            }
            if (B100 != null && __isset.b100)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("B100: ");
                sb.Append(B100 == null ? "<null>" : B100.ToString());
            }
            if (__isset.check_true)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Check_true: ");
                sb.Append(Check_true);
            }
            if (B1000 != null && __isset.b1000)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("B1000: ");
                sb.Append(B1000 == null ? "<null>" : B1000.ToString());
            }
            if (__isset.check_false)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Check_false: ");
                sb.Append(Check_false);
            }
            if (Vertwo2000 != null && __isset.vertwo2000)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Vertwo2000: ");
                sb.Append(Vertwo2000 == null ? "<null>" : Vertwo2000.ToString());
            }
            if (A_set2500 != null && __isset.a_set2500)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("A_set2500: ");
                sb.Append(A_set2500);
            }
            if (Vertwo3000 != null && __isset.vertwo3000)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Vertwo3000: ");
                sb.Append(Vertwo3000 == null ? "<null>" : Vertwo3000.ToString());
            }
            if (Big_numbers != null && __isset.big_numbers)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Big_numbers: ");
                sb.Append(Big_numbers);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool b1;
            [DataMember] public bool b10;
            [DataMember] public bool b100;
            [DataMember] public bool check_true;
            [DataMember] public bool b1000;
            [DataMember] public bool check_false;
            [DataMember] public bool vertwo2000;
            [DataMember] public bool a_set2500;
            [DataMember] public bool vertwo3000;
            [DataMember] public bool big_numbers;
        }

        #region XmlSerializer support

        public bool ShouldSerializeB1()
        {
            return __isset.b1;
        }

        public bool ShouldSerializeB10()
        {
            return __isset.b10;
        }

        public bool ShouldSerializeB100()
        {
            return __isset.b100;
        }

        public bool ShouldSerializeCheck_true()
        {
            return __isset.check_true;
        }

        public bool ShouldSerializeB1000()
        {
            return __isset.b1000;
        }

        public bool ShouldSerializeCheck_false()
        {
            return __isset.check_false;
        }

        public bool ShouldSerializeVertwo2000()
        {
            return __isset.vertwo2000;
        }

        public bool ShouldSerializeA_set2500()
        {
            return __isset.a_set2500;
        }

        public bool ShouldSerializeVertwo3000()
        {
            return __isset.vertwo3000;
        }

        public bool ShouldSerializeBig_numbers()
        {
            return __isset.big_numbers;
        }

        #endregion XmlSerializer support
    }
}