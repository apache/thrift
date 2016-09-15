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
    public partial class VersioningTestV2 : TBase
    {
        [DataMember(Order = 1)] public Isset __isset;

        private int _begin_in_both;
        private int _end_in_both;
        private sbyte _newbyte;
        private double _newdouble;
        private int _newint;
        private List<int> _newlist;
        private long _newlong;
        private Dictionary<int, int> _newmap;
        private THashSet<int> _newset;
        private short _newshort;
        private string _newstring;
        private Bonk _newstruct;

        public VersioningTestV2()
        {
        }

        [DataMember(Order = 0)]
        public int Begin_in_both
        {
            get { return _begin_in_both; }
            set
            {
                __isset.begin_in_both = true;
                this._begin_in_both = value;
            }
        }

        [DataMember(Order = 0)]
        public int Newint
        {
            get { return _newint; }
            set
            {
                __isset.newint = true;
                this._newint = value;
            }
        }

        [DataMember(Order = 0)]
        public sbyte Newbyte
        {
            get { return _newbyte; }
            set
            {
                __isset.newbyte = true;
                this._newbyte = value;
            }
        }

        [DataMember(Order = 0)]
        public short Newshort
        {
            get { return _newshort; }
            set
            {
                __isset.newshort = true;
                this._newshort = value;
            }
        }

        [DataMember(Order = 0)]
        public long Newlong
        {
            get { return _newlong; }
            set
            {
                __isset.newlong = true;
                this._newlong = value;
            }
        }

        [DataMember(Order = 0)]
        public double Newdouble
        {
            get { return _newdouble; }
            set
            {
                __isset.newdouble = true;
                this._newdouble = value;
            }
        }

        [DataMember(Order = 0)]
        public Bonk Newstruct
        {
            get { return _newstruct; }
            set
            {
                __isset.newstruct = true;
                this._newstruct = value;
            }
        }

        [DataMember(Order = 0)]
        public List<int> Newlist
        {
            get { return _newlist; }
            set
            {
                __isset.newlist = true;
                this._newlist = value;
            }
        }

        [DataMember(Order = 0)]
        public THashSet<int> Newset
        {
            get { return _newset; }
            set
            {
                __isset.newset = true;
                this._newset = value;
            }
        }

        [DataMember(Order = 0)]
        public Dictionary<int, int> Newmap
        {
            get { return _newmap; }
            set
            {
                __isset.newmap = true;
                this._newmap = value;
            }
        }

        [DataMember(Order = 0)]
        public string Newstring
        {
            get { return _newstring; }
            set
            {
                __isset.newstring = true;
                this._newstring = value;
            }
        }

        [DataMember(Order = 0)]
        public int End_in_both
        {
            get { return _end_in_both; }
            set
            {
                __isset.end_in_both = true;
                this._end_in_both = value;
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
                                Begin_in_both = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 2:
                            if (field.Type == TType.I32)
                            {
                                Newint = await iprot.ReadI32Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 3:
                            if (field.Type == TType.Byte)
                            {
                                Newbyte = await iprot.ReadByteAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 4:
                            if (field.Type == TType.I16)
                            {
                                Newshort = await iprot.ReadI16Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 5:
                            if (field.Type == TType.I64)
                            {
                                Newlong = await iprot.ReadI64Async(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 6:
                            if (field.Type == TType.Double)
                            {
                                Newdouble = await iprot.ReadDoubleAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 7:
                            if (field.Type == TType.Struct)
                            {
                                Newstruct = new Bonk();
                                await Newstruct.ReadAsync(iprot, cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 8:
                            if (field.Type == TType.List)
                            {
                                {
                                    Newlist = new List<int>();
                                    TList _list44 = await iprot.ReadListBeginAsync(cancellationToken);
                                    for (int _i45 = 0; _i45 < _list44.Count; ++_i45)
                                    {
                                        int _elem46;
                                        _elem46 = await iprot.ReadI32Async(cancellationToken);
                                        Newlist.Add(_elem46);
                                    }
                                    await iprot.ReadListEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 9:
                            if (field.Type == TType.Set)
                            {
                                {
                                    Newset = new THashSet<int>();
                                    TSet _set47 = await iprot.ReadSetBeginAsync(cancellationToken);
                                    for (int _i48 = 0; _i48 < _set47.Count; ++_i48)
                                    {
                                        int _elem49;
                                        _elem49 = await iprot.ReadI32Async(cancellationToken);
                                        Newset.Add(_elem49);
                                    }
                                    await iprot.ReadSetEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 10:
                            if (field.Type == TType.Map)
                            {
                                {
                                    Newmap = new Dictionary<int, int>();
                                    TMap _map50 = await iprot.ReadMapBeginAsync(cancellationToken);
                                    for (int _i51 = 0; _i51 < _map50.Count; ++_i51)
                                    {
                                        int _key52;
                                        int _val53;
                                        _key52 = await iprot.ReadI32Async(cancellationToken);
                                        _val53 = await iprot.ReadI32Async(cancellationToken);
                                        Newmap[_key52] = _val53;
                                    }
                                    await iprot.ReadMapEndAsync(cancellationToken);
                                }
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 11:
                            if (field.Type == TType.String)
                            {
                                Newstring = await iprot.ReadStringAsync(cancellationToken);
                            }
                            else
                            {
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                            }
                            break;
                        case 12:
                            if (field.Type == TType.I32)
                            {
                                End_in_both = await iprot.ReadI32Async(cancellationToken);
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
                var struc = new TStruct("VersioningTestV2");
                await oprot.WriteStructBeginAsync(struc, cancellationToken);
                var field = new TField();
                if (__isset.begin_in_both)
                {
                    field.Name = "begin_in_both";
                    field.Type = TType.I32;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Begin_in_both, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.newint)
                {
                    field.Name = "newint";
                    field.Type = TType.I32;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Newint, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.newbyte)
                {
                    field.Name = "newbyte";
                    field.Type = TType.Byte;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteByteAsync(Newbyte, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.newshort)
                {
                    field.Name = "newshort";
                    field.Type = TType.I16;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI16Async(Newshort, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.newlong)
                {
                    field.Name = "newlong";
                    field.Type = TType.I64;
                    field.ID = 5;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI64Async(Newlong, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.newdouble)
                {
                    field.Name = "newdouble";
                    field.Type = TType.Double;
                    field.ID = 6;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteDoubleAsync(Newdouble, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Newstruct != null && __isset.newstruct)
                {
                    field.Name = "newstruct";
                    field.Type = TType.Struct;
                    field.ID = 7;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Newstruct.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Newlist != null && __isset.newlist)
                {
                    field.Name = "newlist";
                    field.Type = TType.List;
                    field.ID = 8;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.I32, Newlist.Count), cancellationToken);
                        foreach (int _iter54 in Newlist)
                        {
                            await oprot.WriteI32Async(_iter54, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Newset != null && __isset.newset)
                {
                    field.Name = "newset";
                    field.Type = TType.Set;
                    field.ID = 9;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteSetBeginAsync(new TSet(TType.I32, Newset.Count), cancellationToken);
                        foreach (int _iter55 in Newset)
                        {
                            await oprot.WriteI32Async(_iter55, cancellationToken);
                        }
                        await oprot.WriteSetEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Newmap != null && __isset.newmap)
                {
                    field.Name = "newmap";
                    field.Type = TType.Map;
                    field.ID = 10;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteMapBeginAsync(new TMap(TType.I32, TType.I32, Newmap.Count), cancellationToken);
                        foreach (int _iter56 in Newmap.Keys)
                        {
                            await oprot.WriteI32Async(_iter56, cancellationToken);
                            await oprot.WriteI32Async(Newmap[_iter56], cancellationToken);
                        }
                        await oprot.WriteMapEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (Newstring != null && __isset.newstring)
                {
                    field.Name = "newstring";
                    field.Type = TType.String;
                    field.ID = 11;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Newstring, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                }
                if (__isset.end_in_both)
                {
                    field.Name = "end_in_both";
                    field.Type = TType.I32;
                    field.ID = 12;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(End_in_both, cancellationToken);
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
            var sb = new StringBuilder("VersioningTestV2(");
            bool __first = true;
            if (__isset.begin_in_both)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Begin_in_both: ");
                sb.Append(Begin_in_both);
            }
            if (__isset.newint)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newint: ");
                sb.Append(Newint);
            }
            if (__isset.newbyte)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newbyte: ");
                sb.Append(Newbyte);
            }
            if (__isset.newshort)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newshort: ");
                sb.Append(Newshort);
            }
            if (__isset.newlong)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newlong: ");
                sb.Append(Newlong);
            }
            if (__isset.newdouble)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newdouble: ");
                sb.Append(Newdouble);
            }
            if (Newstruct != null && __isset.newstruct)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newstruct: ");
                sb.Append(Newstruct == null ? "<null>" : Newstruct.ToString());
            }
            if (Newlist != null && __isset.newlist)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newlist: ");
                sb.Append(Newlist);
            }
            if (Newset != null && __isset.newset)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newset: ");
                sb.Append(Newset);
            }
            if (Newmap != null && __isset.newmap)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newmap: ");
                sb.Append(Newmap);
            }
            if (Newstring != null && __isset.newstring)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("Newstring: ");
                sb.Append(Newstring);
            }
            if (__isset.end_in_both)
            {
                if (!__first)
                {
                    sb.Append(", ");
                }
                __first = false;
                sb.Append("End_in_both: ");
                sb.Append(End_in_both);
            }
            sb.Append(")");
            return sb.ToString();
        }

        [DataContract]
        public struct Isset
        {
            [DataMember] public bool begin_in_both;
            [DataMember] public bool newint;
            [DataMember] public bool newbyte;
            [DataMember] public bool newshort;
            [DataMember] public bool newlong;
            [DataMember] public bool newdouble;
            [DataMember] public bool newstruct;
            [DataMember] public bool newlist;
            [DataMember] public bool newset;
            [DataMember] public bool newmap;
            [DataMember] public bool newstring;
            [DataMember] public bool end_in_both;
        }

        #region XmlSerializer support

        public bool ShouldSerializeBegin_in_both()
        {
            return __isset.begin_in_both;
        }

        public bool ShouldSerializeNewint()
        {
            return __isset.newint;
        }

        public bool ShouldSerializeNewbyte()
        {
            return __isset.newbyte;
        }

        public bool ShouldSerializeNewshort()
        {
            return __isset.newshort;
        }

        public bool ShouldSerializeNewlong()
        {
            return __isset.newlong;
        }

        public bool ShouldSerializeNewdouble()
        {
            return __isset.newdouble;
        }

        public bool ShouldSerializeNewstruct()
        {
            return __isset.newstruct;
        }

        public bool ShouldSerializeNewlist()
        {
            return __isset.newlist;
        }

        public bool ShouldSerializeNewset()
        {
            return __isset.newset;
        }

        public bool ShouldSerializeNewmap()
        {
            return __isset.newmap;
        }

        public bool ShouldSerializeNewstring()
        {
            return __isset.newstring;
        }

        public bool ShouldSerializeEnd_in_both()
        {
            return __isset.end_in_both;
        }

        #endregion XmlSerializer support
    }
}