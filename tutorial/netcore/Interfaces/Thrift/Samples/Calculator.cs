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
    public partial class Calculator
    {
        /// <summary>
        ///     Ahh, now onto the cool part, defining a service. Services just need a name
        ///     and can optionally inherit from another service using the extends keyword.
        /// </summary>
        [ServiceContract(Namespace = "")]
        public interface IAsync : SharedService.IAsync
        {
            /// <summary>
            ///     A method definition looks like C code. It has a return type, arguments,
            ///     and optionally a list of exceptions that it may throw. Note that argument
            ///     lists and exception lists are specified using the exact same syntax as
            ///     field lists in struct or exception definitions.
            /// </summary>
            [OperationContract]
            Task PingAsync(CancellationToken cancellationToken);

            [OperationContract]
            Task<int> @AddAsync(int num1, int num2, CancellationToken cancellationToken);

            [OperationContract]
            [FaultContract(typeof(InvalidOperationFault))]
            Task<int> CalculateAsync(int logid, Work w, CancellationToken cancellationToken);

            /// <summary>
            ///     This method has a oneway modifier. That means the client only makes
            ///     a request and does not listen for any response at all. Oneway methods
            ///     must be void.
            /// </summary>
            [OperationContract]
            Task ZipAsync(CancellationToken cancellationToken);
        }


        /// <summary>
        ///     Ahh, now onto the cool part, defining a service. Services just need a name
        ///     and can optionally inherit from another service using the extends keyword.
        /// </summary>
        public class Client : SharedService.Client, IAsync
        {
            public Client(TProtocol protocol) : this(protocol, protocol)
            {
            }

            public Client(TProtocol inputProtocol, TProtocol outputProtocol) : base(inputProtocol, outputProtocol)
            {
            }

            public async Task PingAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("Ping", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new PingArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new PingResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                return;
            }

            public async Task<int> @AddAsync(int num1, int num2, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("Add", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new AddArgs();
                args.Num1 = num1;
                args.Num2 = num2;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new AddResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "Add failed: unknown result");
            }

            public async Task<int> CalculateAsync(int logid, Work w, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("Calculate", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new CalculateArgs();
                args.Logid = logid;
                args.W = w;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new CalculateResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ouch)
                {
                    throw result.Ouch;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "Calculate failed: unknown result");
            }

            public async Task ZipAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("Zip", TMessageType.Oneway, SeqId),
                        cancellationToken);

                var args = new ZipArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);
            }
        }

        public class AsyncProcessor : SharedService.AsyncProcessor, ITAsyncProcessor
        {
            private IAsync _iAsync;

            public AsyncProcessor(IAsync iAsync) : base(iAsync)
            {
                if (iAsync == null) throw new ArgumentNullException(nameof(iAsync));

                _iAsync = iAsync;
                processMap_["Ping"] = Ping_ProcessAsync;
                processMap_["Add"] = Add_ProcessAsync;
                processMap_["Calculate"] = Calculate_ProcessAsync;
                processMap_["Zip"] = Zip_ProcessAsync;
            }


            public new async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot)
            {
                return await ProcessAsync(iprot, oprot, CancellationToken.None);
            }

            public new async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                try
                {
                    var msg = await iprot.ReadMessageBeginAsync(cancellationToken);

                    ProcessFunction fn;
                    processMap_.TryGetValue(msg.Name, out fn);

                    if (fn == null)
                    {
                        await TProtocolUtil.SkipAsync(iprot, TType.Struct, cancellationToken);
                        await iprot.ReadMessageEndAsync(cancellationToken);
                        var x = new TApplicationException(TApplicationException.ExceptionType.UnknownMethod,
                            "Invalid method name: '" + msg.Name + "'");
                        await
                            oprot.WriteMessageBeginAsync(new TMessage(msg.Name, TMessageType.Exception, msg.SeqID),
                                cancellationToken);
                        await x.WriteAsync(oprot, cancellationToken);
                        await oprot.WriteMessageEndAsync(cancellationToken);
                        await oprot.Transport.FlushAsync(cancellationToken);
                        return true;
                    }

                    await fn(msg.SeqID, iprot, oprot, cancellationToken);
                }
                catch (IOException)
                {
                    return false;
                }

                return true;
            }

            public async Task Ping_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new PingArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new PingResult();
                try
                {
                    await _iAsync.PingAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("Ping", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("Ping", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task Add_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new AddArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new AddResult();
                try
                {
                    result.Success = await _iAsync.@AddAsync(args.Num1, args.Num2, cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("Add", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("Add", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task Calculate_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new CalculateArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new CalculateResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.CalculateAsync(args.Logid, args.W, cancellationToken);
                    }
                    catch (InvalidOperation ouch)
                    {
                        result.Ouch = ouch;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("Calculate", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("Calculate", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task Zip_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new ZipArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                try
                {
                    await _iAsync.ZipAsync(cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                }
            }
        }


        [DataContract(Namespace = "")]
        public partial class PingArgs : TBase
        {
            public PingArgs()
            {
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
                    var struc = new TStruct("Ping_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
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
                var sb = new StringBuilder("Ping_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class PingResult : TBase
        {
            public PingResult()
            {
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
                    var struc = new TStruct("Ping_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
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
                var sb = new StringBuilder("Ping_result(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class AddArgs : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private int _num1;
            private int _num2;

            public AddArgs()
            {
            }

            [DataMember(Order = 0)]
            public int Num1
            {
                get { return _num1; }
                set
                {
                    __isset.num1 = true;
                    this._num1 = value;
                }
            }

            [DataMember(Order = 0)]
            public int Num2
            {
                get { return _num2; }
                set
                {
                    __isset.num2 = true;
                    this._num2 = value;
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
                                    Num1 = await iprot.ReadI32Async(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.I32)
                                {
                                    Num2 = await iprot.ReadI32Async(cancellationToken);
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
                    var struc = new TStruct("Add_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    if (__isset.num1)
                    {
                        field.Name = "num1";
                        field.Type = TType.I32;
                        field.ID = 1;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async(Num1, cancellationToken);
                        await oprot.WriteFieldEndAsync(cancellationToken);
                    }
                    if (__isset.num2)
                    {
                        field.Name = "num2";
                        field.Type = TType.I32;
                        field.ID = 2;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async(Num2, cancellationToken);
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
                var sb = new StringBuilder("Add_args(");
                bool __first = true;
                if (__isset.num1)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Num1: ");
                    sb.Append(Num1);
                }
                if (__isset.num2)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Num2: ");
                    sb.Append(Num2);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool num1;
                [DataMember] public bool num2;
            }

            #region XmlSerializer support

            public bool ShouldSerializeNum1()
            {
                return __isset.num1;
            }

            public bool ShouldSerializeNum2()
            {
                return __isset.num2;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class AddResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private int _success;

            public AddResult()
            {
            }

            [DataMember(Order = 0)]
            public int Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
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
                            case 0:
                                if (field.Type == TType.I32)
                                {
                                    Success = await iprot.ReadI32Async(cancellationToken);
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
                    var struc = new TStruct("Add_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        field.Name = "Success";
                        field.Type = TType.I32;
                        field.ID = 0;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async(Success, cancellationToken);
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

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("Add_result(");
                bool __first = true;
                if (__isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
            }
        }


        [DataContract(Namespace = "")]
        public partial class CalculateArgs : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private int _logid;
            private Work _w;

            public CalculateArgs()
            {
            }

            [DataMember(Order = 0)]
            public int Logid
            {
                get { return _logid; }
                set
                {
                    __isset.logid = true;
                    this._logid = value;
                }
            }

            [DataMember(Order = 0)]
            public Work W
            {
                get { return _w; }
                set
                {
                    __isset.w = true;
                    this._w = value;
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
                                    Logid = await iprot.ReadI32Async(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    W = new Work();
                                    await W.ReadAsync(iprot, cancellationToken);
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
                    var struc = new TStruct("Calculate_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    if (__isset.logid)
                    {
                        field.Name = "logid";
                        field.Type = TType.I32;
                        field.ID = 1;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async(Logid, cancellationToken);
                        await oprot.WriteFieldEndAsync(cancellationToken);
                    }
                    if (W != null && __isset.w)
                    {
                        field.Name = "w";
                        field.Type = TType.Struct;
                        field.ID = 2;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await W.WriteAsync(oprot, cancellationToken);
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
                var sb = new StringBuilder("Calculate_args(");
                bool __first = true;
                if (__isset.logid)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Logid: ");
                    sb.Append(Logid);
                }
                if (W != null && __isset.w)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("W: ");
                    sb.Append(W == null ? "<null>" : W.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool logid;
                [DataMember] public bool w;
            }

            #region XmlSerializer support

            public bool ShouldSerializeLogid()
            {
                return __isset.logid;
            }

            public bool ShouldSerializeW()
            {
                return __isset.w;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class CalculateResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidOperation _ouch;
            private int _success;

            public CalculateResult()
            {
            }

            [DataMember(Order = 0)]
            public int Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidOperation Ouch
            {
                get { return _ouch; }
                set
                {
                    __isset.ouch = true;
                    this._ouch = value;
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
                            case 0:
                                if (field.Type == TType.I32)
                                {
                                    Success = await iprot.ReadI32Async(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ouch = new InvalidOperation();
                                    await Ouch.ReadAsync(iprot, cancellationToken);
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
                    var struc = new TStruct("Calculate_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        field.Name = "Success";
                        field.Type = TType.I32;
                        field.ID = 0;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async(Success, cancellationToken);
                        await oprot.WriteFieldEndAsync(cancellationToken);
                    }
                    else if (this.__isset.ouch)
                    {
                        if (Ouch != null)
                        {
                            field.Name = "Ouch";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ouch.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
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
                var sb = new StringBuilder("Calculate_result(");
                bool __first = true;
                if (__isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ouch != null && __isset.ouch)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ouch: ");
                    sb.Append(Ouch == null ? "<null>" : Ouch.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ouch;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeOuch()
            {
                return __isset.ouch;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class ZipArgs : TBase
        {
            public ZipArgs()
            {
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
                    var struc = new TStruct("Zip_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
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
                var sb = new StringBuilder("Zip_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }
    }
}