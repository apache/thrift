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
    public partial class SecondService
    {
        [ServiceContract(Namespace = "")]
        public interface IAsync
        {
            [OperationContract]
            Task blahBlahAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     Prints 'testString("%s")' with thing as '%s'
            ///     @param string thing - the string to print
            ///     @return string - returns the string 'thing'
            /// </summary>
            /// <param name="thing"></param>
            [OperationContract]
            Task<string> secondtestStringAsync(string thing, CancellationToken cancellationToken);
        }


        public class Client : TBaseClient, IDisposable, IAsync
        {
            public Client(TProtocol protocol) : this(protocol, protocol)
            {
            }

            public Client(TProtocol inputProtocol, TProtocol outputProtocol) : base(inputProtocol, outputProtocol)
            {
            }

            public async Task blahBlahAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("blahBlah", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new blahBlahArgs();

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

                var result = new blahBlahResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                return;
            }

            public async Task<string> secondtestStringAsync(string thing, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("secondtestString", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new secondtestStringArgs();
                args.Thing = thing;

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

                var result = new secondtestStringResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "secondtestString failed: unknown result");
            }
        }

        public class AsyncProcessor : ITAsyncProcessor
        {
            private IAsync _iAsync;
            protected Dictionary<string, ProcessFunction> processMap_ = new Dictionary<string, ProcessFunction>();

            public AsyncProcessor(IAsync iAsync)
            {
                if (iAsync == null) throw new ArgumentNullException(nameof(iAsync));

                _iAsync = iAsync;
                processMap_["blahBlah"] = blahBlah_ProcessAsync;
                processMap_["secondtestString"] = secondtestString_ProcessAsync;
            }

            public async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot)
            {
                return await ProcessAsync(iprot, oprot, CancellationToken.None);
            }

            public async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken)
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

            public async Task blahBlah_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new blahBlahArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new blahBlahResult();
                try
                {
                    await _iAsync.blahBlahAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("blahBlah", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("blahBlah", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task secondtestString_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new secondtestStringArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new secondtestStringResult();
                try
                {
                    result.Success = await _iAsync.secondtestStringAsync(args.Thing, cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("secondtestString", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("secondtestString", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            protected delegate Task ProcessFunction(
                int seqid, TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken);
        }


        [DataContract(Namespace = "")]
        public partial class blahBlahArgs : TBase
        {
            public blahBlahArgs()
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
                    var struc = new TStruct("blahBlah_args");
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
                var sb = new StringBuilder("blahBlah_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class blahBlahResult : TBase
        {
            public blahBlahResult()
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
                    var struc = new TStruct("blahBlah_result");
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
                var sb = new StringBuilder("blahBlah_result(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class secondtestStringArgs : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _thing;

            public secondtestStringArgs()
            {
            }

            [DataMember(Order = 0)]
            public string Thing
            {
                get { return _thing; }
                set
                {
                    __isset.thing = true;
                    this._thing = value;
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
                                    Thing = await iprot.ReadStringAsync(cancellationToken);
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
                    var struc = new TStruct("secondtestString_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    if (Thing != null && __isset.thing)
                    {
                        field.Name = "thing";
                        field.Type = TType.String;
                        field.ID = 1;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteStringAsync(Thing, cancellationToken);
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

            public bool ShouldSerializeThing()
            {
                return __isset.thing;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("secondtestString_args(");
                bool __first = true;
                if (Thing != null && __isset.thing)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Thing: ");
                    sb.Append(Thing);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool thing;
            }
        }


        [DataContract(Namespace = "")]
        public partial class secondtestStringResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public secondtestStringResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
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
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
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
                    var struc = new TStruct("secondtestString_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
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

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("secondtestString_result(");
                bool __first = true;
                if (Success != null && __isset.success)
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
    }
}