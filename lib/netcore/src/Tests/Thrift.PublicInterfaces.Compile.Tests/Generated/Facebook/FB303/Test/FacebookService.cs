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


namespace Facebook.FB303.Test
{
    public partial class FacebookService
    {
        /// <summary>
        ///     Standard base service
        /// </summary>
        [ServiceContract(Namespace = "")]
        public interface IAsync
        {
            /// <summary>
            ///     Returns a descriptive name of the service
            /// </summary>
            [OperationContract]
            Task<string> getNameAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     Returns the version of the service
            /// </summary>
            [OperationContract]
            Task<string> getVersionAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     Gets the status of this service
            /// </summary>
            [OperationContract]
            Task<fb_status> getStatusAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     User friendly description of status, such as why the service is in
            ///     the dead or warning state, or what is being started or stopped.
            /// </summary>
            [OperationContract]
            Task<string> getStatusDetailsAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     Gets the counters for this service
            /// </summary>
            [OperationContract]
            Task<Dictionary<string, long>> getCountersAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     Gets the value of a single counter
            /// </summary>
            /// <param name="key"></param>
            [OperationContract]
            Task<long> getCounterAsync(string key, CancellationToken cancellationToken);

            /// <summary>
            ///     Sets an option
            /// </summary>
            /// <param name="key"></param>
            /// <param name="value"></param>
            [OperationContract]
            Task setOptionAsync(string key, string @value, CancellationToken cancellationToken);

            /// <summary>
            ///     Gets an option
            /// </summary>
            /// <param name="key"></param>
            [OperationContract]
            Task<string> getOptionAsync(string key, CancellationToken cancellationToken);

            /// <summary>
            ///     Gets all options
            /// </summary>
            [OperationContract]
            Task<Dictionary<string, string>> getOptionsAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     Returns a CPU profile over the given time interval (client and server
            ///     must agree on the profile format).
            /// </summary>
            /// <param name="profileDurationInSec"></param>
            [OperationContract]
            Task<string> getCpuProfileAsync(int profileDurationInSec, CancellationToken cancellationToken);

            /// <summary>
            ///     Returns the unix time that the server has been running since
            /// </summary>
            [OperationContract]
            Task<long> aliveSinceAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     Tell the server to reload its configuration, reopen log files, etc
            /// </summary>
            [OperationContract]
            Task reinitializeAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     Suggest a shutdown to the server
            /// </summary>
            [OperationContract]
            Task shutdownAsync(CancellationToken cancellationToken);
        }


        /// <summary>
        ///     Standard base service
        /// </summary>
        public class Client : TBaseClient, IDisposable, IAsync
        {
            public Client(TProtocol protocol) : this(protocol, protocol)
            {
            }

            public Client(TProtocol inputProtocol, TProtocol outputProtocol) : base(inputProtocol, outputProtocol)
            {
            }

            public async Task<string> getNameAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getName", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getNameArgs();

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

                var result = new getNameResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getName failed: unknown result");
            }

            public async Task<string> getVersionAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getVersion", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getVersionArgs();

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

                var result = new getVersionResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getVersion failed: unknown result");
            }

            public async Task<fb_status> getStatusAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getStatus", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getStatusArgs();

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

                var result = new getStatusResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getStatus failed: unknown result");
            }

            public async Task<string> getStatusDetailsAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getStatusDetails", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getStatusDetailsArgs();

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

                var result = new getStatusDetailsResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getStatusDetails failed: unknown result");
            }

            public async Task<Dictionary<string, long>> getCountersAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getCounters", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getCountersArgs();

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

                var result = new getCountersResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getCounters failed: unknown result");
            }

            public async Task<long> getCounterAsync(string key, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getCounter", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getCounterArgs();
                args.Key = key;

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

                var result = new getCounterResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getCounter failed: unknown result");
            }

            public async Task setOptionAsync(string key, string @value, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("setOption", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new setOptionArgs();
                args.Key = key;
                args.Value = @value;

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

                var result = new setOptionResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                return;
            }

            public async Task<string> getOptionAsync(string key, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getOption", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getOptionArgs();
                args.Key = key;

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

                var result = new getOptionResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getOption failed: unknown result");
            }

            public async Task<Dictionary<string, string>> getOptionsAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getOptions", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getOptionsArgs();

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

                var result = new getOptionsResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getOptions failed: unknown result");
            }

            public async Task<string> getCpuProfileAsync(int profileDurationInSec, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("getCpuProfile", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getCpuProfileArgs();
                args.ProfileDurationInSec = profileDurationInSec;

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

                var result = new getCpuProfileResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "getCpuProfile failed: unknown result");
            }

            public async Task<long> aliveSinceAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("aliveSince", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new aliveSinceArgs();

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

                var result = new aliveSinceResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "aliveSince failed: unknown result");
            }

            public async Task reinitializeAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("reinitialize", TMessageType.Oneway, SeqId),
                        cancellationToken);

                var args = new reinitializeArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);
            }

            public async Task shutdownAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("shutdown", TMessageType.Oneway, SeqId),
                        cancellationToken);

                var args = new shutdownArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);
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
                processMap_["getName"] = getName_ProcessAsync;
                processMap_["getVersion"] = getVersion_ProcessAsync;
                processMap_["getStatus"] = getStatus_ProcessAsync;
                processMap_["getStatusDetails"] = getStatusDetails_ProcessAsync;
                processMap_["getCounters"] = getCounters_ProcessAsync;
                processMap_["getCounter"] = getCounter_ProcessAsync;
                processMap_["setOption"] = setOption_ProcessAsync;
                processMap_["getOption"] = getOption_ProcessAsync;
                processMap_["getOptions"] = getOptions_ProcessAsync;
                processMap_["getCpuProfile"] = getCpuProfile_ProcessAsync;
                processMap_["aliveSince"] = aliveSince_ProcessAsync;
                processMap_["reinitialize"] = reinitialize_ProcessAsync;
                processMap_["shutdown"] = shutdown_ProcessAsync;
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

            public async Task getName_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getNameArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getNameResult();
                try
                {
                    result.Success = await _iAsync.getNameAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getName", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getName", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task getVersion_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getVersionArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getVersionResult();
                try
                {
                    result.Success = await _iAsync.getVersionAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getVersion", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getVersion", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task getStatus_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getStatusArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getStatusResult();
                try
                {
                    result.Success = await _iAsync.getStatusAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getStatus", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getStatus", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task getStatusDetails_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getStatusDetailsArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getStatusDetailsResult();
                try
                {
                    result.Success = await _iAsync.getStatusDetailsAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getStatusDetails", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getStatusDetails", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task getCounters_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getCountersArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getCountersResult();
                try
                {
                    result.Success = await _iAsync.getCountersAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getCounters", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getCounters", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task getCounter_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getCounterArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getCounterResult();
                try
                {
                    result.Success = await _iAsync.getCounterAsync(args.Key, cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getCounter", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getCounter", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task setOption_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new setOptionArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new setOptionResult();
                try
                {
                    await _iAsync.setOptionAsync(args.Key, args.Value, cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("setOption", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("setOption", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task getOption_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getOptionArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getOptionResult();
                try
                {
                    result.Success = await _iAsync.getOptionAsync(args.Key, cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getOption", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getOption", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task getOptions_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getOptionsArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getOptionsResult();
                try
                {
                    result.Success = await _iAsync.getOptionsAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getOptions", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getOptions", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task getCpuProfile_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getCpuProfileArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getCpuProfileResult();
                try
                {
                    result.Success = await _iAsync.getCpuProfileAsync(args.ProfileDurationInSec, cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("getCpuProfile", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("getCpuProfile", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task aliveSince_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new aliveSinceArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new aliveSinceResult();
                try
                {
                    result.Success = await _iAsync.aliveSinceAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("aliveSince", TMessageType.Reply, seqid),
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
                        oprot.WriteMessageBeginAsync(new TMessage("aliveSince", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task reinitialize_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new reinitializeArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                try
                {
                    await _iAsync.reinitializeAsync(cancellationToken);
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

            public async Task shutdown_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new shutdownArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                try
                {
                    await _iAsync.shutdownAsync(cancellationToken);
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

            protected delegate Task ProcessFunction(
                int seqid, TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken);
        }


        [DataContract(Namespace = "")]
        public partial class getNameArgs : TBase
        {
            public getNameArgs()
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
                    var struc = new TStruct("getName_args");
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
                var sb = new StringBuilder("getName_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class getNameResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public getNameResult()
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
                    var struc = new TStruct("getName_result");
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
                var sb = new StringBuilder("getName_result(");
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


        [DataContract(Namespace = "")]
        public partial class getVersionArgs : TBase
        {
            public getVersionArgs()
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
                    var struc = new TStruct("getVersion_args");
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
                var sb = new StringBuilder("getVersion_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class getVersionResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public getVersionResult()
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
                    var struc = new TStruct("getVersion_result");
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
                var sb = new StringBuilder("getVersion_result(");
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


        [DataContract(Namespace = "")]
        public partial class getStatusArgs : TBase
        {
            public getStatusArgs()
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
                    var struc = new TStruct("getStatus_args");
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
                var sb = new StringBuilder("getStatus_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class getStatusResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private fb_status _success;

            public getStatusResult()
            {
            }

            /// <summary>
            ///     <seealso cref="fb_status" />
            /// </summary>
            [DataMember(Order = 0)]
            public fb_status Success
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
                                    Success = (fb_status) await iprot.ReadI32Async(cancellationToken);
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
                    var struc = new TStruct("getStatus_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        field.Name = "Success";
                        field.Type = TType.I32;
                        field.ID = 0;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async((int) Success, cancellationToken);
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
                var sb = new StringBuilder("getStatus_result(");
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
        public partial class getStatusDetailsArgs : TBase
        {
            public getStatusDetailsArgs()
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
                    var struc = new TStruct("getStatusDetails_args");
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
                var sb = new StringBuilder("getStatusDetails_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class getStatusDetailsResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public getStatusDetailsResult()
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
                    var struc = new TStruct("getStatusDetails_result");
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
                var sb = new StringBuilder("getStatusDetails_result(");
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


        [DataContract(Namespace = "")]
        public partial class getCountersArgs : TBase
        {
            public getCountersArgs()
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
                    var struc = new TStruct("getCounters_args");
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
                var sb = new StringBuilder("getCounters_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class getCountersResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private Dictionary<string, long> _success;

            public getCountersResult()
            {
            }

            [DataMember(Order = 0)]
            public Dictionary<string, long> Success
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
                                if (field.Type == TType.Map)
                                {
                                    {
                                        Success = new Dictionary<string, long>();
                                        TMap _map0 = await iprot.ReadMapBeginAsync(cancellationToken);
                                        for (int _i1 = 0; _i1 < _map0.Count; ++_i1)
                                        {
                                            string _key2;
                                            long _val3;
                                            _key2 = await iprot.ReadStringAsync(cancellationToken);
                                            _val3 = await iprot.ReadI64Async(cancellationToken);
                                            Success[_key2] = _val3;
                                        }
                                        await iprot.ReadMapEndAsync(cancellationToken);
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
                    var struc = new TStruct("getCounters_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Map;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteMapBeginAsync(new TMap(TType.String, TType.I64, Success.Count),
                                        cancellationToken);
                                foreach (string _iter4 in Success.Keys)
                                {
                                    await oprot.WriteStringAsync(_iter4, cancellationToken);
                                    await oprot.WriteI64Async(Success[_iter4], cancellationToken);
                                }
                                await oprot.WriteMapEndAsync(cancellationToken);
                            }
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
                var sb = new StringBuilder("getCounters_result(");
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


        [DataContract(Namespace = "")]
        public partial class getCounterArgs : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _key;

            public getCounterArgs()
            {
            }

            [DataMember(Order = 0)]
            public string Key
            {
                get { return _key; }
                set
                {
                    __isset.key = true;
                    this._key = value;
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
                                    Key = await iprot.ReadStringAsync(cancellationToken);
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
                    var struc = new TStruct("getCounter_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    if (Key != null && __isset.key)
                    {
                        field.Name = "key";
                        field.Type = TType.String;
                        field.ID = 1;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteStringAsync(Key, cancellationToken);
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

            public bool ShouldSerializeKey()
            {
                return __isset.key;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("getCounter_args(");
                bool __first = true;
                if (Key != null && __isset.key)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Key: ");
                    sb.Append(Key);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool key;
            }
        }


        [DataContract(Namespace = "")]
        public partial class getCounterResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private long _success;

            public getCounterResult()
            {
            }

            [DataMember(Order = 0)]
            public long Success
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
                                if (field.Type == TType.I64)
                                {
                                    Success = await iprot.ReadI64Async(cancellationToken);
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
                    var struc = new TStruct("getCounter_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        field.Name = "Success";
                        field.Type = TType.I64;
                        field.ID = 0;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI64Async(Success, cancellationToken);
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
                var sb = new StringBuilder("getCounter_result(");
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
        public partial class setOptionArgs : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _key;
            private string _value;

            public setOptionArgs()
            {
            }

            [DataMember(Order = 0)]
            public string Key
            {
                get { return _key; }
                set
                {
                    __isset.key = true;
                    this._key = value;
                }
            }

            [DataMember(Order = 0)]
            public string Value
            {
                get { return _value; }
                set
                {
                    __isset.@value = true;
                    this._value = value;
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
                                    Key = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.String)
                                {
                                    Value = await iprot.ReadStringAsync(cancellationToken);
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
                    var struc = new TStruct("setOption_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    if (Key != null && __isset.key)
                    {
                        field.Name = "key";
                        field.Type = TType.String;
                        field.ID = 1;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteStringAsync(Key, cancellationToken);
                        await oprot.WriteFieldEndAsync(cancellationToken);
                    }
                    if (Value != null && __isset.@value)
                    {
                        field.Name = "value";
                        field.Type = TType.String;
                        field.ID = 2;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteStringAsync(Value, cancellationToken);
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
                var sb = new StringBuilder("setOption_args(");
                bool __first = true;
                if (Key != null && __isset.key)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Key: ");
                    sb.Append(Key);
                }
                if (Value != null && __isset.@value)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Value: ");
                    sb.Append(Value);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool key;
                [DataMember] public bool @value;
            }

            #region XmlSerializer support

            public bool ShouldSerializeKey()
            {
                return __isset.key;
            }

            public bool ShouldSerializeValue()
            {
                return __isset.@value;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class setOptionResult : TBase
        {
            public setOptionResult()
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
                    var struc = new TStruct("setOption_result");
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
                var sb = new StringBuilder("setOption_result(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class getOptionArgs : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _key;

            public getOptionArgs()
            {
            }

            [DataMember(Order = 0)]
            public string Key
            {
                get { return _key; }
                set
                {
                    __isset.key = true;
                    this._key = value;
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
                                    Key = await iprot.ReadStringAsync(cancellationToken);
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
                    var struc = new TStruct("getOption_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    if (Key != null && __isset.key)
                    {
                        field.Name = "key";
                        field.Type = TType.String;
                        field.ID = 1;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteStringAsync(Key, cancellationToken);
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

            public bool ShouldSerializeKey()
            {
                return __isset.key;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("getOption_args(");
                bool __first = true;
                if (Key != null && __isset.key)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Key: ");
                    sb.Append(Key);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool key;
            }
        }


        [DataContract(Namespace = "")]
        public partial class getOptionResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public getOptionResult()
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
                    var struc = new TStruct("getOption_result");
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
                var sb = new StringBuilder("getOption_result(");
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


        [DataContract(Namespace = "")]
        public partial class getOptionsArgs : TBase
        {
            public getOptionsArgs()
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
                    var struc = new TStruct("getOptions_args");
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
                var sb = new StringBuilder("getOptions_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class getOptionsResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private Dictionary<string, string> _success;

            public getOptionsResult()
            {
            }

            [DataMember(Order = 0)]
            public Dictionary<string, string> Success
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
                                if (field.Type == TType.Map)
                                {
                                    {
                                        Success = new Dictionary<string, string>();
                                        TMap _map5 = await iprot.ReadMapBeginAsync(cancellationToken);
                                        for (int _i6 = 0; _i6 < _map5.Count; ++_i6)
                                        {
                                            string _key7;
                                            string _val8;
                                            _key7 = await iprot.ReadStringAsync(cancellationToken);
                                            _val8 = await iprot.ReadStringAsync(cancellationToken);
                                            Success[_key7] = _val8;
                                        }
                                        await iprot.ReadMapEndAsync(cancellationToken);
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
                    var struc = new TStruct("getOptions_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Map;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteMapBeginAsync(new TMap(TType.String, TType.String, Success.Count),
                                        cancellationToken);
                                foreach (string _iter9 in Success.Keys)
                                {
                                    await oprot.WriteStringAsync(_iter9, cancellationToken);
                                    await oprot.WriteStringAsync(Success[_iter9], cancellationToken);
                                }
                                await oprot.WriteMapEndAsync(cancellationToken);
                            }
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
                var sb = new StringBuilder("getOptions_result(");
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


        [DataContract(Namespace = "")]
        public partial class getCpuProfileArgs : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private int _profileDurationInSec;

            public getCpuProfileArgs()
            {
            }

            [DataMember(Order = 0)]
            public int ProfileDurationInSec
            {
                get { return _profileDurationInSec; }
                set
                {
                    __isset.profileDurationInSec = true;
                    this._profileDurationInSec = value;
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
                                    ProfileDurationInSec = await iprot.ReadI32Async(cancellationToken);
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
                    var struc = new TStruct("getCpuProfile_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    if (__isset.profileDurationInSec)
                    {
                        field.Name = "profileDurationInSec";
                        field.Type = TType.I32;
                        field.ID = 1;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async(ProfileDurationInSec, cancellationToken);
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

            public bool ShouldSerializeProfileDurationInSec()
            {
                return __isset.profileDurationInSec;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("getCpuProfile_args(");
                bool __first = true;
                if (__isset.profileDurationInSec)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("ProfileDurationInSec: ");
                    sb.Append(ProfileDurationInSec);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool profileDurationInSec;
            }
        }


        [DataContract(Namespace = "")]
        public partial class getCpuProfileResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public getCpuProfileResult()
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
                    var struc = new TStruct("getCpuProfile_result");
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
                var sb = new StringBuilder("getCpuProfile_result(");
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


        [DataContract(Namespace = "")]
        public partial class aliveSinceArgs : TBase
        {
            public aliveSinceArgs()
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
                    var struc = new TStruct("aliveSince_args");
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
                var sb = new StringBuilder("aliveSince_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class aliveSinceResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private long _success;

            public aliveSinceResult()
            {
            }

            [DataMember(Order = 0)]
            public long Success
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
                                if (field.Type == TType.I64)
                                {
                                    Success = await iprot.ReadI64Async(cancellationToken);
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
                    var struc = new TStruct("aliveSince_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        field.Name = "Success";
                        field.Type = TType.I64;
                        field.ID = 0;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI64Async(Success, cancellationToken);
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
                var sb = new StringBuilder("aliveSince_result(");
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
        public partial class reinitializeArgs : TBase
        {
            public reinitializeArgs()
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
                    var struc = new TStruct("reinitialize_args");
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
                var sb = new StringBuilder("reinitialize_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class shutdownArgs : TBase
        {
            public shutdownArgs()
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
                    var struc = new TStruct("shutdown_args");
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
                var sb = new StringBuilder("shutdown_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }
    }
}