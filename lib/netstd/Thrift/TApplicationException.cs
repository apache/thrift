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

using System.Threading;
using System.Threading.Tasks;
using Thrift.Protocol;
using Thrift.Protocol.Entities;
using Thrift.Protocol.Utilities;

namespace Thrift
{
    // ReSharper disable once InconsistentNaming
    public class TApplicationException : TException
    {
        public enum ExceptionType
        {
            Unknown,
            UnknownMethod,
            InvalidMessageType,
            WrongMethodName,
            BadSequenceId,
            MissingResult,
            InternalError,
            ProtocolError,
            InvalidTransform,
            InvalidProtocol,
            UnsupportedClientType
        }

        private const int MessageTypeFieldId = 1;
        private const int ExTypeFieldId = 2;

        public ExceptionType Type { get; private set; }

        public TApplicationException()
        {
        }

        public TApplicationException(ExceptionType type)
        {
            Type = type;
        }

        public TApplicationException(ExceptionType type, string message)
            : base(message, null) // TApplicationException is serializable, but we never serialize InnerException
        {
            Type = type;
        }

        public static async ValueTask<TApplicationException> ReadAsync(TProtocol inputProtocol, CancellationToken cancellationToken)
        {
            string message = null;
            var type = ExceptionType.Unknown;

            await inputProtocol.ReadStructBeginAsync(cancellationToken).ConfigureAwait(false);
            while (true)
            {
                var field = await inputProtocol.ReadFieldBeginAsync(cancellationToken).ConfigureAwait(false);
                if (field.Type == TType.Stop)
                {
                    break;
                }

                switch (field.ID)
                {
                    case MessageTypeFieldId:
                        if (field.Type == TType.String)
                        {
                            message = await inputProtocol.ReadStringAsync(cancellationToken).ConfigureAwait(false);
                        }
                        else
                        {
                            await TProtocolUtil.SkipAsync(inputProtocol, field.Type, cancellationToken).ConfigureAwait(false);
                        }
                        break;
                    case ExTypeFieldId:
                        if (field.Type == TType.I32)
                        {
                            type = (ExceptionType) await inputProtocol.ReadI32Async(cancellationToken).ConfigureAwait(false);
                        }
                        else
                        {
                            await TProtocolUtil.SkipAsync(inputProtocol, field.Type, cancellationToken).ConfigureAwait(false);
                        }
                        break;
                    default:
                        await TProtocolUtil.SkipAsync(inputProtocol, field.Type, cancellationToken).ConfigureAwait(false);
                        break;
                }

                await inputProtocol.ReadFieldEndAsync(cancellationToken).ConfigureAwait(false);
            }

            await inputProtocol.ReadStructEndAsync(cancellationToken).ConfigureAwait(false);

            return new TApplicationException(type, message);
        }

        public async Task WriteAsync(TProtocol outputProtocol, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            const string messageTypeFieldName = "message";
            const string exTypeFieldName = "exType";
            const string structApplicationExceptionName = "TApplicationException";

            var struc = new TStruct(structApplicationExceptionName);
            var field = new TField();

            await outputProtocol.WriteStructBeginAsync(struc, cancellationToken).ConfigureAwait(false);

            if (!string.IsNullOrEmpty(Message))
            {
                field.Name = messageTypeFieldName;
                field.Type = TType.String;
                field.ID = MessageTypeFieldId;
                await outputProtocol.WriteFieldBeginAsync(field, cancellationToken).ConfigureAwait(false);
                await outputProtocol.WriteStringAsync(Message, cancellationToken).ConfigureAwait(false);
                await outputProtocol.WriteFieldEndAsync(cancellationToken).ConfigureAwait(false);
            }

            field.Name = exTypeFieldName;
            field.Type = TType.I32;
            field.ID = ExTypeFieldId;

            await outputProtocol.WriteFieldBeginAsync(field, cancellationToken).ConfigureAwait(false);
            await outputProtocol.WriteI32Async((int) Type, cancellationToken).ConfigureAwait(false);
            await outputProtocol.WriteFieldEndAsync(cancellationToken).ConfigureAwait(false);
            await outputProtocol.WriteFieldStopAsync(cancellationToken).ConfigureAwait(false);
            await outputProtocol.WriteStructEndAsync(cancellationToken).ConfigureAwait(false);
        }
    }
}
