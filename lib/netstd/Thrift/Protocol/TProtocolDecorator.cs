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
using System.Threading;
using System.Threading.Tasks;
using Thrift.Protocol.Entities;

namespace Thrift.Protocol
{
    // ReSharper disable once InconsistentNaming
    /// <summary>
    ///     TProtocolDecorator forwards all requests to an enclosed TProtocol instance,
    ///     providing a way to author concise concrete decorator subclasses.While it has
    ///     no abstract methods, it is marked abstract as a reminder that by itself,
    ///     it does not modify the behaviour of the enclosed TProtocol.
    /// </summary>
    public abstract class TProtocolDecorator : TProtocol
    {
        private readonly TProtocol _wrappedProtocol;

        protected TProtocolDecorator(TProtocol protocol)
            : base(protocol.Transport)
        {
            _wrappedProtocol = protocol ?? throw new ArgumentNullException(nameof(protocol));
        }

        public override async Task WriteMessageBeginAsync(TMessage message, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteMessageBeginAsync(message, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteMessageEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteMessageEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteStructBeginAsync(TStruct @struct, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteStructBeginAsync(@struct, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteStructEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteStructEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteFieldBeginAsync(TField field, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteFieldBeginAsync(field, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteFieldEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteFieldEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteFieldStopAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteFieldStopAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteMapBeginAsync(TMap map, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteMapBeginAsync(map, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteMapEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteMapEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteListBeginAsync(TList list, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteListBeginAsync(list, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteListEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteListEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteSetBeginAsync(TSet set, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteSetBeginAsync(set, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteSetEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteSetEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteBoolAsync(bool b, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteBoolAsync(b, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteByteAsync(sbyte b, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteByteAsync(b, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteI16Async(short i16, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteI16Async(i16, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteI32Async(int i32, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteI32Async(i32, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteI64Async(long i64, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteI64Async(i64, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteDoubleAsync(double d, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteDoubleAsync(d, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteStringAsync(string s, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteStringAsync(s, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteBinaryAsync(byte[] bytes, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteBinaryAsync(bytes, cancellationToken).ConfigureAwait(false);
        }

        public override async Task WriteUuidAsync(Guid uuid, CancellationToken cancellationToken)
        {
            await _wrappedProtocol.WriteUuidAsync(uuid, cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<TMessage> ReadMessageBeginAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadMessageBeginAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task ReadMessageEndAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            await _wrappedProtocol.ReadMessageEndAsync(cancellationToken).ConfigureAwait(false);
            Transport.ResetMessageSizeAndConsumedBytes();
        }

        public override async ValueTask<TStruct> ReadStructBeginAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadStructBeginAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task ReadStructEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.ReadStructEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<TField> ReadFieldBeginAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadFieldBeginAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task ReadFieldEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.ReadFieldEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<TMap> ReadMapBeginAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadMapBeginAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task ReadMapEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.ReadMapEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<TList> ReadListBeginAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadListBeginAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task ReadListEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.ReadListEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<TSet> ReadSetBeginAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadSetBeginAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async Task ReadSetEndAsync(CancellationToken cancellationToken)
        {
            await _wrappedProtocol.ReadSetEndAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<bool> ReadBoolAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadBoolAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<sbyte> ReadByteAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadByteAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<short> ReadI16Async(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadI16Async(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<int> ReadI32Async(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadI32Async(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<long> ReadI64Async(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadI64Async(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<double> ReadDoubleAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadDoubleAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<string> ReadStringAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadStringAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<byte[]> ReadBinaryAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadBinaryAsync(cancellationToken).ConfigureAwait(false);
        }

        public override async ValueTask<Guid> ReadUuidAsync(CancellationToken cancellationToken)
        {
            return await _wrappedProtocol.ReadUuidAsync(cancellationToken).ConfigureAwait(false);
        }

        // Returns the minimum amount of bytes needed to store the smallest possible instance of TType.
        public override int GetMinSerializedSize(TType type)
        {
            return _wrappedProtocol.GetMinSerializedSize(type);
        }


    }
}
