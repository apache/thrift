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
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Thrift.Protocol.Entities;
using Thrift.Transport;

namespace Thrift.Protocol
{
    // ReSharper disable once InconsistentNaming
    public abstract class TProtocol : IDisposable
    {
        public const int DefaultRecursionDepth = 64;
        private bool _isDisposed;
        protected int RecursionDepth;

        protected TTransport Trans;

        protected TProtocol(TTransport trans)
        {
            Trans = trans;
            RecursionLimit = DefaultRecursionDepth;
            RecursionDepth = 0;
        }

        public TTransport Transport => Trans;

        protected int RecursionLimit { get; set; }

        public void Dispose()
        {
            Dispose(true);
        }

        public void IncrementRecursionDepth()
        {
            if (RecursionDepth < RecursionLimit)
            {
                ++RecursionDepth;
            }
            else
            {
                throw new TProtocolException(TProtocolException.DEPTH_LIMIT, "Depth limit exceeded");
            }
        }

        public void DecrementRecursionDepth()
        {
            --RecursionDepth;
        }

        protected virtual void Dispose(bool disposing)
        {
            if (!_isDisposed)
            {
                if (disposing)
                {
                    (Trans as IDisposable)?.Dispose();
                }
            }
            _isDisposed = true;
        }

        public virtual async Task WriteMessageBeginAsync(TMessage message)
        {
            await WriteMessageBeginAsync(message, CancellationToken.None);
        }

        public abstract Task WriteMessageBeginAsync(TMessage message, CancellationToken cancellationToken);

        public virtual async Task WriteMessageEndAsync()
        {
            await WriteMessageEndAsync(CancellationToken.None);
        }

        public abstract Task WriteMessageEndAsync(CancellationToken cancellationToken);

        public virtual async Task WriteStructBeginAsync(TStruct @struct)
        {
            await WriteStructBeginAsync(@struct, CancellationToken.None);
        }

        public abstract Task WriteStructBeginAsync(TStruct @struct, CancellationToken cancellationToken);

        public virtual async Task WriteStructEndAsync()
        {
            await WriteStructEndAsync(CancellationToken.None);
        }

        public abstract Task WriteStructEndAsync(CancellationToken cancellationToken);

        public virtual async Task WriteFieldBeginAsync(TField field)
        {
            await WriteFieldBeginAsync(field, CancellationToken.None);
        }

        public abstract Task WriteFieldBeginAsync(TField field, CancellationToken cancellationToken);

        public virtual async Task WriteFieldEndAsync()
        {
            await WriteFieldEndAsync(CancellationToken.None);
        }

        public abstract Task WriteFieldEndAsync(CancellationToken cancellationToken);

        public virtual async Task WriteFieldStopAsync()
        {
            await WriteFieldStopAsync(CancellationToken.None);
        }

        public abstract Task WriteFieldStopAsync(CancellationToken cancellationToken);

        public virtual async Task WriteMapBeginAsync(TMap map)
        {
            await WriteMapBeginAsync(map, CancellationToken.None);
        }

        public abstract Task WriteMapBeginAsync(TMap map, CancellationToken cancellationToken);

        public virtual async Task WriteMapEndAsync()
        {
            await WriteMapEndAsync(CancellationToken.None);
        }

        public abstract Task WriteMapEndAsync(CancellationToken cancellationToken);

        public virtual async Task WriteListBeginAsync(TList list)
        {
            await WriteListBeginAsync(list, CancellationToken.None);
        }

        public abstract Task WriteListBeginAsync(TList list, CancellationToken cancellationToken);

        public virtual async Task WriteListEndAsync()
        {
            await WriteListEndAsync(CancellationToken.None);
        }

        public abstract Task WriteListEndAsync(CancellationToken cancellationToken);

        public virtual async Task WriteSetBeginAsync(TSet set)
        {
            await WriteSetBeginAsync(set, CancellationToken.None);
        }

        public abstract Task WriteSetBeginAsync(TSet set, CancellationToken cancellationToken);

        public virtual async Task WriteSetEndAsync()
        {
            await WriteSetEndAsync(CancellationToken.None);
        }

        public abstract Task WriteSetEndAsync(CancellationToken cancellationToken);

        public virtual async Task WriteBoolAsync(bool b)
        {
            await WriteBoolAsync(b, CancellationToken.None);
        }

        public abstract Task WriteBoolAsync(bool b, CancellationToken cancellationToken);

        public virtual async Task WriteByteAsync(sbyte b)
        {
            await WriteByteAsync(b, CancellationToken.None);
        }

        public abstract Task WriteByteAsync(sbyte b, CancellationToken cancellationToken);

        public virtual async Task WriteI16Async(short i16)
        {
            await WriteI16Async(i16, CancellationToken.None);
        }

        public abstract Task WriteI16Async(short i16, CancellationToken cancellationToken);

        public virtual async Task WriteI32Async(int i32)
        {
            await WriteI32Async(i32, CancellationToken.None);
        }

        public abstract Task WriteI32Async(int i32, CancellationToken cancellationToken);

        public virtual async Task WriteI64Async(long i64)
        {
            await WriteI64Async(i64, CancellationToken.None);
        }

        public abstract Task WriteI64Async(long i64, CancellationToken cancellationToken);

        public virtual async Task WriteDoubleAsync(double d)
        {
            await WriteDoubleAsync(d, CancellationToken.None);
        }

        public abstract Task WriteDoubleAsync(double d, CancellationToken cancellationToken);

        public virtual async Task WriteStringAsync(string s)
        {
            await WriteStringAsync(s, CancellationToken.None);
        }

        public virtual async Task WriteStringAsync(string s, CancellationToken cancellationToken)
        {
            var bytes = Encoding.UTF8.GetBytes(s);
            await WriteBinaryAsync(bytes, cancellationToken);
        }

        public virtual async Task WriteBinaryAsync(byte[] bytes)
        {
            await WriteBinaryAsync(bytes, CancellationToken.None);
        }

        public abstract Task WriteBinaryAsync(byte[] bytes, CancellationToken cancellationToken);

        public virtual async ValueTask<TMessage> ReadMessageBeginAsync()
        {
            return await ReadMessageBeginAsync(CancellationToken.None);
        }

        public abstract ValueTask<TMessage> ReadMessageBeginAsync(CancellationToken cancellationToken);

        public virtual async Task ReadMessageEndAsync()
        {
            await ReadMessageEndAsync(CancellationToken.None);
        }

        public abstract Task ReadMessageEndAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<TStruct> ReadStructBeginAsync()
        {
            return await ReadStructBeginAsync(CancellationToken.None);
        }

        public abstract ValueTask<TStruct> ReadStructBeginAsync(CancellationToken cancellationToken);

        public virtual async Task ReadStructEndAsync()
        {
            await ReadStructEndAsync(CancellationToken.None);
        }

        public abstract Task ReadStructEndAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<TField> ReadFieldBeginAsync()
        {
            return await ReadFieldBeginAsync(CancellationToken.None);
        }

        public abstract ValueTask<TField> ReadFieldBeginAsync(CancellationToken cancellationToken);

        public virtual async Task ReadFieldEndAsync()
        {
            await ReadFieldEndAsync(CancellationToken.None);
        }

        public abstract Task ReadFieldEndAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<TMap> ReadMapBeginAsync()
        {
            return await ReadMapBeginAsync(CancellationToken.None);
        }

        public abstract ValueTask<TMap> ReadMapBeginAsync(CancellationToken cancellationToken);

        public virtual async Task ReadMapEndAsync()
        {
            await ReadMapEndAsync(CancellationToken.None);
        }

        public abstract Task ReadMapEndAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<TList> ReadListBeginAsync()
        {
            return await ReadListBeginAsync(CancellationToken.None);
        }

        public abstract ValueTask<TList> ReadListBeginAsync(CancellationToken cancellationToken);

        public virtual async Task ReadListEndAsync()
        {
            await ReadListEndAsync(CancellationToken.None);
        }

        public abstract Task ReadListEndAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<TSet> ReadSetBeginAsync()
        {
            return await ReadSetBeginAsync(CancellationToken.None);
        }

        public abstract ValueTask<TSet> ReadSetBeginAsync(CancellationToken cancellationToken);

        public virtual async Task ReadSetEndAsync()
        {
            await ReadSetEndAsync(CancellationToken.None);
        }

        public abstract Task ReadSetEndAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<bool> ReadBoolAsync()
        {
            return await ReadBoolAsync(CancellationToken.None);
        }

        public abstract ValueTask<bool> ReadBoolAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<sbyte> ReadByteAsync()
        {
            return await ReadByteAsync(CancellationToken.None);
        }

        public abstract ValueTask<sbyte> ReadByteAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<short> ReadI16Async()
        {
            return await ReadI16Async(CancellationToken.None);
        }

        public abstract ValueTask<short> ReadI16Async(CancellationToken cancellationToken);

        public virtual async ValueTask<int> ReadI32Async()
        {
            return await ReadI32Async(CancellationToken.None);
        }

        public abstract ValueTask<int> ReadI32Async(CancellationToken cancellationToken);

        public virtual async ValueTask<long> ReadI64Async()
        {
            return await ReadI64Async(CancellationToken.None);
        }

        public abstract ValueTask<long> ReadI64Async(CancellationToken cancellationToken);

        public virtual async ValueTask<double> ReadDoubleAsync()
        {
            return await ReadDoubleAsync(CancellationToken.None);
        }

        public abstract ValueTask<double> ReadDoubleAsync(CancellationToken cancellationToken);

        public virtual async ValueTask<string> ReadStringAsync()
        {
            return await ReadStringAsync(CancellationToken.None);
        }

        public virtual async ValueTask<string> ReadStringAsync(CancellationToken cancellationToken)
        {
            var buf = await ReadBinaryAsync(cancellationToken);
            return Encoding.UTF8.GetString(buf, 0, buf.Length);
        }

        public virtual async ValueTask<byte[]> ReadBinaryAsync()
        {
            return await ReadBinaryAsync(CancellationToken.None);
        }

        public abstract ValueTask<byte[]> ReadBinaryAsync(CancellationToken cancellationToken);
    }
}
