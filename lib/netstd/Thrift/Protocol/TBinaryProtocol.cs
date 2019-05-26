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
    public class TBinaryProtocol : TProtocol
    {
        protected const uint VersionMask = 0xffff0000;
        protected const uint Version1 = 0x80010000;

        protected bool StrictRead;
        protected bool StrictWrite;

        // minimize memory allocations by means of an preallocated bytes buffer
        // The value of 128 is arbitrarily chosen, the required minimum size must be sizeof(long)
        private byte[] PreAllocatedBuffer = new byte[128];

        private static readonly TStruct AnonymousStruct = new TStruct(string.Empty);
        private static readonly TField StopField = new TField() { Type = TType.Stop };

        public TBinaryProtocol(TTransport trans)
            : this(trans, false, true)
        {
        }

        public TBinaryProtocol(TTransport trans, bool strictRead, bool strictWrite)
            : base(trans)
        {
            StrictRead = strictRead;
            StrictWrite = strictWrite;
        }

        public override async Task WriteMessageBeginAsync(TMessage message, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            if (StrictWrite)
            {
                var version = Version1 | (uint) message.Type;
                await WriteI32Async((int) version, cancellationToken);
                await WriteStringAsync(message.Name, cancellationToken);
                await WriteI32Async(message.SeqID, cancellationToken);
            }
            else
            {
                await WriteStringAsync(message.Name, cancellationToken);
                await WriteByteAsync((sbyte) message.Type, cancellationToken);
                await WriteI32Async(message.SeqID, cancellationToken);
            }
        }

        public override async Task WriteMessageEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async Task WriteStructBeginAsync(TStruct @struct, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async Task WriteStructEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async Task WriteFieldBeginAsync(TField field, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            await WriteByteAsync((sbyte) field.Type, cancellationToken);
            await WriteI16Async(field.ID, cancellationToken);
        }

        public override async Task WriteFieldEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async Task WriteFieldStopAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            await WriteByteAsync((sbyte) TType.Stop, cancellationToken);
        }

        public override async Task WriteMapBeginAsync(TMap map, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            PreAllocatedBuffer[0] = (byte)map.KeyType;
            PreAllocatedBuffer[1] = (byte)map.ValueType;
            await Trans.WriteAsync(PreAllocatedBuffer, 0, 2, cancellationToken);

            await WriteI32Async(map.Count, cancellationToken);
        }

        public override async Task WriteMapEndAsync(CancellationToken cancellationToken)

        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async Task WriteListBeginAsync(TList list, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            await WriteByteAsync((sbyte) list.ElementType, cancellationToken);
            await WriteI32Async(list.Count, cancellationToken);
        }

        public override async Task WriteListEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async Task WriteSetBeginAsync(TSet set, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            await WriteByteAsync((sbyte) set.ElementType, cancellationToken);
            await WriteI32Async(set.Count, cancellationToken);
        }

        public override async Task WriteSetEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async Task WriteBoolAsync(bool b, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            await WriteByteAsync(b ? (sbyte) 1 : (sbyte) 0, cancellationToken);
        }

        public override async Task WriteByteAsync(sbyte b, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            PreAllocatedBuffer[0] = (byte)b;

            await Trans.WriteAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
        }
        public override async Task WriteI16Async(short i16, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            PreAllocatedBuffer[0] = (byte)(0xff & (i16 >> 8));
            PreAllocatedBuffer[1] = (byte)(0xff & i16);

            await Trans.WriteAsync(PreAllocatedBuffer, 0, 2, cancellationToken);
        }

        public override async Task WriteI32Async(int i32, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            PreAllocatedBuffer[0] = (byte)(0xff & (i32 >> 24));
            PreAllocatedBuffer[1] = (byte)(0xff & (i32 >> 16));
            PreAllocatedBuffer[2] = (byte)(0xff & (i32 >> 8));
            PreAllocatedBuffer[3] = (byte)(0xff & i32);

            await Trans.WriteAsync(PreAllocatedBuffer, 0, 4, cancellationToken);
        }

      
        public override async Task WriteI64Async(long i64, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            PreAllocatedBuffer[0] = (byte)(0xff & (i64 >> 56));
            PreAllocatedBuffer[1] = (byte)(0xff & (i64 >> 48));
            PreAllocatedBuffer[2] = (byte)(0xff & (i64 >> 40));
            PreAllocatedBuffer[3] = (byte)(0xff & (i64 >> 32));
            PreAllocatedBuffer[4] = (byte)(0xff & (i64 >> 24));
            PreAllocatedBuffer[5] = (byte)(0xff & (i64 >> 16));
            PreAllocatedBuffer[6] = (byte)(0xff & (i64 >> 8));
            PreAllocatedBuffer[7] = (byte)(0xff & i64);

            await Trans.WriteAsync(PreAllocatedBuffer, 0, 8, cancellationToken);
        }

        public override async Task WriteDoubleAsync(double d, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            await WriteI64Async(BitConverter.DoubleToInt64Bits(d), cancellationToken);
        }


        public override async Task WriteBinaryAsync(byte[] bytes, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            await WriteI32Async(bytes.Length, cancellationToken);
            await Trans.WriteAsync(bytes, 0, bytes.Length, cancellationToken);
        }

        public override async ValueTask<TMessage> ReadMessageBeginAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<TMessage>(cancellationToken);
            }

            var message = new TMessage();
            var size = await ReadI32Async(cancellationToken);
            if (size < 0)
            {
                var version = (uint) size & VersionMask;
                if (version != Version1)
                {
                    throw new TProtocolException(TProtocolException.BAD_VERSION,
                        $"Bad version in ReadMessageBegin: {version}");
                }
                message.Type = (TMessageType) (size & 0x000000ff);
                message.Name = await ReadStringAsync(cancellationToken);
                message.SeqID = await ReadI32Async(cancellationToken);
            }
            else
            {
                if (StrictRead)
                {
                    throw new TProtocolException(TProtocolException.BAD_VERSION,
                        "Missing version in ReadMessageBegin, old client?");
                }
                message.Name = (size > 0) ? await ReadStringBodyAsync(size, cancellationToken) : string.Empty;
                message.Type = (TMessageType) await ReadByteAsync(cancellationToken);
                message.SeqID = await ReadI32Async(cancellationToken);
            }
            return message;
        }

        public override async Task ReadMessageEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async ValueTask<TStruct> ReadStructBeginAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }

            return AnonymousStruct;
        }

        public override async Task ReadStructEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async ValueTask<TField> ReadFieldBeginAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<TField>(cancellationToken);
            }


            var type = (TType)await ReadByteAsync(cancellationToken);
            if (type == TType.Stop)
            {
                return StopField;
            }

            return new TField {
                Type = type,
                ID = await ReadI16Async(cancellationToken)
            };
        }

        public override async Task ReadFieldEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async ValueTask<TMap> ReadMapBeginAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<TMap>(cancellationToken);
            }

            var map = new TMap
            {
                KeyType = (TType) await ReadByteAsync(cancellationToken),
                ValueType = (TType) await ReadByteAsync(cancellationToken),
                Count = await ReadI32Async(cancellationToken)
            };

            return map;
        }

        public override async Task ReadMapEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async ValueTask<TList> ReadListBeginAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<TList>(cancellationToken);
            }

            var list = new TList
            {
                ElementType = (TType) await ReadByteAsync(cancellationToken),
                Count = await ReadI32Async(cancellationToken)
            };

            return list;
        }

        public override async Task ReadListEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async ValueTask<TSet> ReadSetBeginAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<TSet>(cancellationToken);
            }

            var set = new TSet
            {
                ElementType = (TType) await ReadByteAsync(cancellationToken),
                Count = await ReadI32Async(cancellationToken)
            };

            return set;
        }

        public override async Task ReadSetEndAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        public override async ValueTask<bool> ReadBoolAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<bool>(cancellationToken);
            }

            return await ReadByteAsync(cancellationToken) == 1;
        }

        public override async ValueTask<sbyte> ReadByteAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<sbyte>(cancellationToken);
            }

            await Trans.ReadAllAsync(PreAllocatedBuffer, 0, 1, cancellationToken);
            return (sbyte)PreAllocatedBuffer[0];
        }

        public override async ValueTask<short> ReadI16Async(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<short>(cancellationToken);
            }

            await Trans.ReadAllAsync(PreAllocatedBuffer, 0, 2, cancellationToken);
            var result = (short) (((PreAllocatedBuffer[0] & 0xff) << 8) | PreAllocatedBuffer[1] & 0xff);
            return result;
        }

        public override async ValueTask<int> ReadI32Async(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<int>(cancellationToken);
            }

            await Trans.ReadAllAsync(PreAllocatedBuffer, 0, 4, cancellationToken);

            var result = 
                ((PreAllocatedBuffer[0] & 0xff) << 24) | 
                ((PreAllocatedBuffer[1] & 0xff) << 16) | 
                ((PreAllocatedBuffer[2] & 0xff) << 8) |
                PreAllocatedBuffer[3] & 0xff;

            return result;
        }

#pragma warning disable 675

        protected internal long ReadI64FromPreAllocatedBuffer()
        {
            var result =
                ((long) (PreAllocatedBuffer[0] & 0xff) << 56) |
                ((long) (PreAllocatedBuffer[1] & 0xff) << 48) |
                ((long) (PreAllocatedBuffer[2] & 0xff) << 40) |
                ((long) (PreAllocatedBuffer[3] & 0xff) << 32) |
                ((long) (PreAllocatedBuffer[4] & 0xff) << 24) |
                ((long) (PreAllocatedBuffer[5] & 0xff) << 16) |
                ((long) (PreAllocatedBuffer[6] & 0xff) << 8) |
                PreAllocatedBuffer[7] & 0xff;

            return result;
        }

#pragma warning restore 675

        public override async ValueTask<long> ReadI64Async(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<long>(cancellationToken);
            }

            await Trans.ReadAllAsync(PreAllocatedBuffer, 0, 8, cancellationToken);
            return ReadI64FromPreAllocatedBuffer();
        }

        public override async ValueTask<double> ReadDoubleAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<double>(cancellationToken);
            }

            var d = await ReadI64Async(cancellationToken);
            return BitConverter.Int64BitsToDouble(d);
        }

        public override async ValueTask<byte[]> ReadBinaryAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<byte[]>(cancellationToken);
            }

            var size = await ReadI32Async(cancellationToken);
            var buf = new byte[size];
            await Trans.ReadAllAsync(buf, 0, size, cancellationToken);
            return buf;
        }

        public override async ValueTask<string> ReadStringAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                return await Task.FromCanceled<string>(cancellationToken);
            }

            var size = await ReadI32Async(cancellationToken);
            return size > 0 ? await ReadStringBodyAsync(size, cancellationToken) : string.Empty;
        }

        private async ValueTask<string> ReadStringBodyAsync(int size, CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled<string>(cancellationToken);
            }

            if (size <= PreAllocatedBuffer.Length)
            {
                await Trans.ReadAllAsync(PreAllocatedBuffer, 0, size, cancellationToken);
                return Encoding.UTF8.GetString(PreAllocatedBuffer, 0, size);
            }

            var buf = new byte[size];
            await Trans.ReadAllAsync(buf, 0, size, cancellationToken);
            return Encoding.UTF8.GetString(buf, 0, buf.Length);
        }

        public class Factory : TProtocolFactory
        {
            protected bool StrictRead;
            protected bool StrictWrite;

            public Factory()
                : this(false, true)
            {
            }

            public Factory(bool strictRead, bool strictWrite)
            {
                StrictRead = strictRead;
                StrictWrite = strictWrite;
            }

            public override TProtocol GetProtocol(TTransport trans)
            {
                return new TBinaryProtocol(trans, StrictRead, StrictWrite);
            }
        }
    }
}
