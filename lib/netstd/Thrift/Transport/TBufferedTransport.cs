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
using System.Diagnostics;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport
{
    // ReSharper disable once InconsistentNaming
    public class TBufferedTransport : TTransport
    {
        private readonly int DesiredBufferSize;
        private readonly Client.TMemoryBufferTransport ReadBuffer = new Client.TMemoryBufferTransport(1024);
        private readonly Client.TMemoryBufferTransport WriteBuffer = new Client.TMemoryBufferTransport(1024);
        private readonly TTransport InnerTransport;
        private bool IsDisposed;

        public class Factory : TTransportFactory
        {
            public override TTransport GetTransport(TTransport trans)
            {
                return new TBufferedTransport(trans);
            }
        }

        //TODO: should support only specified input transport?
        public TBufferedTransport(TTransport transport, int bufSize = 1024)
        {
            if (bufSize <= 0)
            {
                throw new ArgumentOutOfRangeException(nameof(bufSize), "Buffer size must be a positive number.");
            }

            InnerTransport = transport ?? throw new ArgumentNullException(nameof(transport));
            DesiredBufferSize = bufSize;

            if (DesiredBufferSize != ReadBuffer.Capacity)
                ReadBuffer.Capacity = DesiredBufferSize;
            if (DesiredBufferSize != WriteBuffer.Capacity)
                WriteBuffer.Capacity = DesiredBufferSize;
        }

        public TTransport UnderlyingTransport
        {
            get
            {
                CheckNotDisposed();

                return InnerTransport;
            }
        }

        public override bool IsOpen => !IsDisposed && InnerTransport.IsOpen;

        public override async Task OpenAsync(CancellationToken cancellationToken)
        {
            CheckNotDisposed();

            await InnerTransport.OpenAsync(cancellationToken);
        }

        public override void Close()
        {
            CheckNotDisposed();

            InnerTransport.Close();
        }

        public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            CheckNotDisposed();
            ValidateBufferArgs(buffer, offset, length);

            if (!IsOpen)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }


            // do we have something buffered?
            var count = ReadBuffer.Length - ReadBuffer.Position;
            if (count > 0)
            {
                return await ReadBuffer.ReadAsync(buffer, offset, length, cancellationToken);
            }

            // does the request even fit into the buffer?
            // Note we test for >= instead of > to avoid nonsense buffering
            if (length >= ReadBuffer.Capacity)
            {
                return await InnerTransport.ReadAsync(buffer, offset, length, cancellationToken);
            }

            // buffer a new chunk of bytes from the underlying transport
            ReadBuffer.Length = ReadBuffer.Capacity;
            ArraySegment<byte> bufSegment;
            ReadBuffer.TryGetBuffer(out bufSegment);
            ReadBuffer.Length = await InnerTransport.ReadAsync(bufSegment.Array, 0, bufSegment.Count, cancellationToken);
            ReadBuffer.Position = 0;

            // deliver the bytes
            return await ReadBuffer.ReadAsync(buffer, offset, length, cancellationToken);
        }


        public override async Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            CheckNotDisposed();
            ValidateBufferArgs(buffer, offset, length);

            if (!IsOpen)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }

            // enough space left in buffer?
            var free = WriteBuffer.Capacity - WriteBuffer.Length;
            if (length > free)
            {
                ArraySegment<byte> bufSegment;
                WriteBuffer.TryGetBuffer(out bufSegment);
                await InnerTransport.WriteAsync(bufSegment.Array, 0, bufSegment.Count, cancellationToken);
                WriteBuffer.SetLength(0);
            }

            // do the data even fit into the buffer?
            // Note we test for < instead of <= to avoid nonsense buffering
            if (length < WriteBuffer.Capacity)
            {
                await WriteBuffer.WriteAsync(buffer, offset, length, cancellationToken);
                return;
            }

            // write thru
            await InnerTransport.WriteAsync(buffer, offset, length, cancellationToken);
        }

        public override async Task FlushAsync(CancellationToken cancellationToken)
        {
            CheckNotDisposed();

            if (!IsOpen)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }

            if (WriteBuffer.Length > 0)
            {
                ArraySegment<byte> bufSegment;
                WriteBuffer.TryGetBuffer(out bufSegment);
                await InnerTransport.WriteAsync(bufSegment.Array, 0, bufSegment.Count, cancellationToken);
                WriteBuffer.SetLength(0);
            }

            await InnerTransport.FlushAsync(cancellationToken);
        }

        private void CheckNotDisposed()
        {
            if (IsDisposed)
            {
                throw new ObjectDisposedException(nameof(InnerTransport));
            }
        }

        // IDisposable
        protected override void Dispose(bool disposing)
        {
            if (!IsDisposed)
            {
                if (disposing)
                {
                    ReadBuffer?.Dispose();
                    WriteBuffer?.Dispose();
                    InnerTransport?.Dispose();
                }
            }
            IsDisposed = true;
        }
    }
}
