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
using System.Buffers.Binary;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport
{
    // ReSharper disable once InconsistentNaming
    public class TFramedTransport : TLayeredTransport
    {
        private const int HeaderSize = 4;
        private readonly byte[] HeaderBuf = new byte[HeaderSize];
        private readonly Client.TMemoryBufferTransport ReadBuffer;
        private readonly Client.TMemoryBufferTransport WriteBuffer;

        private bool IsDisposed;

        public class Factory : TTransportFactory
        {
            public override TTransport GetTransport(TTransport trans)
            {
                return new TFramedTransport(trans);
            }
        }

        public TFramedTransport(TTransport transport)
            : base(transport)
        {
            ReadBuffer = new Client.TMemoryBufferTransport(Configuration);
            WriteBuffer = new Client.TMemoryBufferTransport(Configuration);
            InitWriteBuffer();
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

            // Read another frame of data if we run out of bytes
            if (ReadBuffer.Position >= ReadBuffer.Length)
            {
                await ReadFrameAsync(cancellationToken);
            }

            return await ReadBuffer.ReadAsync(buffer, offset, length, cancellationToken);
        }

        private async ValueTask ReadFrameAsync(CancellationToken cancellationToken)
        {
            await InnerTransport.ReadAllAsync(HeaderBuf, 0, HeaderSize, cancellationToken);
            int size = BinaryPrimitives.ReadInt32BigEndian(HeaderBuf);

            if ((0 > size) || (size > Configuration.MaxFrameSize))  // size must be in the range 0 to allowed max
                throw new TTransportException(TTransportException.ExceptionType.Unknown, $"Maximum frame size exceeded ({size} bytes)");
            UpdateKnownMessageSize(size + HeaderSize);

            ReadBuffer.SetLength(size);
            ReadBuffer.Seek(0, SeekOrigin.Begin);

            ArraySegment<byte> bufSegment;
            ReadBuffer.TryGetBuffer(out bufSegment);
            await InnerTransport.ReadAllAsync(bufSegment.Array, 0, size, cancellationToken);
        }

        public override async Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            CheckNotDisposed();
            ValidateBufferArgs(buffer, offset, length);

            if (!IsOpen)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }

            if (WriteBuffer.Length > (int.MaxValue - length))
            {
                await FlushAsync(cancellationToken);
            }

            await WriteBuffer.WriteAsync(buffer, offset, length, cancellationToken);
        }

        public override async Task FlushAsync(CancellationToken cancellationToken)
        {
            CheckNotDisposed();

            if (!IsOpen)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }

            ArraySegment<byte> bufSegment;
            WriteBuffer.TryGetBuffer(out bufSegment);

            int dataLen = bufSegment.Count - HeaderSize;
            if (dataLen < 0)
            {
                throw new InvalidOperationException(); // logic error actually
            }

            // Inject message header into the reserved buffer space
            BinaryPrimitives.WriteInt32BigEndian(bufSegment.Array, dataLen);

            // Send the entire message at once
            await InnerTransport.WriteAsync(bufSegment.Array, 0, bufSegment.Count, cancellationToken);

            InitWriteBuffer();

            await InnerTransport.FlushAsync(cancellationToken);
        }

        private void InitWriteBuffer()
        {
            // Reserve space for message header to be put right before sending it out
            WriteBuffer.SetLength(HeaderSize);
            WriteBuffer.Seek(0, SeekOrigin.End);
        }

        public override void CheckReadBytesAvailable(long numBytes)
        {
            var buffered = ReadBuffer.Length - ReadBuffer.Position;
            if (buffered < numBytes)
            {
                numBytes -= buffered;
                InnerTransport.CheckReadBytesAvailable(numBytes);
            }
        }

        private void CheckNotDisposed()
        {
            if (IsDisposed)
            {
                throw new ObjectDisposedException(this.GetType().Name);
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
