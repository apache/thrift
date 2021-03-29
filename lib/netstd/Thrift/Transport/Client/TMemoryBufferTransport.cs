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

namespace Thrift.Transport.Client
{
    // ReSharper disable once InconsistentNaming
    public class TMemoryBufferTransport : TEndpointTransport
    {
        private bool IsDisposed;
        private byte[] Bytes;
        private int _bytesUsed;

        public TMemoryBufferTransport(TConfiguration config, int initialCapacity = 2048)
            : base(config)
        {
            Bytes = new byte[initialCapacity];  
        }

        public TMemoryBufferTransport(byte[] buf, TConfiguration config)
            :base(config)
        {
            Bytes = (byte[])buf.Clone();
            _bytesUsed = Bytes.Length;
            UpdateKnownMessageSize(_bytesUsed);
        }

        public int Position { get; set; }

        public int Capacity
        {
            get
            {
                Debug.Assert(_bytesUsed <= Bytes.Length);
                return Bytes.Length;
            }
            set
            {
                Array.Resize(ref Bytes, value);
                _bytesUsed = value;
            }
        }

        public int Length
        {
            get {
                Debug.Assert(_bytesUsed <= Bytes.Length);
                return _bytesUsed;
            }
            set {
                if ((Bytes.Length < value) || (Bytes.Length > (10 * value)))
                    Array.Resize(ref Bytes, Math.Max(2048, (int)(value * 1.25)));
                _bytesUsed = value;
            }
        }

        public void SetLength(int value)
        {
            Length = value;
            Position = Math.Min(Position, value);
        }

        public override bool IsOpen => true;

        public override Task OpenAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override void Close()
        {
            /** do nothing **/
        }

        public void Seek(int delta, SeekOrigin origin)
        {
            int newPos;
            switch (origin)
            {
                case SeekOrigin.Begin:
                    newPos = delta;
                    break;
                case SeekOrigin.Current:
                    newPos = Position + delta;
                    break;
                case SeekOrigin.End:
                    newPos = _bytesUsed + delta;
                    break;
                default:
                    throw new ArgumentException(nameof(origin));
            }

            if ((0 > newPos) || (newPos > _bytesUsed))
                throw new ArgumentException(nameof(origin));
            Position = newPos;

            ResetConsumedMessageSize();
            CountConsumedMessageBytes(Position);
        }

        public override ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            var count = Math.Min(Length - Position, length);
            Buffer.BlockCopy(Bytes, Position, buffer, offset, count);
            Position += count;
            CountConsumedMessageBytes(count);
            return new ValueTask<int>(count);
        }

        public override Task WriteAsync(byte[] buffer, CancellationToken cancellationToken)
        {
            return WriteAsync(buffer, 0, buffer.Length, cancellationToken);
        }

        public override Task WriteAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken)
        {
            var free = Length - Position;
            Length = Length + count - free;
            Buffer.BlockCopy(buffer, offset, Bytes, Position, count);
            Position += count;
            return Task.CompletedTask;
        }

        public override Task FlushAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            ResetConsumedMessageSize();
            return Task.CompletedTask;
        }

        public byte[] GetBuffer()
        {
            var retval = new byte[Length];
            Buffer.BlockCopy(Bytes, 0, retval, 0, Length);
            return retval;
        }

        internal bool TryGetBuffer(out ArraySegment<byte> bufSegment)
        {
            bufSegment = new ArraySegment<byte>(Bytes, 0, _bytesUsed);
            return true;
        }

        // IDisposable
        protected override void Dispose(bool disposing)
        {
            if (!IsDisposed)
            {
                if (disposing)
                {
                    // nothing to do
                }
            }
            IsDisposed = true;
        }
    }
}
