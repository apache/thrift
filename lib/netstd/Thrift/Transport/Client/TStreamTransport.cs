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
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport.Client
{
    // ReSharper disable once InconsistentNaming
    public class TStreamTransport : TEndpointTransport
    {
        private bool _isDisposed;

        protected TStreamTransport(TConfiguration config)
            :base(config)
        {
        }

        public TStreamTransport(Stream inputStream, Stream outputStream, TConfiguration config)
            : base(config)
        {
            InputStream = inputStream;
            OutputStream = outputStream;
        }

        protected Stream OutputStream { get; set; }

        private Stream _InputStream = null;
        protected Stream InputStream {
            get => _InputStream;
            set {
                _InputStream = value;
                ResetConsumedMessageSize();
            }
        }

        public override bool IsOpen => true;

        public override Task OpenAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override void Close()
        {
            if (InputStream != null)
            {
                InputStream.Dispose();
                InputStream = null;
            }

            if (OutputStream != null)
            {
                OutputStream.Dispose();
                OutputStream = null;
            }
        }

        public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            if (InputStream == null)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                    "Cannot read from null inputstream");
            }

#if NETSTANDARD2_1
            return await InputStream.ReadAsync(new Memory<byte>(buffer, offset, length), cancellationToken);
#else
            return await InputStream.ReadAsync(buffer, offset, length, cancellationToken);
#endif
        }

        public override async Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            if (OutputStream == null)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                    "Cannot write to null outputstream");
            }

            await OutputStream.WriteAsync(buffer, offset, length, cancellationToken);
        }

        public override async Task FlushAsync(CancellationToken cancellationToken)
        {
            await OutputStream.FlushAsync(cancellationToken);
            ResetConsumedMessageSize();
        }


        // IDisposable
        protected override void Dispose(bool disposing)
        {
            if (!_isDisposed)
            {
                if (disposing)
                {
                    InputStream?.Dispose();
                    OutputStream?.Dispose();
                }
            }
            _isDisposed = true;
        }
    }
}
