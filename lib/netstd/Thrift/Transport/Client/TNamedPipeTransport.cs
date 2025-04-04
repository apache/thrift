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
using System.IO.Pipes;
using System.Security.Principal;
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport.Client
{
    // ReSharper disable once InconsistentNaming
    public class TNamedPipeTransport : TEndpointTransport
    {
        private NamedPipeClientStream PipeStream;
        private readonly int ConnectTimeout;
        private const int DEFAULT_CONNECT_TIMEOUT = 60 * 1000;   // Timeout.Infinite is not a good default

        public TNamedPipeTransport(string pipe, TConfiguration config, int timeout = DEFAULT_CONNECT_TIMEOUT) 
            : this(".", pipe, config, timeout)
        {
        }

        public TNamedPipeTransport(string server, string pipe, TConfiguration config, int timeout = DEFAULT_CONNECT_TIMEOUT) 
            : base(config)
        {
            var serverName = string.IsNullOrWhiteSpace(server) ? server : ".";
            ConnectTimeout = (timeout > 0) ? timeout : DEFAULT_CONNECT_TIMEOUT;

            PipeStream = new NamedPipeClientStream(serverName, pipe, PipeDirection.InOut, PipeOptions.None, TokenImpersonationLevel.Anonymous);
        }

        public override bool IsOpen => PipeStream != null && PipeStream.IsConnected;

        public override async Task OpenAsync(CancellationToken cancellationToken)
        {
            if (IsOpen)
            {
                throw new TTransportException(TTransportException.ExceptionType.AlreadyOpen);
            }

            await PipeStream.ConnectAsync( ConnectTimeout, cancellationToken);
            ResetMessageSizeAndConsumedBytes();
        }

        public override void Close()
        {
            if (PipeStream != null)
            {
                if (PipeStream.IsConnected)
                    PipeStream.Close();
                PipeStream.Dispose();
                PipeStream = null;
            }
        }

        public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            if (PipeStream == null)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }

            CheckReadBytesAvailable(length);
#if NET5_0_OR_GREATER
            var numRead = await PipeStream.ReadAsync(new Memory<byte>(buffer, offset, length), cancellationToken);
#else
            var numRead = await PipeStream.ReadAsync(buffer, offset, length, cancellationToken);
#endif
            CountConsumedMessageBytes(numRead);
            return numRead;
        }

        public override async Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            if (PipeStream == null)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }

            // if necessary, send the data in chunks
            // there's a system limit around 0x10000 bytes that we hit otherwise
            // MSDN: "Pipe write operations across a network are limited to 65,535 bytes per write. For more information regarding pipes, see the Remarks section."
            var nBytes = Math.Min(15 * 4096, length); // 16 would exceed the limit
            while (nBytes > 0)
            {
#if NET5_0_OR_GREATER
                await PipeStream.WriteAsync(buffer.AsMemory(offset, nBytes), cancellationToken);
#else
                await PipeStream.WriteAsync(buffer, offset, nBytes, cancellationToken);
#endif
                offset += nBytes;
                length -= nBytes;
                nBytes = Math.Min(nBytes, length);
            }
        }

        public override async Task FlushAsync(CancellationToken cancellationToken)
        {
            await PipeStream.FlushAsync(cancellationToken);
            ResetMessageSizeAndConsumedBytes();
        }

        
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (PipeStream != null)
                {
                    if (PipeStream.IsConnected)
                        PipeStream.Close();
                    PipeStream.Dispose();
                    PipeStream = null;
                }
            }
        }
    }
}
