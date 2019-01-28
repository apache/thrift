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

using System.IO.Pipes;
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport.Client
{
    // ReSharper disable once InconsistentNaming
    public class TNamedPipeTransport : TTransport
    {
        private NamedPipeClientStream _client;
        private int ConnectTimeout;

        public TNamedPipeTransport(string pipe, int timeout = Timeout.Infinite) 
            : this(".", pipe, timeout)
        {
        }

        public TNamedPipeTransport(string server, string pipe, int timeout = Timeout.Infinite)
        {
            var serverName = string.IsNullOrWhiteSpace(server) ? server : ".";
            ConnectTimeout = (timeout > 0) ? timeout : Timeout.Infinite;

            _client = new NamedPipeClientStream(serverName, pipe, PipeDirection.InOut, PipeOptions.None);
        }

        public override bool IsOpen => _client != null && _client.IsConnected;

        public override async Task OpenAsync(CancellationToken cancellationToken)
        {
            if (IsOpen)
            {
                throw new TTransportException(TTransportException.ExceptionType.AlreadyOpen);
            }

            await _client.ConnectAsync( ConnectTimeout, cancellationToken);
        }

        public override void Close()
        {
            if (_client != null)
            {
                _client.Dispose();
                _client = null;
            }
        }

        public override async Task<int> ReadAsync(byte[] buffer, int offset, int length,
            CancellationToken cancellationToken)
        {
            if (_client == null)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }

            return await _client.ReadAsync(buffer, offset, length, cancellationToken);
        }

        public override async Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            if (_client == null)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);
            }

            await _client.WriteAsync(buffer, offset, length, cancellationToken);
        }

        public override async Task FlushAsync(CancellationToken cancellationToken)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                await Task.FromCanceled(cancellationToken);
            }
        }

        protected override void Dispose(bool disposing)
        {
            _client.Dispose();
        }
    }
}