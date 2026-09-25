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
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport.Client
{
    /// <summary>
    /// Implementation of a per-call transport for THttpTransport.
    /// Each call to the server will create a new HTTP request and response.
    /// </summary>
    internal class THttpPerCallTransport : TEndpointTransport
    {
        private readonly THttpTransport _parent;
        private readonly MemoryStream _request = new MemoryStream();
        private readonly CancellationTokenSource _lifecycleCancellation = new CancellationTokenSource();

        private HttpResponseMessage _response;
        private Stream _responseStream;
        private CancellationTokenSource _responseReadTimeout;
        private bool _sent;
        private bool _disposed;

        public THttpPerCallTransport(THttpTransport parent) : base(parent.Configuration)
        {
            this._parent = parent;
        }

        public override bool IsOpen
        {
            get
            {
                lock (_parent.LifecycleLock)
                {
                    return !_disposed && _parent.IsLifecycleOpen;
                }
            }
        }

        public override Task WriteAsync(
            byte[] buffer,
            int offset,
            int length,
            CancellationToken cancellationToken)
        {
            lock (_parent.LifecycleLock)
            {
                if (_disposed || !_parent.IsLifecycleOpen)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }
                if (_sent)
                {
                    throw new InvalidOperationException("The request has already been sent.");
                }

                return _request.WriteAsync(buffer, offset, length, cancellationToken);
            }
        }

        public override async Task FlushAsync(
            CancellationToken cancellationToken)
        {
            int connectTimeout;
            var responseDeadline = Stopwatch.StartNew();
            CancellationTokenSource operationCts;
            lock (_parent.LifecycleLock)
            {
                if (_disposed || !_parent.IsLifecycleOpen)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }
                if (_sent)
                {
                    throw new InvalidOperationException("The request has already been sent.");
                }

                // Once a send is attempted the server may have processed the RPC, even if
                // response acquisition fails. Require a fresh per-call transport to retry.
                _sent = true;
                _request.Position = 0;
                connectTimeout = _parent.ConnectTimeout;
                operationCts = CancellationTokenSource.CreateLinkedTokenSource(
                    cancellationToken,
                    _lifecycleCancellation.Token);
            }

            HttpResponseMessage response = null;
            CancellationTokenSource responseReadTimeout = null;
            Stream responseStream = null;
            try
            {
                response = await _parent.SendAsync(_request, operationCts.Token);
                lock (_parent.LifecycleLock)
                {
                    if (_disposed || !_parent.IsLifecycleOpen)
                    {
                        throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                            "The transport has been closed.");
                    }

                    responseReadTimeout = new CancellationTokenSource();
                    if (connectTimeout > 0)
                    {
                        var remainingTimeout = connectTimeout - (int)responseDeadline.ElapsedMilliseconds;
                        responseReadTimeout.CancelAfter(Math.Max(0, remainingTimeout));
                    }
                }
                using (var acquisitionCts = CancellationTokenSource.CreateLinkedTokenSource(
                    operationCts.Token, responseReadTimeout.Token))
                {
                    responseStream = await THttpTransport.ReadAsStreamAsync(
                        response.Content, acquisitionCts.Token);
                }
                if (responseStream.CanSeek)
                {
                    responseStream.Seek(0, SeekOrigin.Begin);
                }

                // Only take ownership of the new response/timeout/stream once stream acquisition
                // and normalization have succeeded, so a failed FlushAsync leaves no dangling
                // resources behind and a subsequent retry does not leak the previous response.
                lock (_parent.LifecycleLock)
                {
                    if (_disposed || !_parent.IsLifecycleOpen)
                    {
                        throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                            "The transport has been closed.");
                    }

                    _response?.Dispose();
                    _response = response;
                    response = null;
                    _responseReadTimeout?.Dispose();
                    _responseReadTimeout = responseReadTimeout;
                    responseReadTimeout = null;
                    _responseStream = responseStream;
                    responseStream = null;
                }
            }
            catch (TTransportException)
            {
                throw;
            }
            catch (IOException iox)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown, iox.ToString(), iox);
            }
            catch (HttpRequestException wx)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown,
                    "Couldn't connect to server: " + wx, wx);
            }
            catch (OperationCanceledException ocx)
            {
                throw new TTransportException(TTransportException.ExceptionType.Interrupted, ocx.Message, ocx);
            }
            catch (Exception ex)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown, ex.Message, ex);
            }
            finally
            {
                responseStream?.Dispose();
                response?.Dispose();
                responseReadTimeout?.Dispose();
                operationCts.Dispose();
            }
        }

        public override async ValueTask<int> ReadAsync(
            byte[] buffer,
            int offset,
            int length,
            CancellationToken cancellationToken)
        {
            if (!_sent)
            {
                throw new TTransportException( TTransportException.ExceptionType.NotOpen, "The request has not been sent.");
            }

            CheckReadBytesAvailable(length);

            CancellationTokenSource readCts = null;
            Stream responseStream;
            bool hasResponseReadTimeout;
            lock (_parent.LifecycleLock)
            {
                if (_disposed || !_parent.IsLifecycleOpen || _responseStream == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }

                responseStream = _responseStream;
                hasResponseReadTimeout = _responseReadTimeout != null;
                readCts = CancellationTokenSource.CreateLinkedTokenSource(
                    cancellationToken,
                    _lifecycleCancellation.Token,
                    _responseReadTimeout?.Token ?? CancellationToken.None);
            }

            var readToken = readCts?.Token ?? cancellationToken;
            try
            {
                var count = await responseStream.ReadAsync(
                    buffer,
                    offset,
                    length,
                    readToken);

                CountConsumedMessageBytes(count);
                return count;
            }
            catch (OperationCanceledException ocx) when (hasResponseReadTimeout && !cancellationToken.IsCancellationRequested)
            {
                throw new TTransportException(TTransportException.ExceptionType.Interrupted, ocx.Message, ocx);
            }
            catch (ObjectDisposedException odx)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                    "The transport has been closed.", odx);
            }
            catch (IOException iox)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown, iox.ToString(), iox);
            }
            finally
            {
                readCts?.Dispose();
            }
        }

        protected override void Dispose(bool disposing)
        {
            bool shouldCancel = false;
            lock (_parent.LifecycleLock)
            {
                if (_disposed)
                {
                    return;
                }

                if (disposing)
                {
                    shouldCancel = true;
                    _response?.Dispose();
                    _response = null;
                    _responseReadTimeout?.Dispose();
                    _responseReadTimeout = null;
                    _request.Dispose();
                }

                _disposed = true;
                _parent.RemovePerCallTransport(this);
            }

            if (shouldCancel)
            {
                _lifecycleCancellation.Cancel();
                _lifecycleCancellation.Dispose();
            }
        }

        public override Task OpenAsync(CancellationToken cancellationToken = default)
        {
            cancellationToken.ThrowIfCancellationRequested();
            lock (_parent.LifecycleLock)
            {
                if (_disposed || !_parent.IsLifecycleOpen)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }
            }
            return Task.CompletedTask;
        }

        public override void Close()
        {
            // Do not dispose the parent transport, as it is shared across multiple per-call transports.
            this.Dispose();
        }
    }
}
