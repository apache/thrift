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
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Threading.Tasks;

#pragma warning disable IDE0079  // unneeded suppression -> all except net8
#pragma warning disable IDE0301  // simplify collection init -> net8 only
#pragma warning disable IDE0305  // simplify collection init -> net8 only

namespace Thrift.Transport.Client
{
    // ReSharper disable once InconsistentNaming
    public class THttpTransport : TEndpointTransport, ITPerCallTransportProvider
    {
        private readonly X509Certificate[] _certificates;
        private readonly Uri _uri;
        private readonly object _lifecycleLock = new object();
        private readonly HashSet<THttpPerCallTransport> _perCallTransports = new HashSet<THttpPerCallTransport>();
        private CancellationTokenSource _lifecycleCancellation = new CancellationTokenSource();
        private bool _lifecycleOpen = true;
        private int _flushInProgress;

        private int _connectTimeout = 30000; // Timeouts in milliseconds
        private HttpClient _httpClient;
        private Stream _inputStream;
        private MemoryStream _outputStream = new MemoryStream();
        private HttpResponseMessage _response;
        private CancellationTokenSource _responseReadTimeout;
        private bool _isDisposed;

        public THttpTransport(Uri uri, TConfiguration config, IDictionary<string, string> customRequestHeaders = null, string userAgent = null)
            : this(uri, config, Enumerable.Empty<X509Certificate>(), customRequestHeaders, userAgent)
        {
        }

        public THttpTransport(Uri uri, TConfiguration config, IEnumerable<X509Certificate> certificates,
            IDictionary<string, string> customRequestHeaders, string userAgent = null)
            : base(config)
        {
            _uri = uri;
            _certificates = (certificates ?? Enumerable.Empty<X509Certificate>()).ToArray();

            if (!string.IsNullOrEmpty(userAgent))
                UserAgent = userAgent;

            // due to current bug with performance of Dispose in netcore https://github.com/dotnet/corefx/issues/8809
            // this can be switched to default way (create client->use->dispose per flush) later
            _httpClient = CreateClient(customRequestHeaders);
            ConfigureClient(_httpClient);
        }

        /// <summary>
        /// Constructor that takes a <c>HttpClient</c> instance to support using <c>IHttpClientFactory</c>.
        /// </summary>
        /// <remarks>As the <c>HttpMessageHandler</c> of the client must be configured at the time of creation, it
        /// is assumed that the consumer has already added any certificates and configured decompression methods. The
        /// consumer can use the <c>CreateHttpClientHandler</c> method to get a handler with these set.</remarks>
        /// <param name="httpClient">Client configured with the desired message handler, user agent, and URI if not
        /// specified in the <c>uri</c> parameter. A default user agent will be used if not set.</param>
        /// <param name="config">Thrift configuration object</param>
        /// <param name="uri">Optional URI to use for requests, if not specified the base address of <c>httpClient</c>
        /// is used.</param>
        public THttpTransport(HttpClient httpClient, TConfiguration config, Uri uri = null)
            : base(config)
        {
            _httpClient = httpClient;

            _uri = uri ?? httpClient.BaseAddress;
            httpClient.BaseAddress = _uri;

            var userAgent = _httpClient.DefaultRequestHeaders.UserAgent.ToString();
            if (!string.IsNullOrEmpty(userAgent))
                UserAgent = userAgent;

            ConfigureClient(_httpClient);
        }

        // According to RFC 2616 section 3.8, the "User-Agent" header may not carry a version number
        public readonly string UserAgent = "Thrift netstd THttpClient";

        public int ConnectTimeout
        {
            set
            {
                _connectTimeout = value;
                if(_httpClient != null)
                    _httpClient.Timeout = TimeSpan.FromMilliseconds(_connectTimeout);
            }
            get
            {
                if (_httpClient == null)
                    return _connectTimeout;
                return (int)_httpClient.Timeout.TotalMilliseconds;
            }
        }

        public override bool IsOpen
        {
            get
            {
                lock (_lifecycleLock)
                {
                    return _lifecycleOpen && _httpClient != null;
                }
            }
        }

        public HttpRequestHeaders RequestHeaders => _httpClient.DefaultRequestHeaders;

        public MediaTypeHeaderValue ContentType { get; set; }

        public override Task OpenAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            lock (_lifecycleLock)
            {
                if (!_lifecycleOpen || _httpClient == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }
            }
            return Task.CompletedTask;
        }

        public override void Close()
        {
            List<THttpPerCallTransport> transports;
            CancellationTokenSource lifecycleCancellation;
            lock (_lifecycleLock)
            {
                _lifecycleOpen = false;
                transports = new List<THttpPerCallTransport>(_perCallTransports);
                _perCallTransports.Clear();
                lifecycleCancellation = _lifecycleCancellation;
                _lifecycleCancellation = null;

                _inputStream?.Dispose();
                _inputStream = null;
                var outputStream = _outputStream;
                _outputStream = null;
                if (_flushInProgress == 0)
                {
                    outputStream?.Dispose();
                }
                _response?.Dispose();
                _response = null;
                _responseReadTimeout?.Dispose();
                _responseReadTimeout = null;
                _httpClient?.Dispose();
                _httpClient = null;
            }

            lifecycleCancellation?.Cancel();
            lifecycleCancellation?.Dispose();
            foreach (var transport in transports)
            {
                transport.Dispose();
            }
        }

        public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (_inputStream == null)
                throw new TTransportException(TTransportException.ExceptionType.NotOpen, "No request has been sent");

            CheckReadBytesAvailable(length);

            CancellationTokenSource readCts = null;
            Stream inputStream;
            bool hasResponseReadTimeout;
            lock (_lifecycleLock)
            {
                if (!_lifecycleOpen || _inputStream == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }

                inputStream = _inputStream;
                hasResponseReadTimeout = _responseReadTimeout != null;
                readCts = CancellationTokenSource.CreateLinkedTokenSource(
                    cancellationToken,
                    _lifecycleCancellation.Token,
                    _responseReadTimeout?.Token ?? CancellationToken.None);
            }

            try
            {
                var readToken = readCts?.Token ?? cancellationToken;
                int ret;
#if NET5_0_OR_GREATER
                ret = await inputStream.ReadAsync(new Memory<byte>(buffer, offset, length), readToken);
#else
                ret = await inputStream.ReadAsync(buffer, offset, length, readToken);
#endif
                if (ret == -1)
                {
                    throw new TTransportException(TTransportException.ExceptionType.EndOfFile, "No more data available");
                }

                CountConsumedMessageBytes(ret);
                return ret;
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

        public override async Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            Stream outputStream;
            lock (_lifecycleLock)
            {
                if (!_lifecycleOpen || _outputStream == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }

                outputStream = _outputStream;
            }

#if NET5_0_OR_GREATER
            await outputStream.WriteAsync(buffer.AsMemory(offset, length), cancellationToken);
#else
            await outputStream.WriteAsync(buffer, offset, length, cancellationToken);
#endif
        }

        /// <summary>
        /// Get a client handler configured with recommended properties to use with the <c>HttpClient</c> constructor
        /// and an <c>IHttpClientFactory</c>.
        /// </summary>
        /// <param name="certificates">An optional array of client certificates to associate with the handler.</param>
        /// <returns>
        /// A client handler with deflate and gZip compression-decompression algorithms and any client
        /// certificates passed in via <c>certificates</c>.
        /// </returns>
        public virtual HttpClientHandler CreateHttpClientHandler(X509Certificate[] certificates = null)
        {
            var handler = new HttpClientHandler();
            if (certificates != null)
                handler.ClientCertificates.AddRange(certificates);
            handler.AutomaticDecompression = System.Net.DecompressionMethods.Deflate | System.Net.DecompressionMethods.GZip;
            return handler;
        }

        private HttpClient CreateClient(IDictionary<string, string> customRequestHeaders)
        {
            var handler = CreateHttpClientHandler(_certificates);
            var httpClient = new HttpClient(handler);


            if (customRequestHeaders != null)
            {
                foreach (var item in customRequestHeaders)
                {
                    httpClient.DefaultRequestHeaders.Add(item.Key, item.Value);
                }
            }

            return httpClient;
        }

        private void ConfigureClient(HttpClient httpClient)
        {
            if (_connectTimeout > 0)
            {
                httpClient.Timeout = TimeSpan.FromMilliseconds(_connectTimeout);
            }

            httpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/x-thrift"));

            // Clear any user agent values to avoid drift with the field value
            httpClient.DefaultRequestHeaders.UserAgent.Clear();
            httpClient.DefaultRequestHeaders.UserAgent.TryParseAdd(UserAgent);

            httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("deflate"));
            httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("gzip"));
        }

        public override async Task FlushAsync(CancellationToken cancellationToken)
        {
            HttpClient httpClient;
            Stream outputStream;
            int connectTimeout;
            CancellationTokenSource operationCts;
            lock (_lifecycleLock)
            {
                if (!_lifecycleOpen || _httpClient == null || _outputStream == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }
                httpClient = _httpClient;
                outputStream = _outputStream;
                connectTimeout = _connectTimeout;
                operationCts = CancellationTokenSource.CreateLinkedTokenSource(
                    cancellationToken,
                    _lifecycleCancellation.Token);
                _flushInProgress++;
            }

            StreamContent contentStream = null;
            HttpRequestMessage request = null;
            HttpResponseMessage response = null;
            CancellationTokenSource responseReadTimeout = null;
            Stream inputStream = null;
            var responseDeadline = Stopwatch.StartNew();
            try
            {
                outputStream.Seek(0, SeekOrigin.Begin);

                contentStream = new StreamContent(outputStream);
                contentStream.Headers.ContentType = ContentType ?? new MediaTypeHeaderValue(@"application/x-thrift");

                request = new HttpRequestMessage(HttpMethod.Post, _uri)
                {
                    Content = contentStream
                };

                response = await httpClient.SendAsync(
                    request,
                    HttpCompletionOption.ResponseHeadersRead,
                    operationCts.Token);
                response.EnsureSuccessStatusCode();

               responseReadTimeout = new CancellationTokenSource();
               var remainingTimeout = connectTimeout - (int)responseDeadline.ElapsedMilliseconds;
               if (connectTimeout > 0)
               {
                   responseReadTimeout.CancelAfter(Math.Max(0, remainingTimeout));
               }

                using (var acquisitionCts = CancellationTokenSource.CreateLinkedTokenSource(
                    operationCts.Token, responseReadTimeout.Token))
                {
                    inputStream = await ReadAsStreamAsync(response.Content, acquisitionCts.Token);
                }
                if (inputStream.CanSeek)
                {
                    inputStream.Seek(0, SeekOrigin.Begin);
                }

                // Only take ownership of the new response/timeout/stream once stream acquisition
                // has succeeded, so a failed FlushAsync leaves no dangling resources behind.
                lock (_lifecycleLock)
                {
                    if (!_lifecycleOpen || !ReferenceEquals(_httpClient, httpClient))
                    {
                        throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                            "The transport has been closed.");
                    }

                    _inputStream?.Dispose();
                    _inputStream = inputStream;
                    inputStream = null;
                    _response?.Dispose();
                    _response = response;
                    response = null;
                    _responseReadTimeout?.Dispose();
                    _responseReadTimeout = responseReadTimeout;
                    responseReadTimeout = null;
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
            catch (ObjectDisposedException odx)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                    "The transport has been closed.", odx);
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
                inputStream?.Dispose();
                response?.Dispose();
                responseReadTimeout?.Dispose();
                operationCts?.Dispose();
                request?.Dispose();
                if (request == null)
                {
                    contentStream?.Dispose();
                }

                lock (_lifecycleLock)
                {
                    _flushInProgress--;
                    if (_lifecycleOpen)
                    {
                        _outputStream = new MemoryStream();
                    }
                    else
                    {
                        outputStream.Dispose();
                        _outputStream = null;
                    }
                    ResetMessageSizeAndConsumedBytes();
                }
            }
        }


        // IDisposable
        protected override void Dispose(bool disposing)
        {
            if (_isDisposed)
            {
                return;
            }

            if (disposing)
            {
                Close();
            }
            _isDisposed = true;
        }

        public ValueTask<TTransport> CreatePerCallTransportAsync(CancellationToken cancellationToken = default)
        {
            cancellationToken.ThrowIfCancellationRequested();
            lock (_lifecycleLock)
            {
                if (!_lifecycleOpen || _httpClient == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }

                var transport = new THttpPerCallTransport(this);
                _perCallTransports.Add(transport);
                return new ValueTask<TTransport>(transport);
            }
        }

        internal object LifecycleLock => _lifecycleLock;

        internal bool IsLifecycleOpen
        {
            get
            {
                lock (_lifecycleLock)
                {
                    return _lifecycleOpen && _httpClient != null;
                }
            }
        }

        internal void RemovePerCallTransport(THttpPerCallTransport transport)
        {
            lock (_lifecycleLock)
            {
                _perCallTransports.Remove(transport);
            }
        }

        internal static async Task<Stream> ReadAsStreamAsync(HttpContent content, CancellationToken cancellationToken)
        {
#if NET5_0_OR_GREATER
            return await content.ReadAsStreamAsync(cancellationToken);
#else
            var readTask = content.ReadAsStreamAsync();
            if (readTask.IsCompleted || !cancellationToken.CanBeCanceled)
            {
                return await readTask;
            }

            var cancellationTask = new TaskCompletionSource<bool>();
            using (cancellationToken.Register(() => cancellationTask.TrySetResult(true)))
            {
                if (await Task.WhenAny(readTask, cancellationTask.Task) != readTask)
                {
                    // The legacy API cannot cancel acquisition. If it later succeeds, dispose
                    // the stream because cancellation has already abandoned the acquisition.
                    _ = readTask.ContinueWith(
                        task =>
                        {
                            if (task.Status == TaskStatus.RanToCompletion)
                            {
                                task.Result.Dispose();
                            }
                            else if (task.IsFaulted)
                            {
                                var ignored = task.Exception;
                            }
                        },
                        CancellationToken.None,
                        TaskContinuationOptions.ExecuteSynchronously,
                        TaskScheduler.Default);
                    throw new OperationCanceledException(cancellationToken);
                }
            }

            return await readTask;
#endif
        }

        /// <summary>
        /// Sends the given request stream to the server and returns the HTTP response.
        /// </summary>
        /// <param name="request">The request stream to send.</param>
        /// <param name="cancellationToken">The cancellation token to cancel the operation.</param>
        /// <returns>The HTTP response message.</returns>
        /// <exception cref="TTransportException">Thrown if an error occurs during the request.</exception>
        internal async Task<HttpResponseMessage> SendAsync(Stream request, CancellationToken cancellationToken)
        {
            // Snapshot the client under the lifecycle lock so Close()/Dispose() cannot race
            // the state check and publish a response after the parent has closed.
            HttpClient httpClient;
            lock (_lifecycleLock)
            {
                if (!_lifecycleOpen || _httpClient == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.");
                }
                httpClient = _httpClient;
            }

            // Wrap the caller-owned request stream so that disposing the HttpRequestMessage/
            // StreamContent below (which we do deterministically once the request has been sent)
            // does not dispose the stream itself; that remains owned and disposed by the caller.
            var content = new StreamContent(new NonDisposingStream(request));
            content.Headers.ContentType = ContentType ?? new MediaTypeHeaderValue("application/x-thrift");

            using (var message = new HttpRequestMessage(HttpMethod.Post, _uri) { Content = content })
            {
                HttpResponseMessage response = null;
                try
                {
                    response = await httpClient.SendAsync(
                        message,
                        HttpCompletionOption.ResponseHeadersRead,
                        cancellationToken);

                    response.EnsureSuccessStatusCode();
                    lock (_lifecycleLock)
                    {
                        if (!_lifecycleOpen || !ReferenceEquals(_httpClient, httpClient))
                        {
                            throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                                "The transport has been closed.");
                        }
                    }
                    var result = response;
                    response = null;
                    return result;
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
                catch (ObjectDisposedException odx)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen,
                        "The transport has been closed.", odx);
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
                    response?.Dispose();
                }
            }
        }

        /// <summary>
        /// A <see cref="Stream"/> wrapper whose <c>Dispose</c> is a no-op, used to let an
        /// <see cref="HttpRequestMessage"/>/<see cref="StreamContent"/> pair be disposed
        /// deterministically without disposing a caller-owned inner stream.
        /// </summary>
        private sealed class NonDisposingStream : Stream
        {
            private readonly Stream _inner;

            public NonDisposingStream(Stream inner) => _inner = inner;

            public override bool CanRead => _inner.CanRead;
            public override bool CanSeek => _inner.CanSeek;
            public override bool CanWrite => _inner.CanWrite;
            public override long Length => _inner.Length;
            public override long Position { get => _inner.Position; set => _inner.Position = value; }

            public override void Flush() => _inner.Flush();
            public override int Read(byte[] buffer, int offset, int count) => _inner.Read(buffer, offset, count);
            public override long Seek(long offset, SeekOrigin origin) => _inner.Seek(offset, origin);
            public override void SetLength(long value) => _inner.SetLength(value);
            public override void Write(byte[] buffer, int offset, int count) => _inner.Write(buffer, offset, count);

            public override Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken) =>
                _inner.ReadAsync(buffer, offset, count, cancellationToken);

#if NET5_0_OR_GREATER
            public override ValueTask<int> ReadAsync(Memory<byte> buffer, CancellationToken cancellationToken = default) =>
                _inner.ReadAsync(buffer, cancellationToken);
#endif

            protected override void Dispose(bool disposing)
            {
                // Intentionally do not dispose the inner stream; it is owned by the caller.
                base.Dispose(disposing);
            }
        }
    }
}
