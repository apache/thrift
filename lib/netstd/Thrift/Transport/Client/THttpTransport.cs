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
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport.Client
{
    // ReSharper disable once InconsistentNaming
    public class THttpTransport : TEndpointTransport
    {
        private readonly X509Certificate[] _certificates;
        private readonly Uri _uri;

        private int _connectTimeout = 30000; // Timeouts in milliseconds
        private HttpClient _httpClient;
        private Stream _inputStream;
        private MemoryStream _outputStream = new MemoryStream();
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

        public override bool IsOpen => true;

        public HttpRequestHeaders RequestHeaders => _httpClient.DefaultRequestHeaders;

        public MediaTypeHeaderValue ContentType { get; set; }

        public override Task OpenAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.CompletedTask;
        }

        public override void Close()
        {
            if (_inputStream != null)
            {
                _inputStream.Dispose();
                _inputStream = null;
            }

            if (_outputStream != null)
            {
                _outputStream.Dispose();
                _outputStream = null;
            }

            if (_httpClient != null)
            {
                _httpClient.Dispose();
                _httpClient = null;
            }
        }

        public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (_inputStream == null)
                throw new TTransportException(TTransportException.ExceptionType.NotOpen, "No request has been sent");

            CheckReadBytesAvailable(length);

            try
            {
#if NETSTANDARD2_1
                var ret = await _inputStream.ReadAsync(new Memory<byte>(buffer, offset, length), cancellationToken);
#else
                var ret = await _inputStream.ReadAsync(buffer, offset, length, cancellationToken);
#endif
                if (ret == -1)
                {
                    throw new TTransportException(TTransportException.ExceptionType.EndOfFile, "No more data available");
                }

                CountConsumedMessageBytes(ret);
                return ret;
            }
            catch (IOException iox)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown, iox.ToString());
            }
        }

        public override async Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            await _outputStream.WriteAsync(buffer, offset, length, cancellationToken);
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
            try
            {
                _outputStream.Seek(0, SeekOrigin.Begin);

                using (var contentStream = new StreamContent(_outputStream))
                {
                    contentStream.Headers.ContentType = ContentType ?? new MediaTypeHeaderValue(@"application/x-thrift");

                    var response = (await _httpClient.PostAsync(_uri, contentStream, cancellationToken)).EnsureSuccessStatusCode();

                    _inputStream?.Dispose();
                    _inputStream = await response.Content.ReadAsStreamAsync();
                    if (_inputStream.CanSeek)
                    {
                        _inputStream.Seek(0, SeekOrigin.Begin);
                    }
                }
            }
            catch (IOException iox)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown, iox.ToString());
            }
            catch (HttpRequestException wx)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown,
                    "Couldn't connect to server: " + wx);
            }
            catch (Exception ex)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown, ex.Message);
            }
            finally
            {
                _outputStream = new MemoryStream();
                ResetConsumedMessageSize();
            }
        }


        // IDisposable
        protected override void Dispose(bool disposing)
        {
            if (!_isDisposed)
            {
                if (disposing)
                {
                    _inputStream?.Dispose();
                    _outputStream?.Dispose();
                    _httpClient?.Dispose();
                }
            }
            _isDisposed = true;
        }
    }
}
