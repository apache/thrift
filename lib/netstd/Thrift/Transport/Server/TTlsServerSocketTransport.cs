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
using System.Net;
using System.Net.Security;
using System.Net.Sockets;
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Threading.Tasks;
using Thrift.Transport.Client;

namespace Thrift.Transport.Server
{
    // ReSharper disable once InconsistentNaming
    public class TTlsServerSocketTransport : TServerTransport
    {
        /// <summary>
        /// The default of <see cref="HandshakeTimeout"/>: 10 seconds.
        /// </summary>
        public static readonly TimeSpan DefaultHandshakeTimeout = TimeSpan.FromSeconds(10);

        private readonly RemoteCertificateValidationCallback _clientCertValidator;
        private readonly int _clientTimeout = 0;
        private readonly LocalCertificateSelectionCallback _localCertificateSelectionCallback;
        private readonly X509Certificate2 _serverCertificate;
        private readonly SslProtocols _sslProtocols;
        private TcpListener _server;
        private TimeSpan _handshakeTimeout = DefaultHandshakeTimeout;

        /// <summary>
        /// How long <see cref="TServerTransport.AcceptAsync"/> waits for a connected client to complete the TLS
        /// handshake. A client that takes longer is disconnected, and AcceptAsync fails with
        /// <see cref="TTransportException.ExceptionType.TimedOut"/>, so that the server can accept the next client.
        /// <see cref="Timeout.InfiniteTimeSpan"/> waits without a limit. Defaults to <see cref="DefaultHandshakeTimeout"/>.
        /// </summary>
        public TimeSpan HandshakeTimeout
        {
            get => _handshakeTimeout;
            set
            {
                if (value != Timeout.InfiniteTimeSpan && (value <= TimeSpan.Zero || value.TotalMilliseconds > int.MaxValue))
                {
                    throw new ArgumentOutOfRangeException(nameof(value), value,
                        "HandshakeTimeout must be positive and at most int.MaxValue milliseconds, or Timeout.InfiniteTimeSpan");
                }
                _handshakeTimeout = value;
            }
        }

        public TTlsServerSocketTransport(
            TcpListener listener,
            TConfiguration config,
            X509Certificate2 certificate,
            RemoteCertificateValidationCallback clientCertValidator = null,
            LocalCertificateSelectionCallback localCertificateSelectionCallback = null,
            SslProtocols sslProtocols = TTlsSocketTransport.DefaultSslProtocols)
            : base(config)
        {
            if (!certificate.HasPrivateKey)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown,
                    "Your server-certificate needs to have a private key");
            }

            _serverCertificate = certificate;
            _clientCertValidator = clientCertValidator;
            _localCertificateSelectionCallback = localCertificateSelectionCallback;
            _sslProtocols = sslProtocols;
            _server = listener;
        }

        public TTlsServerSocketTransport(
            int port,
            TConfiguration config,
            X509Certificate2 certificate,
            RemoteCertificateValidationCallback clientCertValidator = null,
            LocalCertificateSelectionCallback localCertificateSelectionCallback = null,
            SslProtocols sslProtocols = TTlsSocketTransport.DefaultSslProtocols)
            : this(null, config, certificate, clientCertValidator, localCertificateSelectionCallback, sslProtocols)
        {
            try
            {
                // Create server socket
                _server = new TcpListener(IPAddress.Any, port);
                _server.Server.NoDelay = true;
            }
            catch (Exception ex)
            {
                _server = null;
                throw new TTransportException($"Could not create ServerSocket on port {port}.", ex);
            }
        }

        public override bool IsOpen()
        {
            return (_server != null) 
				&& (_server.Server != null) 
				&& _server.Server.IsBound;
        }

        public int GetPort()
        {
            if ((_server != null) && (_server.Server != null) && (_server.Server.LocalEndPoint != null))
            {
                if (_server.Server.LocalEndPoint is IPEndPoint server)
                {
                    return server.Port;
                }
                else
                {
                    throw new TTransportException("ServerSocket is not a network socket");
                }
            }
            else
            {
                throw new TTransportException("ServerSocket is not open");
            }
        }

        public override void Listen()
        {
            // Make sure accept is not blocking
            if (_server != null)
            {
                try
                {
                    _server.Start();
                }
                catch (SocketException sx)
                {
                    throw new TTransportException($"Could not accept on listening socket: {sx.Message}", sx);
                }
            }
        }

        public override bool IsClientPending()
        {
            return _server.Pending();
        }

        protected override async ValueTask<TTransport> AcceptImplementationAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (_server == null)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen, "No underlying server socket.");
            }

            try
            {
                #if NET6_0_OR_GREATER
                var client = await _server.AcceptTcpClientAsync(cancellationToken).ConfigureAwait(false);
                #else
                var client = await _server.AcceptTcpClientAsync().ConfigureAwait(false);
                #endif
                client.SendTimeout = client.ReceiveTimeout = _clientTimeout;

                //wrap the client in an SSL Socket passing in the SSL cert
                var tTlsSocket = new TTlsSocketTransport(
                    client, Configuration,
                    _serverCertificate, true, _clientCertValidator,
                    _localCertificateSelectionCallback, _sslProtocols);

                await SetupTlsWithinHandshakeTimeoutAsync(tTlsSocket, cancellationToken).ConfigureAwait(false);

                return tTlsSocket;
            }
            catch (TTransportException ttx) when (ttx.Type == TTransportException.ExceptionType.TimedOut)
            {
                throw;
            }
            catch (Exception ex)
            {
                throw new TTransportException(ex.ToString(), ex);
            }
        }

        private async Task SetupTlsWithinHandshakeTimeoutAsync(TTlsSocketTransport tTlsSocket, CancellationToken cancellationToken)
        {
            var handshakeTimeout = _handshakeTimeout;
            using (var handshake = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken))
            {
                if (handshakeTimeout != Timeout.InfiniteTimeSpan)
                {
                    handshake.CancelAfter(handshakeTimeout);
                }

                try
                {
                    await tTlsSocket.SetupTlsAsync(handshake.Token).ConfigureAwait(false);
                }
                catch (OperationCanceledException) when (!cancellationToken.IsCancellationRequested)
                {
                    // SetupTlsAsync has closed the connection.
                    throw new TTransportException(TTransportException.ExceptionType.TimedOut,
                        $"The client did not complete the TLS handshake within {handshakeTimeout.TotalMilliseconds} ms.");
                }
            }
        }

        public override void Close()
        {
            if (_server != null)
            {
                try
                {
                    _server.Stop();
                }
                catch (Exception ex)
                {
                    throw new TTransportException($"WARNING: Could not close server socket: {ex}", ex);
                }

                _server = null;
            }
        }
    }
}
