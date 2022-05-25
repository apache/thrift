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
        private readonly RemoteCertificateValidationCallback _clientCertValidator;
        private readonly int _clientTimeout = 0;
        private readonly LocalCertificateSelectionCallback _localCertificateSelectionCallback;
        private readonly X509Certificate2 _serverCertificate;
        private readonly SslProtocols _sslProtocols;
        private TcpListener _server;

        public TTlsServerSocketTransport(
            TcpListener listener,
            TConfiguration config,
            X509Certificate2 certificate,
            RemoteCertificateValidationCallback clientCertValidator = null,
            LocalCertificateSelectionCallback localCertificateSelectionCallback = null,
            SslProtocols sslProtocols = SslProtocols.Tls12)
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
            SslProtocols sslProtocols = SslProtocols.Tls12)
            : this(null, config, certificate, clientCertValidator, localCertificateSelectionCallback, sslProtocols)
        {
            try
            {
                // Create server socket
                _server = new TcpListener(IPAddress.Any, port);
                _server.Server.NoDelay = true;
            }
            catch (Exception)
            {
                _server = null;
                throw new TTransportException($"Could not create ServerSocket on port {port}.");
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
                    throw new TTransportException($"Could not accept on listening socket: {sx.Message}");
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
                var client = await _server.AcceptTcpClientAsync(cancellationToken);
                #else
                var client = await _server.AcceptTcpClientAsync();
                #endif
                client.SendTimeout = client.ReceiveTimeout = _clientTimeout;

                //wrap the client in an SSL Socket passing in the SSL cert
                var tTlsSocket = new TTlsSocketTransport(
                    client, Configuration,
                    _serverCertificate, true, _clientCertValidator,
                    _localCertificateSelectionCallback, _sslProtocols);

                await tTlsSocket.SetupTlsAsync();

                return tTlsSocket;
            }
            catch (Exception ex)
            {
                throw new TTransportException(ex.ToString());
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
                    throw new TTransportException($"WARNING: Could not close server socket: {ex}");
                }

                _server = null;
            }
        }
    }
}
