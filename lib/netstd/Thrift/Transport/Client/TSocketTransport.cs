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
using System.Net.Sockets;
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport.Client
{
    // ReSharper disable once InconsistentNaming
    public class TSocketTransport : TStreamTransport
    {
        private bool _isDisposed;


        public TSocketTransport(TcpClient client, TConfiguration config)
            : base(config)
        {
            TcpClient = client ?? throw new ArgumentNullException(nameof(client));
            SetInputOutputStream();
        }

        /// <summary>
        /// The constructor for a TSocketTransport which takes an IPAddress object.
        /// </summary>
        /// <param name="host">The IP address.</param>
        /// <param name="port">The TcpClient port number.</param>
        /// <param name="config">The <see cref="TConfiguration"/>.</param>
        /// <param name="timeout">The TcpClient send timeout.</param>
        /// <remarks>
        /// The TcpClient is not connected automatically.  You are required to use <see cref="OpenAsync(CancellationToken)"/>.
        /// </remarks>
        public TSocketTransport(IPAddress host, int port, TConfiguration config, int timeout = 0)
            : base(config)
        {
            Host = host;
            Port = port;

            TcpClient = new TcpClient();
            TcpClient.ReceiveTimeout = TcpClient.SendTimeout = timeout;
            TcpClient.Client.NoDelay = true;

            SetInputOutputStream();
        }

        /// <summary>
        /// The constructor for a TSocketTransport which takes either a host name, e.g. 'host.example.com' and port number.
        /// If host is not found using Dns.GetHostEntry(host) an exception will be thrown.
        /// </summary>
        /// <param name="host">The host name.</param>
        /// <param name="port">The TcpClient port number.</param>
        /// <param name="config">The <see cref="TConfiguration"/>.</param>
        /// <param name="timeout">The TcpClient send timeout.</param>
        /// <exception cref="TTransportException"></exception>
        /// <remarks>
        /// The TcpClient is connected automatically.
        /// </remarks>
        public TSocketTransport(string host, int port, TConfiguration config, int timeout = 0)
            : base(config)
        {
            try
            {
                var entry = Dns.GetHostEntry(host);
                if (entry.AddressList.Length == 0)
                    throw new TTransportException(TTransportException.ExceptionType.Unknown, "unable to resolve host name");

                Host = entry.AddressList[0];
                Port = port;

                TcpClient = new TcpClient(host, port);
                TcpClient.ReceiveTimeout = TcpClient.SendTimeout = timeout;
                TcpClient.Client.NoDelay = true;

                SetInputOutputStream();
            }
            catch (SocketException e)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown, e.Message, e);
            }
        }

        /// <summary>
        /// <para>The constructor for a TSocketTransport which takes either a host name, e.g. 'host.example.com' or and IP address string, e.g '123.456.789' and port number.
        /// If hostNameOrIpAddress represents a valid IP address this will be used directly.
        /// If hostNameOrIpAddress does not represent a valid IP address an IP address will be retrieved using Dns.GetHostEntry(hostNameOrIpAddress).
        /// If that fails an exception will be thrown.</para>
        /// </summary>
        /// <param name="hostNameOrIpAddress">The host name or IP address.</param>
        /// <param name="port">The TcpClient port number.</param>
        /// <param name="connectClient">If true attempt to connect the TcpClient.</param>
        /// <param name="config">The <see cref="TConfiguration"/>.</param>
        /// <param name="timeout">The TcpClient send timeout.</param>
        /// <exception cref="TTransportException"></exception>
        /// <remarks>
        /// The TcpClient is connected dependent on the value of <paramref name="connectClient"/>./>.
        /// </remarks>
        public TSocketTransport(string hostNameOrIpAddress, int port, bool connectClient, TConfiguration config, int timeout = 0)
            : base(config)
        {
            try
            {
                if (!IPAddress.TryParse(hostNameOrIpAddress, out var address))
                {
                    var entry = Dns.GetHostEntry(hostNameOrIpAddress);

                    if (entry.AddressList.Length == 0)
                        throw new TTransportException(TTransportException.ExceptionType.Unknown, "unable to resolve host name");

                    address = entry.AddressList[0];
                }

                Host = address;
                Port = port;

                TcpClient = new TcpClient();
                TcpClient.ReceiveTimeout = TcpClient.SendTimeout = timeout;
                TcpClient.Client.NoDelay = true;

                if (connectClient)
                {
                    TcpClient.Connect(Host, Port);
                }

                SetInputOutputStream();
            }
            catch (SocketException e)
            {
                throw new TTransportException(TTransportException.ExceptionType.Unknown, e.Message, e);
            }
        }

        private void SetInputOutputStream()
        { 
            if (IsOpen)
            {
                InputStream = TcpClient.GetStream();
                OutputStream = TcpClient.GetStream();
            }
        }

        public TcpClient TcpClient { get; private set; }
        public IPAddress Host { get; }
        public int Port { get; }

        public int Timeout
        {
            set
            {
                if (TcpClient != null)
                {
                    TcpClient.ReceiveTimeout = TcpClient.SendTimeout = value;
                }
            }
        }

        public override bool IsOpen
        {
            get
            {
                return (TcpClient != null) && TcpClient.Connected;
            }
        }

        public override async Task OpenAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (IsOpen)
            {
                throw new TTransportException(TTransportException.ExceptionType.AlreadyOpen, "Socket already connected");
            }

            if (Port <= 0)
            {
                throw new TTransportException(TTransportException.ExceptionType.NotOpen, "Cannot open without port");
            }

            if (TcpClient == null)
            {
                throw new InvalidOperationException("Invalid or not initialized tcp client");
            }

            await TcpClient.ConnectAsync(Host, Port);
            SetInputOutputStream();
        }

        public override void Close()
        {
            base.Close();

            if (TcpClient != null)
            {
                TcpClient.Dispose();
                TcpClient = null;
            }
        }

        // IDisposable
        protected override void Dispose(bool disposing)
        {
            if (!_isDisposed)
            {
                if (disposing)
                {
                    TcpClient?.Dispose();

                    base.Dispose(disposing);
                }
            }
            _isDisposed = true;
        }
    }
}
