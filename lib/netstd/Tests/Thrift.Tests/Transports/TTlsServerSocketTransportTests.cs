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
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Thrift.Transport;
using Thrift.Transport.Server;

namespace Thrift.Tests.Transports
{
    // AcceptAsync() completes the TLS handshake before it returns a client. A client that connects and does not
    // finish the handshake must not keep AcceptAsync() waiting longer than HandshakeTimeout, so that the server's
    // accept loop can go on with the next client.
    [TestClass]
    public class TTlsServerSocketTransportTests
    {
        // Every wait has a limit, so that a regression fails the test instead of hanging it.
        private static readonly TimeSpan WaitLimit = TimeSpan.FromSeconds(10);

        private static X509Certificate2 NewServerCertificate()
        {
            using var rsa = RSA.Create(2048);
            var request = new CertificateRequest("CN=localhost", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
            using var ephemeral = request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
            // A PKCS#12 round trip gives a certificate whose private key every platform's TLS stack can use.
            return X509CertificateLoader.LoadPkcs12(ephemeral.Export(X509ContentType.Pkcs12), null);
        }

        private static TTlsServerSocketTransport NewServer(X509Certificate2 certificate, TimeSpan? handshakeTimeout)
        {
            var server = new TTlsServerSocketTransport(new TcpListener(IPAddress.Loopback, 0), new TConfiguration(),
                certificate, (sender, cert, chain, errors) => true);
            if (handshakeTimeout.HasValue)
            {
                server.HandshakeTimeout = handshakeTimeout.Value;
            }
            server.Listen();
            return server;
        }

        private static async Task<TcpClient> ConnectSilentlyAsync(int port)
        {
            var client = new TcpClient();
            await client.ConnectAsync(IPAddress.Loopback, port);
            return client;
        }

        private static async Task<SslStream> ConnectWithTlsAsync(int port, CancellationToken cancellationToken)
        {
            var client = new TcpClient();
            await client.ConnectAsync(IPAddress.Loopback, port, cancellationToken);
            var tls = new SslStream(client.GetStream(), false, (sender, cert, chain, errors) => true);
            await tls.AuthenticateAsClientAsync(new SslClientAuthenticationOptions { TargetHost = "localhost" }, cancellationToken);
            return tls;
        }

        [TestMethod]
        public void HandshakeTimeout_HasAFiniteDefault()
        {
            using var certificate = NewServerCertificate();
            var server = new TTlsServerSocketTransport(new TcpListener(IPAddress.Loopback, 0), new TConfiguration(), certificate);

            Assert.AreEqual(TTlsServerSocketTransport.DefaultHandshakeTimeout, server.HandshakeTimeout);
            Assert.AreEqual(TimeSpan.FromSeconds(10), TTlsServerSocketTransport.DefaultHandshakeTimeout);
        }

        [TestMethod]
        public void HandshakeTimeout_RejectsZeroAndNegativeValues()
        {
            using var certificate = NewServerCertificate();
            var server = new TTlsServerSocketTransport(new TcpListener(IPAddress.Loopback, 0), new TConfiguration(), certificate);

            Assert.ThrowsExactly<ArgumentOutOfRangeException>(() => server.HandshakeTimeout = TimeSpan.Zero);
            Assert.ThrowsExactly<ArgumentOutOfRangeException>(() => server.HandshakeTimeout = TimeSpan.FromSeconds(-2));
            server.HandshakeTimeout = Timeout.InfiniteTimeSpan; // no limit, as before
            Assert.AreEqual(Timeout.InfiniteTimeSpan, server.HandshakeTimeout);
        }

        [TestMethod]
        public async Task AcceptAsync_GivesUpOnAClientThatDoesNotStartTheHandshake()
        {
            using var certificate = NewServerCertificate();
            var server = NewServer(certificate, TimeSpan.FromMilliseconds(300));
            try
            {
                using var silent = await ConnectSilentlyAsync(server.GetPort());

                var accept = server.AcceptAsync(CancellationToken.None).AsTask();
                var first = await Task.WhenAny(accept, Task.Delay(WaitLimit));

                Assert.AreSame(accept, first, "AcceptAsync() still waits for the client's handshake");
                var error = await Assert.ThrowsExactlyAsync<TTransportException>(() => accept);
                Assert.AreEqual(TTransportException.ExceptionType.TimedOut, error.Type);
            }
            finally
            {
                server.Close();
            }
        }

        [TestMethod]
        public async Task AcceptAsync_AcceptsTheNextClientAfterAClientThatDoesNotStartTheHandshake()
        {
            using var certificate = NewServerCertificate();
            var server = NewServer(certificate, TimeSpan.FromMilliseconds(300));
            try
            {
                using var silent = await ConnectSilentlyAsync(server.GetPort());
                using var limit = new CancellationTokenSource(WaitLimit);

                await Assert.ThrowsExactlyAsync<TTransportException>(() => server.AcceptAsync(limit.Token).AsTask());

                var clientTask = ConnectWithTlsAsync(server.GetPort(), limit.Token);
                using var accepted = await server.AcceptAsync(limit.Token);
                using var client = await clientTask;

                Assert.IsTrue(accepted.IsOpen);
            }
            finally
            {
                server.Close();
            }
        }

        [TestMethod]
        public async Task AcceptAsync_ReturnsAClientThatCompletesTheHandshakeInTime()
        {
            using var certificate = NewServerCertificate();
            var server = NewServer(certificate, null);
            try
            {
                using var limit = new CancellationTokenSource(WaitLimit);
                var clientTask = ConnectWithTlsAsync(server.GetPort(), limit.Token);

                using var accepted = await server.AcceptAsync(limit.Token);
                using var client = await clientTask;

                Assert.IsTrue(accepted.IsOpen);
            }
            finally
            {
                server.Close();
            }
        }
    }
}
