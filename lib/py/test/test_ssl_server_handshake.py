#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#
# A client of a TSSLServerSocket completes the TLS handshake before it is
# served. A client that connects and does not complete it must not hold up the
# server's other clients: accept() gives up on it after handshake_timeout, and
# TNonblockingServer advances every handshake from its select loop, so that the
# connections it already serves and the next client are answered meanwhile.
#

import os
import socket
import ssl
import struct
import sys
import threading
import time
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.protocol import TBinaryProtocol  # noqa
from thrift.server import TNonblockingServer, TServer  # noqa
from thrift.transport import TSSLSocket, TTransport  # noqa

SCRIPT_DIR = os.path.realpath(os.path.dirname(__file__))
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.dirname(SCRIPT_DIR)))
SERVER_CERT = os.path.join(ROOT_DIR, 'test', 'keys', 'server.crt')
SERVER_KEY = os.path.join(ROOT_DIR, 'test', 'keys', 'server.key')

# Every wait has a limit, so that a regression fails a test instead of hanging it.
WAIT = 5.0
PING = struct.pack('!i', 4) + b'ping'
PONG = struct.pack('!i', 4) + b'pong'


class PongProcessor(object):
    """Answers every 4-byte request with b'pong'."""

    def process(self, iprot, oprot):
        iprot.trans.readAll(4)
        oprot.trans.write(b'pong')
        oprot.trans.flush()


class ListeningServerSocket(TSSLSocket.TSSLServerSocket):
    """Tells when listen() is done: its handle is set before the socket is bound."""

    def __init__(self, *args, **kwargs):
        self.listening = threading.Event()
        super(ListeningServerSocket, self).__init__(*args, **kwargs)

    def listen(self):
        super(ListeningServerSocket, self).listen()
        self.listening.set()


def server_socket(**kwargs):
    return ListeningServerSocket(host='127.0.0.1', port=0,
                                 certfile=SERVER_CERT, keyfile=SERVER_KEY,
                                 ssl_version=ssl.PROTOCOL_TLS_SERVER, **kwargs)


def client_context():
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    context.check_hostname = False
    context.verify_mode = ssl.CERT_NONE
    return context


class SSLServerHandshakeTest(unittest.TestCase):

    def start(self, server, lsocket):
        thread = threading.Thread(target=server.serve)
        thread.daemon = True
        thread.start()
        self.assertTrue(lsocket.listening.wait(WAIT), 'server did not listen')
        return lsocket.handle.getsockname()[1]

    def start_nonblocking(self, **kwargs):
        lsocket = server_socket(**kwargs)
        server = TNonblockingServer.TNonblockingServer(PongProcessor(), lsocket, threads=2)

        def stop():
            server.stop()
            server.close()
        self.addCleanup(stop)
        return self.start(server, lsocket)

    def start_threaded(self, **kwargs):
        lsocket = server_socket(**kwargs)
        server = TServer.TThreadedServer(PongProcessor(), lsocket,
                                         TTransport.TFramedTransportFactory(),
                                         TBinaryProtocol.TBinaryProtocolFactory(), daemon=True)
        self.addCleanup(lsocket.close)
        return self.start(server, lsocket)

    def connect_silently(self, port):
        """A client that opens the TCP connection and sends nothing."""
        sock = socket.create_connection(('127.0.0.1', port), timeout=WAIT)
        self.addCleanup(sock.close)
        return sock

    def connect_tls(self, port):
        sock = socket.create_connection(('127.0.0.1', port), timeout=WAIT)
        self.addCleanup(sock.close)
        tls = client_context().wrap_socket(sock)
        self.addCleanup(tls.close)
        return tls

    def ping(self, sock):
        sock.sendall(PING)
        reply = b''
        while len(reply) < len(PONG):
            chunk = sock.recv(len(PONG) - len(reply))
            if not chunk:
                break
            reply += chunk
        return reply

    def test_handshake_timeout_has_a_finite_default(self):
        self.assertEqual(TSSLSocket.TSSLServerSocket.DEFAULT_HANDSHAKE_TIMEOUT, 10.0)
        self.assertEqual(server_socket().handshake_timeout, 10.0)
        self.assertIsNone(server_socket(handshake_timeout=None).handshake_timeout)

    def test_handshake_timeout_must_be_positive(self):
        for value in (0, -1, -0.5):
            with self.assertRaisesRegex(ValueError, 'must be positive'):
                server_socket(handshake_timeout=value)

    def test_accept_gives_up_on_a_client_that_does_not_start_the_handshake(self):
        lsocket = server_socket(handshake_timeout=0.3)
        lsocket.listen()
        self.addCleanup(lsocket.close)
        self.connect_silently(lsocket.handle.getsockname()[1])

        result = []
        acceptor = threading.Thread(target=lambda: result.append(lsocket.accept()))
        acceptor.daemon = True
        acceptor.start()
        acceptor.join(WAIT)

        self.assertFalse(acceptor.is_alive(), 'accept() still waits for the handshake')
        self.assertEqual(result, [None])

    def test_threaded_server_accepts_the_next_client_after_a_silent_one(self):
        port = self.start_threaded(handshake_timeout=0.3)
        self.connect_silently(port)

        self.assertEqual(self.ping(self.connect_tls(port)), PONG)

    def test_nonblocking_server_keeps_serving_while_a_client_has_not_started_the_handshake(self):
        port = self.start_nonblocking()
        established = self.connect_tls(port)
        self.assertEqual(self.ping(established), PONG)

        self.connect_silently(port)
        time.sleep(0.2)

        self.assertEqual(self.ping(established), PONG)
        self.assertEqual(self.ping(self.connect_tls(port)), PONG)

    def test_nonblocking_server_keeps_serving_after_a_client_leaves_during_the_handshake(self):
        port = self.start_nonblocking()
        silent = self.connect_silently(port)
        time.sleep(0.2)
        silent.close()

        self.assertEqual(self.ping(self.connect_tls(port)), PONG)

    def test_nonblocking_server_answers_a_request_whose_tls_record_arrives_in_two_parts(self):
        port = self.start_nonblocking()
        sock = socket.create_connection(('127.0.0.1', port), timeout=WAIT)
        self.addCleanup(sock.close)
        incoming, outgoing = ssl.MemoryBIO(), ssl.MemoryBIO()
        tls = client_context().wrap_bio(incoming, outgoing)

        def flush():
            data = outgoing.read()
            if data:
                sock.sendall(data)

        def receive():
            data = sock.recv(65536)
            if not data:
                raise ConnectionError('server closed the connection')
            incoming.write(data)

        while True:
            try:
                tls.do_handshake()
                break
            except ssl.SSLWantReadError:
                flush()
                receive()
        flush()

        tls.write(PING)
        record = outgoing.read()
        sock.sendall(record[:3])
        time.sleep(0.2)
        sock.sendall(record[3:])

        reply = b''
        deadline = time.time() + WAIT
        while len(reply) < len(PONG) and time.time() < deadline:
            try:
                reply += tls.read(len(PONG) - len(reply))
            except ssl.SSLWantReadError:
                receive()
        self.assertEqual(reply, PONG)


if __name__ == '__main__':
    logging_level = os.environ.get('THRIFT_TEST_LOG_LEVEL')
    if logging_level:
        import logging
        logging.basicConfig(level=logging_level)
    unittest.main()
