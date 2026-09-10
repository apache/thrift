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
# THttpServer used a request's Content-Length as the read size of its input
# transport, so one read asked the connection for as many bytes as the peer
# declared, and a length inside the message could ask for more than the body
# carried. These tests hold the server to a maximum body size, and to reading
# nothing past the end of the body.
#

import os
import socket
import struct
import sys
import threading
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.protocol import TBinaryProtocol  # noqa
from thrift.server import THttpServer  # noqa
from thrift.transport.TTransport import DEFAULT_MAX_FRAME_SIZE  # noqa

WAIT = 2.0


def message(name=b'ping'):
    """A strict binary CALL header followed by an empty argument struct."""
    return (struct.pack('!i', -2147418111) + struct.pack('!i', len(name)) + name
            + struct.pack('!i', 0) + b'\x00')


class RecordingProcessor(object):
    def __init__(self):
        self.names = []

    def on_message_begin(self, func):
        pass

    def process(self, iprot, oprot):
        name, _, _ = iprot.readMessageBegin()
        self.names.append(name)


def post(port, headers, body=b''):
    """Send a POST and return the status code.

    None means the server closed the connection without answering; 'timeout'
    means it did neither within WAIT seconds, which is what waiting for bytes
    the peer never sent looks like from the outside.
    """
    sock = socket.create_connection(('127.0.0.1', port))
    try:
        lines = ['POST / HTTP/1.1', 'Host: localhost']
        lines += ['%s: %s' % header for header in headers]
        sock.sendall(('\r\n'.join(lines) + '\r\n\r\n').encode('ascii') + body)
        sock.settimeout(WAIT)
        data = b''
        while b'\r\n' not in data:
            chunk = sock.recv(1024)
            if not chunk:
                return None
            data += chunk
        return int(data.split(b' ', 2)[1])
    except socket.timeout:
        return 'timeout'
    except ConnectionResetError:
        return None
    finally:
        sock.close()


class HttpServerBodySizeTest(unittest.TestCase):

    def serve(self, **kwargs):
        processor = RecordingProcessor()
        server = THttpServer.THttpServer(
            processor, ('127.0.0.1', 0),
            TBinaryProtocol.TBinaryProtocolFactory(), **kwargs)
        # Keep the output readable: no access log, and no traceback for the
        # requests these tests expect to fail.
        server.httpd.RequestHandlerClass.log_message = lambda *args: None
        server.httpd.handle_error = lambda request, client_address: None
        thread = threading.Thread(target=server.serve)
        thread.daemon = True
        thread.start()
        self.addCleanup(self.stop, server, thread)
        return processor, server.httpd.server_address[1]

    @staticmethod
    def stop(server, thread):
        server.httpd.shutdown()
        server.httpd.server_close()
        thread.join(WAIT)

    def test_a_body_over_the_maximum_is_refused_before_it_is_read(self):
        processor, port = self.serve(max_body_size=100)
        self.assertEqual(post(port, [('Content-Length', '101')]), 413)
        self.assertEqual(processor.names, [])

    def test_a_body_over_the_default_maximum_is_refused(self):
        processor, port = self.serve()
        length = str(DEFAULT_MAX_FRAME_SIZE + 1)
        self.assertEqual(post(port, [('Content-Length', length)]), 413)
        self.assertEqual(processor.names, [])

    def test_a_length_too_large_for_any_read_is_refused(self):
        processor, port = self.serve()
        self.assertEqual(post(port, [('Content-Length', str(10 ** 20))]), 413)
        self.assertEqual(processor.names, [])

    def test_a_missing_length_is_refused(self):
        processor, port = self.serve()
        self.assertEqual(post(port, [], message()), 411)
        self.assertEqual(processor.names, [])

    def test_a_malformed_or_negative_length_is_refused(self):
        processor, port = self.serve()
        for length in ('abc', '-1'):
            status = post(port, [('Content-Length', length)], message())
            self.assertEqual(status, 400, length)
        self.assertEqual(processor.names, [])

    def test_nothing_past_the_end_of_the_body_is_read(self):
        # An 8-byte body whose message name claims 0x7fffffff bytes.
        processor, port = self.serve()
        body = struct.pack('!ii', -2147418111, 0x7fffffff)
        self.assertIsNone(post(port, [('Content-Length', str(len(body)))], body))
        self.assertEqual(processor.names, [])

    def test_a_body_at_the_maximum_is_processed(self):
        body = message()
        processor, port = self.serve(max_body_size=len(body))
        self.assertEqual(post(port, [('Content-Length', str(len(body)))], body), 200)
        self.assertEqual(processor.names, ['ping'])

    def test_a_body_within_the_default_maximum_is_processed(self):
        body = message()
        processor, port = self.serve()
        self.assertEqual(post(port, [('Content-Length', str(len(body)))], body), 200)
        self.assertEqual(processor.names, ['ping'])

    def test_the_maximum_is_validated(self):
        with self.assertRaises(ValueError):
            THttpServer.THttpServer(
                RecordingProcessor(), ('127.0.0.1', 0),
                TBinaryProtocol.TBinaryProtocolFactory(), max_body_size=0)


if __name__ == '__main__':
    unittest.main()
