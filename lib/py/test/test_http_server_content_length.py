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
# Content-Length is one or more decimal digits (RFC 9110 8.6), and the
# whitespace around a field value is not part of the value (RFC 9110 5.5).
# Python's int() accepts more than that: a sign, underscores between digits,
# and any whitespace it knows of, including a no-break space. These tests hold
# THttpServer to the grammar.
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

WAIT = 2.0

# A strict binary CALL header followed by an empty argument struct.
BODY = (struct.pack('!i', -2147418111) + struct.pack('!i', 4) + b'ping' +
        struct.pack('!i', 0) + b'\x00')
LENGTH = str(len(BODY))


class RecordingProcessor(object):
    def __init__(self):
        self.names = []

    def on_message_begin(self, func):
        pass

    def process(self, iprot, oprot):
        name, _, _ = iprot.readMessageBegin()
        self.names.append(name)


def post(port, length):
    """Send BODY with the given Content-Length value and return the status.

    The header is sent as Latin-1, the encoding http.server decodes it with,
    so that any character of the value reaches the server unchanged. None means
    the server closed the connection without answering.
    """
    sock = socket.create_connection(('127.0.0.1', port))
    try:
        head = 'POST / HTTP/1.1\r\nHost: localhost\r\nContent-Length:%s\r\n\r\n' % length
        sock.sendall(head.encode('latin-1') + BODY)
        sock.settimeout(WAIT)
        data = b''
        while b'\r\n' not in data:
            chunk = sock.recv(1024)
            if not chunk:
                return None
            data += chunk
        return int(data.split(b' ', 2)[1])
    except ConnectionResetError:
        return None
    finally:
        sock.close()


class HttpServerContentLengthTest(unittest.TestCase):

    def setUp(self):
        self.processor = RecordingProcessor()
        server = THttpServer.THttpServer(
            self.processor, ('127.0.0.1', 0),
            TBinaryProtocol.TBinaryProtocolFactory())
        # Keep the output readable: no access log, and no traceback for the
        # requests these tests expect to fail.
        server.httpd.RequestHandlerClass.log_message = lambda *args: None
        server.httpd.handle_error = lambda request, client_address: None
        thread = threading.Thread(target=server.serve)
        thread.daemon = True
        thread.start()
        self.addCleanup(self.stop, server, thread)
        self.port = server.httpd.server_address[1]

    @staticmethod
    def stop(server, thread):
        server.httpd.shutdown()
        server.httpd.server_close()
        thread.join(WAIT)

    def assertRefused(self, length, statuses=(400,)):
        self.assertIn(post(self.port, length), statuses, repr(length))
        self.assertEqual(self.processor.names, [], repr(length))

    def assertServed(self, length):
        self.assertEqual(post(self.port, length), 200, repr(length))
        self.assertEqual(self.processor.names, ['ping'], repr(length))
        del self.processor.names[:]

    def test_digits_are_served(self):
        self.assertServed(' ' + LENGTH)
        self.assertServed(' 0' + LENGTH)

    def test_whitespace_around_the_value_is_not_part_of_it(self):
        for length in (' %s ' % LENGTH, ' %s\t' % LENGTH, '\t%s' % LENGTH, LENGTH):
            self.assertServed(length)

    def test_a_sign_is_refused(self):
        self.assertRefused(' +' + LENGTH)

    def test_underscores_are_refused(self):
        self.assertRefused(' %s_%s' % (LENGTH[0], LENGTH[1:]))

    def test_other_whitespace_is_refused(self):
        for space in ('\xa0', '\x0b', '\x0c', '\x85'):
            self.assertRefused(' ' + space + LENGTH)
            self.assertRefused(' ' + LENGTH + space)

    def test_other_forms_of_a_number_are_refused(self):
        for length in ('', ' ', ' 0x11', ' 1e1', ' 17.0', ' 1 7', ' -0'):
            self.assertRefused(length)

    def test_a_list_of_lengths_is_refused(self):
        self.assertRefused(' %s, %s' % (LENGTH, LENGTH))

    def test_more_digits_than_int_converts_are_refused(self):
        # From Python 3.11 on, int() refuses a string of more than 4300 digits
        # by default; before, such a length is simply too large.
        self.assertRefused(' ' + '1' * 5000, statuses=(400, 413))


if __name__ == '__main__':
    unittest.main()
