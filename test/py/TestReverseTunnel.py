#!/usr/bin/env python

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

import sys
from multiprocessing import Process, Queue

from thrift.transport.TTransport import TTransportException
from thrift.server import TServer, TReverseTunnelServer
from thrift.protocol import TBinaryProtocol
from thrift.transport import TTransport
from thrift.transport import TSocket
from ThriftTest import ThriftTest
import unittest

success = False


class ReverseTunnelServerTest(unittest.TestCase):

    def _server(self, q):
        sys.path.append('gen-py')

        class TestHandler(object):
            def testString(self, str):
                global success
                success = True
                return str

        handler = TestHandler()
        processor = ThriftTest.Processor(handler)
        q.get()
        transport = TSocket.TSocket('localhost', 9090)
        transport = TReverseTunnelServer.TReverseTunnelServer(transport, 1)
        tfactory = TTransport.TBufferedTransportFactory()
        pfactory = TBinaryProtocol.TBinaryProtocolFactory()
        server = TServer.TSimpleServer(processor, transport, tfactory, pfactory)
        try:
            server.serve()
        except TTransportException:
            # After the client has executed it will close the socket leaving the
            # server not able to connect. This is expected as long as we successfully ran
            if not success:
                raise

    def _client(self, q):
        transport = TSocket.TServerSocket(host='127.0.0.1', port=9090)
        transport.listen()
        q.put(True)
        transport = transport.accept()
        if not transport:
            raise Exception("No transport TSocket")

        # Buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(transport)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = ThriftTest.Client(protocol)
        new_str = "hello World"
        res = client.testString(new_str)
        self.assertEqual(new_str, res)
        transport.close()

    def testReverse(self):
        queue = Queue()
        cli = Process(target=self._client, args=(queue,))
        cli.start()
        server = Process(target=self._server, args=(queue,))
        server.start()
        cli.join()
        server.join()


if __name__ == '__main__':
    sys.path.append('gen-py/ThriftTest/')
    suite = unittest.TestSuite()
    loader = unittest.TestLoader()
    suite.addTest(loader.loadTestsFromTestCase(ReverseTunnelServerTest))

    testRunner = unittest.TextTestRunner(verbosity=2)
    testRunner.run(suite)
