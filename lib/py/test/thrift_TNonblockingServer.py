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

import os
import sys
import threading
import unittest
import time

gen_path = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "gen-py")
sys.path.append(gen_path)
import _import_local_thrift  # noqa
from TestServer import TestServer
from thrift.transport import TSocket, TTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TNonblockingServer


class Handler:

    def add_and_get_msg(self, msg):
        return msg


class Server:

    def __init__(self):
        handler = Handler()
        processor = TestServer.Processor(handler)
        transport = TSocket.TServerSocket("127.0.0.1", 30030)
        self.server = TNonblockingServer.TNonblockingServer(processor, transport)

    def start_server(self):
        print("-------start server ------\n")
        self.server.serve()
        print("------stop server -----\n")

    def close_server(self):
        self.server.stop()
        self.server.close()


class Client:

    def start_client(self):
        transport = TSocket.TSocket("127.0.0.1", 30030)
        trans = TTransport.TFramedTransport(transport)
        protocol = TBinaryProtocol.TBinaryProtocol(trans)
        client = TestServer.Client(protocol)
        trans.open()
        self.msg = client.add_and_get_msg("hello thrift")

    def get_message(self):
        try:
            msg = self.msg
            return msg
        except AttributeError as e:
            raise e
            print("self.msg not exit\n")


class TestNonblockingServer(unittest.TestCase):

    def test_normalconnection(self):
        serve = Server()
        client = Client()

        serve_thread = threading.Thread(target=serve.start_server)
        client_thread = threading.Thread(target=client.start_client)
        serve_thread.start()
        time.sleep(10)
        client_thread.start()
        client_thread.join(0.5)
        try:
            msg = client.get_message()
            self.assertEqual("hello thrift", msg)
        except AssertionError as e:
            raise e
            print("assert failure")
        finally:
            serve.close_server()


if __name__ == '__main__':
    unittest.main()
