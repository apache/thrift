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
import _import_local_thrift  #noqa
from TProcesspoolServer import HelloService
from thrift.transport import TSocket, TTransport, TBufferedTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TProcessPoolServer
from thrift.TConfiguration import TConfiguration

class HelloServiceHandler:

    def say(self, msg):
        msg = "Received:" + msg
        return msg


class Server:

    def __init__(self):
        handler = HelloServiceHandler()
        processor = HelloService.Processor(handler)
        transport = TSocket.TServerSocket("localhost", 9090)
        self.server = TProcessPoolServer.TProcessPoolServer(processor, transport)

    def open_server(self):
        print("Starting thrift server in python")
        self.server.serve()

    def stop_server(self):
        self.server.stop()
        print("Stop server !")


class Client:
    config = TConfiguration()
    config.setMaxMessageSize(2000)
    def open_client(self):
        transport = TSocket.TSocket("localhost", 9090)
        transport = TBufferedTransport.TBufferedTransport(transport, self.config)
        protocol = TBinaryProtocol.TBinaryProtocol(transport)
        client = HelloService.Client(protocol)
        transport.open()
        self.msg = client.say("Hello!")

    def get_msg(self):
        msg = self.msg
        return msg


class Test_TProcessPoolServer(unittest.TestCase):

    def test_connect(self):
        s = Server()
        c = Client()
        serve_thread = threading.Thread(target=s.open_server)
        client_thread = threading.Thread(target=c.open_client)
        serve_thread.start()
        serve_thread.join(0.5)
        time.sleep(5)
        client_thread.start()
        client_thread.join(0.5)
        try:
            c.open_client()
            ret = c.get_msg()
            self.assertEqual("Received:Hello!", ret)
        except AssertionError as e:
            raise e
            print("assert failure")
        finally:
            s.stop_server()


if __name__ == '__main__':
    unittest.main()
