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

import unittest
import time
import threading

import _import_local_thrift  # noqa
from thrift.transport.TSocket import TServerSocket, TSocket


class Server:
    def __init__(self, host, port, **kwargs):
        self.server = TServerSocket(host, port, **kwargs)

    def start_server(self):
        self.server.listen()
        self.tc = self.server.accept()

    def receive_message_from_client(self, sz=1024):
        rec_message = self.tc.read(sz)
        return rec_message

    def send_message_to_client(self, buff):
        self.tc.write(buff)

    def close_server(self):
        if self.tc.isOpen():
            self.tc.close()
            self.server.close()


class Client:
    def __init__(self, host, port):
        self.host = host
        self.port = port

    def start_client(self):
        self.client = TSocket(self.host, self.port)
        self.client.open()

    def send_message_to_server(self, buff):
        self.client.write(buff)

    def receive_message_from_server(self, sz=1024):
        message = self.client.read(sz)
        return message

    def close_client(self):
        if self.client.isOpen():
            self.client.close()


class TestTSocket(unittest.TestCase):
    def test_nomal_connet(self):
        url = ("localhost", 30031)
        server = Server(*url)
        client = Client(*url)
        sev = threading.Thread(target=server.start_server)
        cln = threading.Thread(target=client.start_client)

        sev.start()
        time.sleep(10)
        cln.start()
        cln.join(0.5)

        client_send_message = "how are you ?"
        sever_send_message = "I fine"
        client.send_message_to_server(client_send_message.encode())
        s_message = server.receive_message_from_client()
        server.send_message_to_client(sever_send_message.encode())
        c_message = client.receive_message_from_server()
        client.close_client()
        server.close_server()

        self.assertEqual(client_send_message, s_message.decode())
        self.assertEqual(sever_send_message, c_message.decode())


if __name__ == '__main__':
    unittest.main()
