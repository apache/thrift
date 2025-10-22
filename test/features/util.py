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

import argparse
import socket

from local_thrift import thrift  # noqa
from thrift.transport.TSocket import TSocket
from thrift.transport.TTransport import TBufferedTransport, TFramedTransport
from thrift.transport.THttpClient import THttpClient
from thrift.protocol.TBinaryProtocol import TBinaryProtocol
from thrift.protocol.TCompactProtocol import TCompactProtocol
from thrift.protocol.TJSONProtocol import TJSONProtocol


def add_common_args(p):
    p.add_argument('--host', default='localhost')
    p.add_argument('--port', type=int, default=9090)
    p.add_argument('--protocol', default='binary')
    p.add_argument('--transport', default='buffered')
    p.add_argument('--ssl', action='store_true')


def parse_common_args(argv):
    p = argparse.ArgumentParser()
    add_common_args(p)
    return p.parse_args(argv)


def init_protocol(args):
    sock = TSocket(args.host, args.port, socket_family=socket.AF_INET)
    sock.setTimeout(500)
    trans = {
        'buffered': TBufferedTransport,
        'framed': TFramedTransport,
        'http': THttpClient,
    }[args.transport](sock)
    trans.open()
    return {
        'binary': TBinaryProtocol,
        'compact': TCompactProtocol,
        'json': TJSONProtocol,
    }[args.protocol](trans)
