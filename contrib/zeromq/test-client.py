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
import time
import zmq
import TZmqClient
import thrift.protocol.TBinaryProtocol
import storage.ttypes
import storage.Storage


def main(args):
    endpoint = "tcp://127.0.0.1:9090"
    socktype = zmq.REQ
    incr = 0
    if len(args) > 1:
        incr = int(args[1])
        if incr:
            socktype = zmq.PUSH
            endpoint = "tcp://127.0.0.1:9091"

    ctx = zmq.Context()
    transport = TZmqClient.TZmqClient(ctx, endpoint, socktype)
    protocol = thrift.protocol.TBinaryProtocol.TBinaryProtocolAccelerated(transport)
    client = storage.Storage.Client(protocol)
    transport.open()

    if incr:
        client.incr(incr)
        time.sleep(0.05)
    else:
        value = client.get()
        print(value)


if __name__ == "__main__":
    main(sys.argv)
