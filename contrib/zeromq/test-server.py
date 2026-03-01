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

import zmq
import TZmqServer
import storage.ttypes
import storage.Storage


class StorageHandler(storage.Storage.Iface):
    def __init__(self):
        self.value = 0

    def incr(self, amount):
        self.value += amount

    def get(self):
        return self.value


def main():
    handler = StorageHandler()
    processor = storage.Storage.Processor(handler)

    ctx = zmq.Context()
    reqrep_server = TZmqServer.TZmqServer(processor, ctx, "tcp://0.0.0.0:9090", zmq.REP)
    oneway_server = TZmqServer.TZmqServer(processor, ctx, "tcp://0.0.0.0:9091", zmq.PULL)
    multiserver = TZmqServer.TZmqMultiServer()
    multiserver.servers.append(reqrep_server)
    multiserver.servers.append(oneway_server)
    multiserver.serveForever()


if __name__ == "__main__":
    main()
