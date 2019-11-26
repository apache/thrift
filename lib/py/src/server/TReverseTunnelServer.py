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

from six.moves import queue
from thrift.transport import TTransport


class TReverseTunnelServer(TTransport.TTransportBase):
    """A transport that allows RPC Servers to use Client Transports.

    This transport is useful for reverse connections, where the server/client
    transport architecture is the reverse of server/client architecture for
    the RPC service. One example where this transport is useful
    is when there is RPC service behind a restrited network that does not allow
    incoming connections.

    The current implementation of this transport is compatible with
    ThreadedPoolServer and other concurrent transport servers.
    As transport servers expect that the accept method is a blocking one,
    we use a queue for job control.
    """

    def __init__(self, transport, acceptable_clients=10):
        self._transport = transport
        self._connections = queue.Queue(acceptable_clients)

    def listen(self):
        pass

    def accept(self):
        self._transport.open()
        self._connections.put(None)
        return self._transport

    def close(self):
        with self._connections.mutex:
            self._connections.queue.clear()

    def flush(self):
        # We assume that once we flush we are done with a message
        self._connections.get()
        self._transport.flush()


class TReverseTunnelServerFactory(object):
    def __init__(self, transport, acceptable_clients=10):
        self._trans = transport
        self._acceptable_clients = acceptable_clients

    def getTransport(self, trans):
        return TReverseTunnelServer(self.trans, self._acceptable_clients)
