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

from __future__ import annotations

from io import BytesIO
import struct
from typing import Any, TYPE_CHECKING

from zope.interface import implementer, Interface, Attribute  # type: ignore[import-untyped]
from twisted.internet.protocol import ServerFactory, ClientFactory, connectionDone  # type: ignore[import-untyped]
from twisted.internet import defer  # type: ignore[import-untyped]
from twisted.internet.threads import deferToThread  # type: ignore[import-untyped]
from twisted.protocols import basic  # type: ignore[import-untyped]
from twisted.web import server, resource, http  # type: ignore[import-untyped]

from thrift.transport import TTransport

if TYPE_CHECKING:
    from thrift.protocol.TProtocol import TProtocolFactory


class TMessageSenderTransport(TTransport.TTransportBase):
    """Base transport class for message-based sending."""

    __wbuf: BytesIO

    def __init__(self) -> None:
        self.__wbuf = BytesIO()

    def write(self, buf: bytes) -> None:
        self.__wbuf.write(buf)

    def flush(self) -> Any:
        msg = self.__wbuf.getvalue()
        self.__wbuf = BytesIO()
        return self.sendMessage(msg)

    def sendMessage(self, message: bytes) -> Any:
        raise NotImplementedError


class TCallbackTransport(TMessageSenderTransport):
    """Transport that invokes a callback function for message sending."""

    func: Any  # Callable[[bytes], Any]

    def __init__(self, func: Any) -> None:
        TMessageSenderTransport.__init__(self)
        self.func = func

    def sendMessage(self, message: bytes) -> Any:
        return self.func(message)


class ThriftClientProtocol(basic.Int32StringReceiver):  # type: ignore[misc]
    """Twisted protocol for Thrift clients."""

    MAX_LENGTH: int = 2 ** 31 - 1

    _client_class: type[Any]
    _iprot_factory: TProtocolFactory
    _oprot_factory: TProtocolFactory
    recv_map: dict[str, Any]
    started: defer.Deferred[Any]
    client: Any

    def __init__(
        self,
        client_class: type[Any],
        iprot_factory: TProtocolFactory,
        oprot_factory: TProtocolFactory | None = None,
    ) -> None:
        self._client_class = client_class
        self._iprot_factory = iprot_factory
        if oprot_factory is None:
            self._oprot_factory = iprot_factory
        else:
            self._oprot_factory = oprot_factory

        self.recv_map = {}
        self.started = defer.Deferred()

    def dispatch(self, msg: bytes) -> None:
        self.sendString(msg)

    def connectionMade(self) -> None:
        tmo = TCallbackTransport(self.dispatch)
        self.client = self._client_class(tmo, self._oprot_factory)
        self.started.callback(self.client)

    def connectionLost(self, reason: Any = connectionDone) -> None:
        # the called errbacks can add items to our client's _reqs,
        # so we need to use a tmp, and iterate until no more requests
        # are added during errbacks
        if self.client:
            tex = TTransport.TTransportException(
                type=TTransport.TTransportException.END_OF_FILE,
                message='Connection closed (%s)' % reason)
            while self.client._reqs:
                _, v = self.client._reqs.popitem()
                v.errback(tex)
            del self.client._reqs
            self.client = None

    def stringReceived(self, frame: bytes) -> None:
        tr = TTransport.TMemoryBuffer(frame)
        iprot = self._iprot_factory.getProtocol(tr)
        (fname, mtype, rseqid) = iprot.readMessageBegin()

        try:
            method = self.recv_map[fname]
        except KeyError:
            method = getattr(self.client, 'recv_' + fname)
            self.recv_map[fname] = method

        method(iprot, mtype, rseqid)


class ThriftSASLClientProtocol(ThriftClientProtocol):
    """Twisted protocol for SASL-authenticated Thrift clients."""

    START: int = 1
    OK: int = 2
    BAD: int = 3
    ERROR: int = 4
    COMPLETE: int = 5

    MAX_LENGTH: int = 2 ** 31 - 1

    SASLClient: type[Any]
    sasl: Any
    _sasl_negotiation_deferred: defer.Deferred[Any] | None
    _sasl_negotiation_status: int | None

    def __init__(
        self,
        client_class: type[Any],
        iprot_factory: TProtocolFactory,
        oprot_factory: TProtocolFactory | None = None,
        host: str | None = None,
        service: str | None = None,
        mechanism: str = 'GSSAPI',
        **sasl_kwargs: Any,
    ) -> None:
        """
        host: the name of the server, from a SASL perspective
        service: the name of the server's service, from a SASL perspective
        mechanism: the name of the preferred mechanism to use

        All other kwargs will be passed to the puresasl.client.SASLClient
        constructor.
        """

        from puresasl.client import SASLClient  # type: ignore[import-untyped]
        self.SASLCLient = SASLClient

        ThriftClientProtocol.__init__(self, client_class, iprot_factory, oprot_factory)

        self._sasl_negotiation_deferred = None
        self._sasl_negotiation_status = None
        self.client = None

        if host is not None:
            self.createSASLClient(host, service, mechanism, **sasl_kwargs)

    def createSASLClient(self, host: str, service: str | None, mechanism: str, **kwargs: Any) -> None:
        self.sasl = self.SASLClient(host, service, mechanism, **kwargs)

    def dispatch(self, msg: bytes) -> None:
        encoded = self.sasl.wrap(msg)
        len_and_encoded = b''.join((struct.pack('!i', len(encoded)), encoded))
        ThriftClientProtocol.dispatch(self, len_and_encoded)

    @defer.inlineCallbacks  # type: ignore[misc]
    def connectionMade(self) -> Any:
        self._sendSASLMessage(self.START, self.sasl.mechanism)
        initial_message = yield deferToThread(self.sasl.process)
        self._sendSASLMessage(self.OK, initial_message)

        while True:
            status, challenge = yield self._receiveSASLMessage()
            if status == self.OK:
                response = yield deferToThread(self.sasl.process, challenge)
                self._sendSASLMessage(self.OK, response)
            elif status == self.COMPLETE:
                if not self.sasl.complete:
                    msg = "The server erroneously indicated that SASL " \
                          "negotiation was complete"
                    raise TTransport.TTransportException(message=msg)
                else:
                    break
            else:
                msg = "Bad SASL negotiation status: %d (%s)" % (status, challenge)
                raise TTransport.TTransportException(message=msg)

        self._sasl_negotiation_deferred = None
        ThriftClientProtocol.connectionMade(self)

    def _sendSASLMessage(self, status: int, body: bytes | str | None) -> None:
        if body is None:
            body = b""
        elif isinstance(body, str):
            body = body.encode('utf-8')
        header = struct.pack(">BI", status, len(body))
        self.transport.write(header + body)

    def _receiveSASLMessage(self) -> defer.Deferred[tuple[int | None, bytes]]:
        self._sasl_negotiation_deferred = defer.Deferred()
        self._sasl_negotiation_status = None
        return self._sasl_negotiation_deferred

    def connectionLost(self, reason: Any = connectionDone) -> None:
        if self.client:
            ThriftClientProtocol.connectionLost(self, reason)

    def dataReceived(self, data: bytes) -> None:
        if self._sasl_negotiation_deferred:
            # we got a sasl challenge in the format (status, length, challenge)
            # save the status, let IntNStringReceiver piece the challenge data together
            self._sasl_negotiation_status, = struct.unpack("B", data[0:1])
            ThriftClientProtocol.dataReceived(self, data[1:])
        else:
            # normal frame, let IntNStringReceiver piece it together
            ThriftClientProtocol.dataReceived(self, data)

    def stringReceived(self, frame: bytes) -> None:
        if self._sasl_negotiation_deferred:
            # the frame is just a SASL challenge
            response = (self._sasl_negotiation_status, frame)
            self._sasl_negotiation_deferred.callback(response)
        else:
            # there's a second 4 byte length prefix inside the frame
            decoded_frame = self.sasl.unwrap(frame[4:])
            ThriftClientProtocol.stringReceived(self, decoded_frame)


class ThriftServerProtocol(basic.Int32StringReceiver):  # type: ignore[misc]
    """Twisted protocol for Thrift servers."""

    MAX_LENGTH: int = 2 ** 31 - 1
    factory: ThriftServerFactory

    def dispatch(self, msg: bytes) -> None:
        self.sendString(msg)

    def processError(self, error: Any) -> None:
        self.transport.loseConnection()

    def processOk(self, _: Any, tmo: TTransport.TMemoryBuffer) -> None:
        msg = tmo.getvalue()

        if len(msg) > 0:
            self.dispatch(msg)

    def stringReceived(self, frame: bytes) -> None:
        tmi = TTransport.TMemoryBuffer(frame)
        tmo = TTransport.TMemoryBuffer()

        iprot = self.factory.iprot_factory.getProtocol(tmi)
        oprot = self.factory.oprot_factory.getProtocol(tmo)

        d = self.factory.processor.process(iprot, oprot)
        d.addCallbacks(self.processOk, self.processError,
                       callbackArgs=(tmo,))


class IThriftServerFactory(Interface):  # type: ignore[misc]
    """Interface for Thrift server factories."""

    processor = Attribute("Thrift processor")

    iprot_factory = Attribute("Input protocol factory")

    oprot_factory = Attribute("Output protocol factory")


class IThriftClientFactory(Interface):  # type: ignore[misc]
    """Interface for Thrift client factories."""

    client_class = Attribute("Thrift client class")

    iprot_factory = Attribute("Input protocol factory")

    oprot_factory = Attribute("Output protocol factory")


@implementer(IThriftServerFactory)
class ThriftServerFactory(ServerFactory):  # type: ignore[misc]
    """Factory for creating Thrift server protocols."""

    protocol: type[ThriftServerProtocol] = ThriftServerProtocol

    processor: Any
    iprot_factory: TProtocolFactory
    oprot_factory: TProtocolFactory

    def __init__(
        self,
        processor: Any,
        iprot_factory: TProtocolFactory,
        oprot_factory: TProtocolFactory | None = None,
    ) -> None:
        self.processor = processor
        self.iprot_factory = iprot_factory
        if oprot_factory is None:
            self.oprot_factory = iprot_factory
        else:
            self.oprot_factory = oprot_factory


@implementer(IThriftClientFactory)
class ThriftClientFactory(ClientFactory):  # type: ignore[misc]
    """Factory for creating Thrift client protocols."""

    protocol: type[ThriftClientProtocol] = ThriftClientProtocol

    client_class: type[Any]
    iprot_factory: TProtocolFactory
    oprot_factory: TProtocolFactory

    def __init__(
        self,
        client_class: type[Any],
        iprot_factory: TProtocolFactory,
        oprot_factory: TProtocolFactory | None = None,
    ) -> None:
        self.client_class = client_class
        self.iprot_factory = iprot_factory
        if oprot_factory is None:
            self.oprot_factory = iprot_factory
        else:
            self.oprot_factory = oprot_factory

    def buildProtocol(self, addr: Any) -> ThriftClientProtocol:
        p = self.protocol(self.client_class, self.iprot_factory,
                          self.oprot_factory)
        p.factory = self  # type: ignore[attr-defined]
        return p


class ThriftResource(resource.Resource):  # type: ignore[misc]
    """Twisted web resource for serving Thrift over HTTP."""

    allowedMethods: tuple[str, ...] = ('POST',)

    inputProtocolFactory: TProtocolFactory
    outputProtocolFactory: TProtocolFactory
    processor: Any

    def __init__(
        self,
        processor: Any,
        inputProtocolFactory: TProtocolFactory,
        outputProtocolFactory: TProtocolFactory | None = None,
    ) -> None:
        resource.Resource.__init__(self)
        self.inputProtocolFactory = inputProtocolFactory
        if outputProtocolFactory is None:
            self.outputProtocolFactory = inputProtocolFactory
        else:
            self.outputProtocolFactory = outputProtocolFactory
        self.processor = processor

    def getChild(self, path: bytes, request: Any) -> ThriftResource:
        return self

    def _cbProcess(self, _: Any, request: Any, tmo: TTransport.TMemoryBuffer) -> None:
        msg = tmo.getvalue()
        request.setResponseCode(http.OK)
        request.setHeader("content-type", "application/x-thrift")
        request.write(msg)
        request.finish()

    def render_POST(self, request: Any) -> int:
        request.content.seek(0, 0)
        data = request.content.read()
        tmi = TTransport.TMemoryBuffer(data)
        tmo = TTransport.TMemoryBuffer()

        iprot = self.inputProtocolFactory.getProtocol(tmi)
        oprot = self.outputProtocolFactory.getProtocol(tmo)

        d = self.processor.process(iprot, oprot)
        d.addCallback(self._cbProcess, request, tmo)
        return server.NOT_DONE_YET
