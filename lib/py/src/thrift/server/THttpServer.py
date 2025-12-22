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

import http.server as BaseHTTPServer
import ssl
from typing import Any, BinaryIO, Callable, TYPE_CHECKING, cast

from thrift.Thrift import TMessageType
from thrift.server import TServer
from thrift.transport import TTransport

if TYPE_CHECKING:
    from thrift.protocol.TProtocol import TProtocolFactory
    from thrift.Thrift import TProcessor


class ResponseException(Exception):
    """Allows handlers to override the HTTP response

    Normally, THttpServer always sends a 200 response.  If a handler wants
    to override this behavior (e.g., to simulate a misconfigured or
    overloaded web server during testing), it can raise a ResponseException.
    The function passed to the constructor will be called with the
    RequestHandler as its only argument.  Note that this is irrelevant
    for ONEWAY requests, as the HTTP response must be sent before the
    RPC is processed.
    """

    handler: Callable[[Any], None]

    def __init__(self, handler: Callable[[Any], None]) -> None:
        self.handler = handler


class THttpServer(TServer.TServer):
    """A simple HTTP-based Thrift server

    This class is not very performant, but it is useful (for example) for
    acting as a mock version of an Apache-based PHP Thrift endpoint.
    Also important to note the HTTP implementation pretty much violates the
    transport/protocol/processor/server layering, by performing the transport
    functions here.  This means things like oneway handling are oddly exposed.
    """

    httpd: BaseHTTPServer.HTTPServer
    _replied: bool | None

    def __init__(
        self,
        processor: TProcessor,
        server_address: tuple[str, int],
        inputProtocolFactory: TProtocolFactory,
        outputProtocolFactory: TProtocolFactory | None = None,
        server_class: type[BaseHTTPServer.HTTPServer] = BaseHTTPServer.HTTPServer,
        **kwargs: Any,
    ) -> None:
        """Set up protocol factories and HTTP (or HTTPS) server.

        See BaseHTTPServer for server_address.
        See TServer for protocol factories.

        To make a secure server, provide the named arguments:
        * cafile    - to validate clients [optional]
        * cert_file - the server cert
        * key_file  - the server's key
        """
        if outputProtocolFactory is None:
            outputProtocolFactory = inputProtocolFactory

        TServer.TServer.__init__(self, processor, None, None, None,
                                 inputProtocolFactory, outputProtocolFactory)

        thttpserver = self
        self._replied = None

        class RequestHander(BaseHTTPServer.BaseHTTPRequestHandler):
            def do_POST(self):
                # Don't care about the request path.
                thttpserver._replied = False
                iftrans = TTransport.TFileObjectTransport(cast(BinaryIO, self.rfile))
                itrans = TTransport.TBufferedTransport(
                    iftrans, int(self.headers['Content-Length']))
                otrans = TTransport.TMemoryBuffer()
                iprot = thttpserver.inputProtocolFactory.getProtocol(itrans)
                oprot = thttpserver.outputProtocolFactory.getProtocol(otrans)
                try:
                    thttpserver.processor.on_message_begin(self.on_begin)
                    thttpserver.processor.process(iprot, oprot)
                except ResponseException as exn:
                    exn.handler(self)
                else:
                    if not thttpserver._replied:
                        # If the request was ONEWAY we would have replied already
                        data = otrans.getvalue()
                        self.send_response(200)
                        self.send_header("Content-Length", str(len(data)))
                        self.send_header("Content-Type", "application/x-thrift")
                        self.end_headers()
                        self.wfile.write(data)

            def on_begin(self, name, type, seqid):
                """
                Inspect the message header.

                This allows us to post an immediate transport response
                if the request is a ONEWAY message type.
                """
                if type == TMessageType.ONEWAY:
                    self.send_response(200)
                    self.send_header("Content-Type", "application/x-thrift")
                    self.end_headers()
                    thttpserver._replied = True

        self.httpd = server_class(server_address, RequestHander)

        cert_file = kwargs.get('cert_file')
        key_file = kwargs.get('key_file')
        cafile = kwargs.get('cafile')
        if cafile or cert_file or key_file:
            context = ssl.create_default_context(cafile=cafile)
            context.check_hostname = False
            if cert_file:
                context.load_cert_chain(cert_file, key_file)
            context.verify_mode = ssl.CERT_REQUIRED if cafile else ssl.CERT_NONE
            self.httpd.socket = context.wrap_socket(self.httpd.socket, server_side=True)

    def serve(self) -> None:
        self.httpd.serve_forever()

    def shutdown(self) -> None:
        self.httpd.socket.close()
        # self.httpd.shutdown() # hangs forever, python doesn't handle POLLNVAL properly!
