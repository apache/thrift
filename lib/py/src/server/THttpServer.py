# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

import BaseHTTPServer

from thrift.server import TServer
from thrift.transport import TTransport

class THttpServer(TServer.TServer):
  """A simple HTTP-based Thrift server

  This class is not very performant, but it is useful (for example) for
  acting as a mock version of an Apache-based PHP Thrift endpoint."""

  def __init__(self, processor, server_address,
      inputProtocolFactory, outputProtocolFactory = None):
    """Set up protocol factories and HTTP server.

    See BaseHTTPServer for server_address.
    See TServer for protocol factories."""

    if outputProtocolFactory is None:
      outputProtocolFactory = inputProtocolFactory

    TServer.TServer.__init__(self, processor, None, None, None,
        inputProtocolFactory, outputProtocolFactory)

    thttpserver = self

    class RequestHander(BaseHTTPServer.BaseHTTPRequestHandler):
      def do_POST(self):
        # Don't care about the request path.
        self.send_response(200)
        self.send_header("content-type", "application/x-thrift")
        self.end_headers()

        itrans = TTransport.TFileObjectTransport(self.rfile)
        otrans = TTransport.TFileObjectTransport(self.wfile)
        iprot = thttpserver.inputProtocolFactory.getProtocol(itrans)
        oprot = thttpserver.outputProtocolFactory.getProtocol(otrans)
        thttpserver.processor.process(iprot, oprot)
        otrans.flush()

    self.httpd = BaseHTTPServer.HTTPServer(server_address, RequestHander)

  def serve(self):
    self.httpd.serve_forever()
