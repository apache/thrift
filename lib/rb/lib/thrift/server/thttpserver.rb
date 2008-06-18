#!/usr/bin/env ruby

require 'thrift/protocol/tprotocol'
require 'thrift/protocol/tbinaryprotocol'
require 'thrift/transport/ttransport'

require 'mongrel'

## Sticks a service on a URL, using mongrel to do the HTTP work
class TSimpleMongrelHTTPServer
  class Handler < Mongrel::HttpHandler
    def initialize processor, protocol_factory
      @processor = processor
      @protocol_factory = protocol_factory
    end

    def process request, response
      unless request.params["REQUEST_METHOD"] == "POST"
        response.start(404) { } # better way?
        return
      end
      response.start(200) do |head, out|
        head["Content-Type"] = "application/x-thrift"
        transport = TIOStreamTransport.new request.body, out
        protocol = @protocol_factory.getProtocol transport
        @processor.process protocol, protocol
      end
    end
  end

  def initialize processor, opts={}
    port = opts[:port] || 80
    ip = opts[:ip] || "0.0.0.0"
    path = opts[:path] || ""
    protocol_factory = opts[:protocol_factory] || TBinaryProtocolFactory.new
    @server = Mongrel::HttpServer.new ip, port
    @server.register "/#{path}", Handler.new(processor, protocol_factory)
  end

  def serve
    @server.run.join
  end
end
