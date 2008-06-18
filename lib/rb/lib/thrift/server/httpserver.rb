require 'thrift/protocol'
require 'thrift/protocol/binaryprotocol'
require 'thrift/transport'

require 'mongrel'

## Sticks a service on a URL, using mongrel to do the HTTP work
module Thrift
  class SimpleMongrelHTTPServer
    class Handler < Mongrel::HttpHandler
      def initialize(processor, protocol_factory)
        @processor = processor
        @protocol_factory = protocol_factory
      end

      def process(request, response)
        if request.params["REQUEST_METHOD"] == "POST"
          response.start(200) do |head, out|
            head["Content-Type"] = "application/x-thrift"
            transport = IOStreamTransport.new request.body, out
            protocol = @protocol_factory.get_protocol transport
            @processor.process protocol, protocol
          end
        else
          response.start(404) { }
        end
      end
    end

    def initialize(processor, opts={})
      port = opts[:port] || 80
      ip = opts[:ip] || "0.0.0.0"
      path = opts[:path] || ""
      protocol_factory = opts[:protocol_factory] || BinaryProtocolFactory.new
      @server = Mongrel::HttpServer.new ip, port
      @server.register "/#{path}", Handler.new(processor, protocol_factory)
    end

    def serve
      @server.run.join
    end
  end
  deprecate_class! :TSimpleMongrelHTTPServer => SimpleMongrelHTTPServer
end
