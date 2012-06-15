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

require 'rack'
require 'thin'

## Sticks a service on a URL, using Thin to do the HTTP work
module Thrift
  class ThinHTTPServer < BaseServer

    THRIFT_HEADER = "application/x-thrift"

    def initialize(processor, opts={})
      port = opts[:port] || 80
      ip = opts[:ip] || "0.0.0.0"
      path = opts[:path] || "/"
      protocol_factory = opts[:protocol_factory] || BinaryProtocolFactory.new
      application_context = create_application_context(path, processor, protocol_factory)
      @server = Thin::Server.new(ip, port, application_context)
    end

    def serve
      @server.start
    end

    def self.successful_request(processor, protocol_factory)
      response = Rack::Response.new([], 200)
      transport = IOStreamTransport.new request.body, response
      protocol = protocol_factory.get_protocol transport
      processor.process protocol, protocol
      response
    end

    def self.failed_request
      Rack::Response.new(['Not Found'], 404)
    end

    def self.valid_thrift_request?(env)
      request = Rack::Request.new(env)
      request.post? && env["CONTENT_TYPE"] == THRIFT_HEADER
    end

    private

    def create_application_context(path, processor, protocol_factory)
      Rack::Builder.new do
        use Rack::CommonLogger
        use Rack::ShowExceptions
        use Rack::ContentType, THRIFT_HEADER
        use Rack::Lint
        map path do
          run lambda { |env|
            if ThinHTTPServer.valid_thrift_request?(env)
              ThinHTTPServer.successful_request(processor, protocol_factory)
            else
              ThinHTTPServer.failed_request
            end
          }
        end
      end
    end
  end
end
