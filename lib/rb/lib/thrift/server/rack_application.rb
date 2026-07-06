# frozen_string_literal: true
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
require 'thrift/transport/io_stream_transport'

module Thrift
  class RackApplication
    THRIFT_HEADER = "application/x-thrift"

    def initialize(processor, protocol_factory = BinaryProtocolFactory.new)
      @processor = processor
      @protocol_factory = protocol_factory
    end

    def call(env)
      request = Rack::Request.new(env)

      if self.class.valid_thrift_request?(request)
        self.class.successful_request(request, @processor, @protocol_factory).finish
      else
        self.class.failed_request.finish
      end
    end

    def self.mapped(path, processor, protocol_factory = BinaryProtocolFactory.new)
      Rack::Builder.new do
        use Rack::ContentLength
        map path do
          run RackApplication.new(processor, protocol_factory)
        end
      end
    end

    def self.successful_request(rack_request, processor, protocol_factory)
      response = Rack::Response.new([], 200, {Rack::CONTENT_TYPE => THRIFT_HEADER})
      transport = IOStreamTransport.new(rack_request.body, response)
      protocol = protocol_factory.get_protocol(transport)
      processor.process protocol, protocol

      response
    end

    def self.failed_request
      Rack::Response.new(['Not Found'], 404, {Rack::CONTENT_TYPE => THRIFT_HEADER})
    end

    def self.valid_thrift_request?(rack_request)
      rack_request.post? && rack_request.media_type == THRIFT_HEADER
    end
  end
end
