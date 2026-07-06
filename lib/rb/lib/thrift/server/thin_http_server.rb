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
require 'thin'
require 'thrift/server/rack_application'

##
# Wraps the Thin web server to provide a Thrift server over HTTP.
# <b>DEPRECATED:</b> Use <tt>Thrift::RackApplication</tt> with a maintained Rack
# server instead.
module Thrift
  class ThinHTTPServer < BaseServer

    ##
    # Accepts a Thrift::Processor
    # Options include:
    # * :port
    # * :ip
    # * :path
    # * :protocol_factory
    # * :ssl
    # * :ssl_options
    def initialize(processor, options = {})
      Kernel.warn "[DEPRECATION WARNING] `Thrift::ThinHTTPServer` is deprecated because Thin is no longer maintained. Please use `Thrift::RackApplication` with a maintained Rack server instead."
      port = options[:port] || 80
      ip = options[:ip] || "0.0.0.0"
      path = options[:path] || "/"
      protocol_factory = options[:protocol_factory] || BinaryProtocolFactory.new
      endpoint = RackApplication.mapped(path, processor, protocol_factory)
      app = Rack::Builder.new do
        use Rack::CommonLogger
        use Rack::ShowExceptions
        use Rack::Lint
        run endpoint
      end
      @server = Thin::Server.new(ip, port, app)
      if options[:ssl]
        @server.ssl = true
        @server.ssl_options = options[:ssl_options] || {}
      end
    end

    ##
    # Starts the server
    def serve
      @server.start
    end

    RackApplication = Thrift::RackApplication
  end
end
