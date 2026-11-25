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

require "logger"

module Thrift
  class BaseServer
    # accept errors that end only the connection being accepted; the listening
    # socket is fine. Ruby retries EINTR and EAGAIN itself. Linux also reports
    # pending network errors of the new connection here; see accept(2).
    ACCEPT_CONNECTION_ERRORS = [
      *%w[ECONNRESET EPIPE ECONNABORTED EPROTO],
      *(RUBY_PLATFORM.include?("linux") ? %w[EPERM ENETDOWN ENETUNREACH EHOSTUNREACH EHOSTDOWN ENONET ENOPROTOOPT EOPNOTSUPP] : []),
    ].select { |name| Errno.const_defined?(name) }.map { |name| Errno.const_get(name) }.freeze

    # accept errors when descriptors or memory run out, which pass once other
    # connections close.
    ACCEPT_RESOURCE_ERRORS = %w[EMFILE ENFILE ENOBUFS ENOMEM]
      .select { |name| Errno.const_defined?(name) }.map { |name| Errno.const_get(name) }.freeze

    private_constant :ACCEPT_CONNECTION_ERRORS, :ACCEPT_RESOURCE_ERRORS

    def initialize(processor, server_transport, transport_factory = nil, protocol_factory = nil, logger: nil)
      @processor = processor
      @server_transport = server_transport
      @transport_factory = transport_factory ? transport_factory : Thrift::BaseTransportFactory.new
      @protocol_factory = protocol_factory ? protocol_factory : Thrift::BinaryProtocolFactory.new
      if logger.nil?
        @logger = Logger.new(STDERR)
        @logger.level = Logger::WARN
      else
        @logger = logger
      end
    end

    def serve
      raise NotImplementedError
    end

    def to_s
      "server(#{@protocol_factory}(#{@transport_factory}(#{@server_transport})))"
    end

    private

    # Yields each client accepted from the server transport, for as long as
    # wait_for_client returns true. Skips a connection that fails while it is
    # accepted. While descriptors or memory run out, accepts again after a
    # pause that starts at 5 ms and doubles up to 1 s, and logs the first
    # failure of each such run. Raises IOError once the transport is closed.
    def accept_clients
      pause = 0
      while wait_for_client
        begin
          client = @server_transport.accept
        rescue *ACCEPT_CONNECTION_ERRORS
          pause = 0
          next
        rescue *ACCEPT_RESOURCE_ERRORS => e
          @logger.error("Error while accepting a client connection, retrying: #{e.inspect}") if pause.zero?
          pause = pause.zero? ? 0.005 : [pause * 2, 1.0].min
          sleep(pause)
          next
        rescue => e
          raise unless defined?(OpenSSL::SSL::SSLError) && e.is_a?(OpenSSL::SSL::SSLError)

          pause = 0
          next
        end
        # ServerSocket#accept returns nil once the socket is closed, for
        # example by a shutdown during a pause.
        raise IOError, "closed stream" if client.nil?

        pause = 0
        yield client
      end
    end

    # Called before each accept; return false to stop accepting. The servers
    # that block in accept have nothing to wait for.
    def wait_for_client
      true
    end
  end
end
