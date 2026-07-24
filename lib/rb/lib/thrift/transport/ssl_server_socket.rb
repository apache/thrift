# encoding: ascii-8bit
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

require 'io/wait'
require 'socket'

module Thrift
  class SSLServerSocket < ServerSocket
    def initialize(host_or_port, port = nil, ssl_context = nil, client_timeout: DEFAULT_CLIENT_TIMEOUT)
      super(host_or_port, port, client_timeout: client_timeout)
      @ssl_context = ssl_context
    end

    attr_accessor :ssl_context

    def listen
      tcp_server = TCPServer.new(@host, @port)
      @handle = OpenSSL::SSL::SSLServer.new(tcp_server, @ssl_context)
      @handle.start_immediately = false
      @handle
    end

    def accept
      return if @handle.nil?

      ssl_socket = @handle.accept
      deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + @client_timeout unless @client_timeout.nil? || @client_timeout == 0
      ssl_socket.to_io.setsockopt(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      accept_ssl_socket(ssl_socket, deadline)

      Socket.new.tap do |transport|
        transport.timeout = @client_timeout
        transport.handle = ssl_socket
      end
    rescue
      close_accepted_socket(ssl_socket)
      raise
    end

    def to_s
      "ssl(#{super.to_s})"
    end

    private

    def accept_ssl_socket(ssl_socket, deadline)
      return ssl_socket.accept unless deadline

      loop do
        case ssl_socket.accept_nonblock(exception: false)
        when ssl_socket
          return ssl_socket
        when :wait_readable
          wait_for_handshake(ssl_socket, :read, deadline)
        when :wait_writable
          wait_for_handshake(ssl_socket, :write, deadline)
        else
          raise OpenSSL::SSL::SSLError, "SSL server socket: Unexpected handshake result"
        end
      end
    end

    def wait_for_handshake(ssl_socket, direction, deadline)
      remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
      raise_handshake_timeout if remaining <= 0

      ready = if direction == :read
        ssl_socket.to_io.wait_readable(remaining)
      else
        ssl_socket.to_io.wait_writable(remaining)
      end
      raise_handshake_timeout unless ready
    end

    def raise_handshake_timeout
      raise OpenSSL::SSL::SSLError, "SSL server socket: Timed out accepting TLS connection"
    end

    def close_accepted_socket(ssl_socket)
      ssl_socket&.close
    rescue StandardError
      nil
    end
  end
end
