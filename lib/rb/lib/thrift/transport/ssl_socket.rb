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

require 'io/wait'
require 'ipaddr'

module Thrift
  class SSLSocket < Socket
    def initialize(host = 'localhost', port = 9090, timeout = nil, ssl_context = nil, server_hostname: host, verify_peer: true)
      super(host, port, timeout)
      @ssl_context = ssl_context
      @server_hostname = server_hostname
      @verify_peer = verify_peer
    end

    attr_accessor :ssl_context
    attr_accessor :server_hostname
    attr_reader :verify_peer

    def open
      deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + @timeout unless @timeout.nil? || @timeout == 0
      socket = connect_socket(deadline)

      begin
        ssl_socket = OpenSSL::SSL::SSLSocket.new(socket, configured_ssl_context)
        ssl_socket.hostname = @server_hostname if send_server_hostname?
        ssl_socket.sync_close = true
        @handle = ssl_socket

        if deadline
          loop do
            result = @handle.connect_nonblock(exception: false)

            case result
            when @handle
              break
            when :wait_readable
              remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
              if remaining <= 0 || !@handle.to_io.wait_readable(remaining)
                raise TransportException.new(TransportException::TIMED_OUT, "SSL socket: Timed out establishing session with #{@desc}")
              end
            when :wait_writable
              remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
              if remaining <= 0 || !@handle.to_io.wait_writable(remaining)
                raise TransportException.new(TransportException::TIMED_OUT, "SSL socket: Timed out establishing session with #{@desc}")
              end
            else
              raise TransportException.new(TransportException::NOT_OPEN, "Could not connect to #{@desc}: unexpected SSL connect result #{result.inspect}")
            end
          end
        else
          @handle.connect
        end

        @handle.post_connection_check(server_hostname_for_verification) if @verify_peer
        @handle
      rescue TransportException
        close_socket(@handle)
        @handle = nil
        raise
      rescue StandardError
        close_socket(@handle || socket)
        @handle = nil
        raise TransportException.new(TransportException::NOT_OPEN, "Could not connect to #{@desc}")
      end
    end

    def to_s
      "ssl(#{super.to_s})"
    end

    private

    def configured_ssl_context
      @ssl_context ||= OpenSSL::SSL::SSLContext.new

      if @verify_peer
        verify_mode = @ssl_context.verify_mode || OpenSSL::SSL::VERIFY_NONE
        @ssl_context.verify_mode = OpenSSL::SSL::VERIFY_PEER if verify_mode == OpenSSL::SSL::VERIFY_NONE
        configure_default_cert_store
      elsif @ssl_context.verify_mode != OpenSSL::SSL::VERIFY_NONE
        @ssl_context.verify_mode = OpenSSL::SSL::VERIFY_NONE
      end

      @ssl_context
    end

    def configure_default_cert_store
      return if @ssl_context.ca_file || @ssl_context.ca_path || @ssl_context.cert_store

      @ssl_context.cert_store = OpenSSL::X509::Store.new.tap(&:set_default_paths)
    end

    def send_server_hostname?
      return false if @server_hostname.nil? || @server_hostname.empty?

      IPAddr.new(@server_hostname)
      false
    rescue IPAddr::InvalidAddressError
      true
    end

    def server_hostname_for_verification
      @server_hostname.nil? || @server_hostname.empty? ? @host : @server_hostname
    end
  end
end
