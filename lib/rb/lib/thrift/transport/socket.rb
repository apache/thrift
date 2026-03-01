# encoding: ascii-8bit
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

require 'socket'

module Thrift
  class Socket < BaseTransport
    def initialize(host='localhost', port=9090, timeout=nil)
      @host = host
      @port = port
      @timeout = timeout
      @desc = "#{host}:#{port}"
      @handle = nil
    end

    attr_accessor :handle, :timeout

    def open
      for addrinfo in ::Socket::getaddrinfo(@host, @port, nil, ::Socket::SOCK_STREAM) do
        begin
          socket = ::Socket.new(addrinfo[4], ::Socket::SOCK_STREAM, 0)
          socket.setsockopt(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
          sockaddr = ::Socket.sockaddr_in(addrinfo[1], addrinfo[3])
          begin
            socket.connect_nonblock(sockaddr)
          rescue Errno::EINPROGRESS
            unless IO.select(nil, [ socket ], nil, @timeout)
              next
            end
            begin
              socket.connect_nonblock(sockaddr)
            rescue Errno::EISCONN
            end
          end
          return @handle = socket
        rescue StandardError => e
          next
        end
      end
      raise TransportException.new(TransportException::NOT_OPEN, "Could not connect to #{@desc}: #{e}")
    end

    def open?
      !@handle.nil? and !@handle.closed?
    end

    def write(str)
      raise TransportException.new(TransportException::NOT_OPEN, "closed stream") unless open?
      str = Bytes.force_binary_encoding(str)
      begin
        if @timeout.nil? or @timeout == 0
          @handle.write(str)
        else
          deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + @timeout
          len = 0

          while len < str.length
            begin
              len += @handle.write_nonblock(str[len..-1])
            rescue IO::WaitWritable
              wait_for(:write, deadline, str.length)
            rescue IO::WaitReadable
              wait_for(:read, deadline, str.length)
            end
          end

          len
        end
      rescue TransportException => e
        # pass this on
        raise e
      rescue StandardError => e
        @handle.close
        @handle = nil
        raise TransportException.new(TransportException::NOT_OPEN, e.message)
      end
    end

    def read(sz)
      raise TransportException.new(TransportException::NOT_OPEN, "closed stream") unless open?

      begin
        if @timeout.nil? or @timeout == 0
          data = @handle.readpartial(sz)
        else
          deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + @timeout

          data = loop do
            begin
              break @handle.read_nonblock(sz)
            rescue IO::WaitReadable
              wait_for(:read, deadline, sz)
            rescue IO::WaitWritable
              wait_for(:write, deadline, sz)
            end
          end
        end
      rescue TransportException => e
        # don't let this get caught by the StandardError handler
        raise e
      rescue StandardError => e
        @handle.close unless @handle.closed?
        @handle = nil
        raise TransportException.new(TransportException::NOT_OPEN, e.message)
      end
      if (data.nil? or data.length == 0)
        raise TransportException.new(TransportException::UNKNOWN, "Socket: Could not read #{sz} bytes from #{@desc}")
      end
      data
    end

    def close
      @handle.close unless @handle.nil? or @handle.closed?
      @handle = nil
    end

    def to_io
      @handle&.to_io || raise(IOError, 'closed stream')
    end

    def to_s
      "socket(#{@host}:#{@port})"
    end

    private

    def wait_for(operation, deadline, sz)
      rd_ary, wr_ary = case operation
      when :read
        [[@handle], nil]
      when :write
        [nil, [@handle]]
      else
        raise ArgumentError, "Unknown IO wait operation: #{operation.inspect}"
      end

      loop do
        remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
        if remaining <= 0
          case operation
          when :read
            raise TransportException.new(TransportException::TIMED_OUT, "Socket: Timed out reading #{sz} bytes from #{@desc}")
          when :write
            raise TransportException.new(TransportException::TIMED_OUT, "Socket: Timed out writing #{sz} bytes to #{@desc}")
          end
        end

        rd, wr, = IO.select(rd_ary, wr_ary, nil, remaining)
        return if (rd && !rd.empty?) || (wr && !wr.empty?)
      end
    end
  end
end
