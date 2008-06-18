#!/usr/bin/env ruby
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#
require 'thrift/transport'
require 'socket'

module Thrift
  class Socket < Transport
    def initialize(host='localhost', port=9090)
      @host = host
      @port = port
      @handle = nil
    end

    def set_handle(handle)
      @handle = handle
    end

    def open()
      begin
        @handle = TCPSocket.new(@host, @port)
      rescue StandardError
        raise TransportException.new(TransportException::NOT_OPEN, "Could not connect to #{@host}:#{@port}")
      end
    end

    def open?()
      return !@handle.nil?
    end

    def write(str)
      begin
        @handle.write(str)
      rescue StandardError
        raise TransportException.new(TransportException::NOT_OPEN)
      end
    end

    def read(sz)
      begin
        data = @handle.recv(sz)
        if (data.length == 0)
          raise TransportException.new("TSocket: Could not read #{sz} bytes from #{@host}:#{@port}")
        end
        return data
      rescue StandardError
        raise TransportException.new(TransportException::NOT_OPEN)
      end
    end

    def close()
      @handle.close() unless @handle.nil?
      @handle = nil
    end
  end
  deprecate_class! :TSocket => Socket

  class ServerSocket < ServerTransport
    def initialize(port)
      @port = port
      @handle = nil
    end

    def listen()
      @handle = TCPserver.new(nil, @port)
    end

    def accept()
      if (@handle != nil)
        sock = @handle.accept()
        trans = Socket.new()
        trans.set_handle(sock)
        return trans
      end
      return nil
    end

    def close()
     @handle.close() unless @handle.nil?
    end
  end
  deprecate_class! :TServerSocket => ServerSocket
end
