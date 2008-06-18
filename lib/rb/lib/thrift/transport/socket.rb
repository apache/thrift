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

    attr_accessor :handle

    def open
      begin
        @handle = TCPSocket.new(@host, @port)
      rescue StandardError
        raise TransportException.new(TransportException::NOT_OPEN, "Could not connect to #{@host}:#{@port}")
      end
    end

    def open?
      !@handle.nil?
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
      rescue StandardError
        raise TransportException.new(TransportException::NOT_OPEN)
      end
      if (data.length == 0)
        raise TransportException.new(TransportException::UNKNOWN, "Socket: Could not read #{sz} bytes from #{@host}:#{@port}")
      end
      data
    end

    def close
      @handle.close unless @handle.nil?
      @handle = nil
    end
  end
  deprecate_class! :TSocket => Socket

  class ServerSocket < ServerTransport
    def initialize(port)
      @port = port
      @handle = nil
    end

    attr_reader :handle

    def listen
      @handle = TCPServer.new(nil, @port)
    end

    def accept
      unless @handle.nil?
        sock = @handle.accept
        trans = Socket.new
        trans.handle = sock
        trans
      end
    end

    def close
     @handle.close unless @handle.nil?
     @handle = nil
    end
  end
  deprecate_class! :TServerSocket => ServerSocket
end
