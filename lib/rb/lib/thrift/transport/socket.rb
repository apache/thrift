# Copyright (c) 2006- Facebook
# Distributed under the Apache Software License
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
      @desc = "#{host}:#{port}"
      @handle = nil
    end

    attr_accessor :handle

    def open
      begin
        @handle = TCPSocket.new(@host, @port)
      rescue StandardError
        raise TransportException.new(TransportException::NOT_OPEN, "Could not connect to #{@desc}")
      end
    end

    def open?
      !@handle.nil?
    end

    def write(str)
      begin
        @handle.write(str)
      rescue StandardError
        @handle.close
        @handle = nil
        raise TransportException.new(TransportException::NOT_OPEN)
      end
    end

    def read(sz, partial=false)
      begin
        if partial
          data = @handle.readpartial(sz)
        else
          data = @handle.read(sz)
        end
      rescue Errno::EAGAIN => e
        # let our parent know that the nonblock read failed
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

    def readpartial(sz)
      read(sz, true)
    end

    def close
      @handle.close unless @handle.nil? or @handle.closed?
      @handle = nil
    end

    def to_io
      @handle
    end
  end
  deprecate_class! :TSocket => Socket

  class ServerSocket < ServerTransport
    # call-seq: initialize(host = nil, port)
    def initialize(host_or_port, port = nil)
      if port
        @host = host_or_port
        @port = port
      else
        @host = nil
        @port = host_or_port
      end
      @handle = nil
    end

    attr_reader :handle

    def listen
      @handle = TCPServer.new(@host, @port)
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
     @handle.close unless @handle.nil? or @handle.closed?
     @handle = nil
    end

    def closed?
      @handle.nil? or @handle.closed?
    end

    alias to_io handle
  end
  deprecate_class! :TServerSocket => ServerSocket
end
