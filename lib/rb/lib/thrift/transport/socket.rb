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
    def initialize(host='localhost', port=9090, timeout=nil)
      @host = host
      @port = port
      @timeout = timeout
      @desc = "#{host}:#{port}"
      @handle = nil
    end

    attr_accessor :handle, :timeout

    def open
      begin
        @handle = TCPSocket.new(@host, @port)
      rescue StandardError
        raise TransportException.new(TransportException::NOT_OPEN, "Could not connect to #{@desc}")
      end
    end

    def open?
      !@handle.nil? and !@handle.closed?
    end

    def write(str)
      raise IOError, "closed stream" unless open?
      begin
        if @timeout.nil? or @timeout == 0
          @handle.write(str)
        else
          len = 0
          start = Time.now
          while Time.now - start < @timeout
            rd, wr, = IO.select(nil, [@handle], nil, @timeout)
            if wr and not wr.empty?
              len += @handle.write_nonblock(str[len..-1])
              break if len >= str.length
            end
          end
          if len < str.length
            raise TransportException.new(TransportException::TIMED_OUT, "Socket: Timed out writing #{str.length} bytes to #{@desc}")
          else
            len
          end
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
      raise IOError, "closed stream" unless open?

      begin
        if @timeout.nil? or @timeout == 0
          data = @handle.readpartial(sz)
        else
          # it's possible to interrupt select for something other than the timeout
          # so we need to ensure we've waited long enough
          start = Time.now
          rd = nil # scoping
          loop do
            rd, = IO.select([@handle], nil, nil, @timeout)
            break if (rd and not rd.empty?) or Time.now - start >= @timeout
          end
          if rd.nil? or rd.empty?
            raise TransportException.new(TransportException::TIMED_OUT, "Socket: Timed out reading #{sz} bytes from #{@desc}")
          else
            data = @handle.readpartial(sz)
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
