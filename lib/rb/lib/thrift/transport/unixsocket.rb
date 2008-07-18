require 'thrift/transport'
require 'socket'

module Thrift
  class UNIXSocket < Socket
    def initialize(path, timeout=nil)
      @path = path
      @timeout = timeout
      @desc = @path # for read()'s error
      @handle = nil
    end

    def open
      begin
        @handle = ::UNIXSocket.new(@path)
      rescue StandardError
        raise TransportException.new(TransportException::NOT_OPEN, "Could not open UNIX socket at #{@path}")
      end
    end
  end

  class UNIXServerSocket < ServerTransport
    def initialize(path)
      @path = path
      @handle = nil
    end

    attr_accessor :handle

    def listen
      @handle = ::UNIXServer.new(@path)
    end

    def accept
      unless @handle.nil?
        sock = @handle.accept
        trans = UNIXSocket.new(nil)
        trans.handle = sock
        trans
      end
    end

    def close
      if @handle
        @handle.close unless @handle.closed?
        @handle = nil
        # UNIXServer doesn't delete the socket file, so we have to do it ourselves
        File.delete(@path)
      end
    end

    def closed?
      @handle.nil? or @handle.closed?
    end

    alias to_io handle
  end
end
