# Copyright (c) 2006- Facebook
# Distributed under the Apache Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#

module Thrift
  class TransportException < Exception
    UNKNOWN = 0
    NOT_OPEN = 1
    ALREADY_OPEN = 2
    TIMED_OUT = 3
    END_OF_FILE = 4

    attr_reader :type

    def initialize(type=UNKNOWN, message=nil)
      super(message)
      @type = type
    end
  end
  deprecate_class! :TTransportException => TransportException

# Transport is basically an abstract class, but isn't raising NotImplementedError
# TODO: Think about if this is the right thing - Kevin Clark - 3/27/08
  class Transport
    def open?; end
    deprecate! :isOpen => :open?
    deprecate! :is_open? => :open?

    def open; end

    def close; end

    def read(sz); end

    def read_all(size)
      buf = ''
    
      while (buf.length < size)
        chunk = read(size - buf.length)
        buf << chunk
      end
    
      buf
    end
    deprecate! :readAll => :read_all
  
    def write(buf); end
    alias_method :<<, :write

    def flush; end
  end
  deprecate_class! :TTransport => Transport

  class ServerTransport
    def listen; nil; end

    def accept; nil; end

    def close; nil; end

    def closed?; nil; end
  end
  deprecate_class! :TServerTransport => ServerTransport

  class TransportFactory
    def get_transport(trans)
      return trans
    end
    deprecate! :getTransport => :get_transport
  end
  deprecate_class! :TTransportFactory => TransportFactory

  class BufferedTransport < Transport
    def initialize(transport)
      @transport = transport
      @wbuf = ''
    end

    def open?
      return @transport.open?
    end

    def open
      @transport.open
    end

    def close
      flush
      @transport.close
    end

    def read(sz)
      return @transport.read(sz)
    end

    def write(buf)
      @wbuf << buf
    end

    def flush
      if @wbuf != ''
        @transport.write(@wbuf)
        @wbuf = ''
      end
      
      @transport.flush
    end
  end
  deprecate_class! :TBufferedTransport => BufferedTransport

  class BufferedTransportFactory < TransportFactory
    def get_transport(transport)
      return BufferedTransport.new(transport)
    end
  end
  deprecate_class! :TBufferedTransportFactory => BufferedTransportFactory

  class FramedTransport < Transport
    def initialize(transport, read=true, write=true)
      @transport = transport
      @rbuf      = ''
      @wbuf      = ''
      @read      = read
      @write     = write
    end

    def open?
      @transport.open?
    end

    def open
      @transport.open
    end

    def close
      @transport.close
    end

    def read(sz)
      return @transport.read(sz) unless @read

      return '' if sz <= 0

      read_frame if @rbuf.empty?

      @rbuf.slice!(0, sz)
    end

    def write(buf,sz=nil)
      return @transport.write(buf) unless @write

      @wbuf << (sz ? buf[0...sz] : buf)
    end

    #
    # Writes the output buffer to the stream in the format of a 4-byte length
    # followed by the actual data.
    #
    def flush
      return @transport.flush unless @write

      out = [@wbuf.length].pack('N')
      out << @wbuf
      @transport.write(out)
      @transport.flush
      @wbuf = ''
    end

    private

    def read_frame
      sz = @transport.read_all(4).unpack('N').first

      @rbuf = @transport.read_all(sz).dup # protect against later #slice!
    end
  end
  deprecate_class! :TFramedTransport => FramedTransport

  class FramedTransportFactory < TransportFactory
    def get_transport(transport)
      return FramedTransport.new(transport)
    end
  end
  deprecate_class! :TFramedTransportFactory => FramedTransportFactory

  class MemoryBuffer < Transport
    # If you pass a string to this, you should #dup that string
    # unless you want it to be modified by #read and #write
    #--
    # yes this behavior is intentional
    def initialize(buffer = nil)
      @buf = buffer || ''
    end

    def open?
      return true
    end

    def open
    end

    def close
    end

    def peek
      not @buf.empty?
    end

    # this method does not use the passed object directly but copies it
    def reset_buffer(new_buf = '')
      @buf.replace new_buf
    end

    def available
      @buf.length
    end

    def read(len)
      @buf.slice!(0, len)
    end

    def write(wbuf)
      @buf << wbuf
    end

    def flush
    end
  end
  deprecate_class! :TMemoryBuffer => MemoryBuffer

  ## Very very simple implementation of wrapping two objects, one with a #read
  ## method and one with a #write method, into a transport for thrift.
  ##
  ## Assumes both objects are open, remain open, don't require flushing, etc.
  class IOStreamTransport < Transport
    def initialize(input, output)
      @input = input
      @output = output
    end

    def open?; true end
    def read(sz); @input.read(sz) end
    def write(buf); @output.write(buf) end
  end
  deprecate_class! :TIOStreamTransport => IOStreamTransport
end
