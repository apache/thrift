# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
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

    def flush; end
  end
  deprecate_class! :TTransport => Transport

  class ServerTransport
    def listen; nil; end

    def accept; nil; end

    def close; nil; end
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
      @transport.close
    end

    def read(sz)
      return @transport.read(sz)
    end

    def write(buf)
      @wbuf << buf
    end

    def flush
      @transport.write(@wbuf)
      @transport.flush
      @wbuf = ''
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
      return @transport.open?
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

      # return full buf
      out = @rbuf.slice(0...sz)
      @rbuf = @rbuf.slice(sz..-1) || ''
      out
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

      @rbuf = @transport.read_all(sz)
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
    def initialize(sz=1024)
      @buf = ''
      @sz  = sz
      @wpos = 0
      @rpos = 0
    end

    def open?
      return true
    end

    def open
    end

    def close
    end

    def peek
      return @rpos < @wpos
    end

    def reset_buffer(new_buf = '')
       @buf  = new_buf
       @sz   = new_buf.length
       @wpos = new_buf.length
       @rpos = 0
    end

    def available
      return @wpos - @rpos
    end

    def read(len)
      avail = available

      return '' if avail == 0

      #how much to give
      give = len
      give = avail if avail < len

      ret = @buf.slice(@rpos,give)

      @rpos += give;

      return ret;
    end

    def write(wbuf)
      @buf  << wbuf
      @wpos += wbuf.length
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
