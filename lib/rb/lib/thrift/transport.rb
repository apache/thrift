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
    DEFAULT_BUFFER = 4096
    
    def initialize(transport)
      @transport = transport
      @wbuf = ''
      @rbuf = ''
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
      ret = @rbuf.slice!(0...sz) 
      if ret.length == 0
        @rbuf = @transport.read([sz, DEFAULT_BUFFER].max) 
        @rbuf.slice!(0...sz) 
      else 
        ret 
      end
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

    def borrow(requested_length = 0)
      # $stderr.puts "#{Time.now.to_f} Have #{@rbuf.length} asking for #{requested_length.inspect}"
      return @rbuf if @rbuf.length > requested_length
      
      if @rbuf.length < DEFAULT_BUFFER
        @rbuf << @transport.read([requested_length, DEFAULT_BUFFER].max)
      end
      
      if @rbuf.length < requested_length
        @rbuf << @transport.read_all(requested_length - @rbuf.length)
      end
    
      @rbuf
    end
    
    def consume!(size)
      @rbuf.slice!(0...size)
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

    def borrow(requested_length = 0)
      read_frame if @rbuf.empty?
      # there isn't any more coming, so if it's not enough, it's an error.
      raise EOFError if requested_length > @rbuf.size
      @rbuf
    end
    
    def consume!(size)
      @rbuf.slice!(0...size)
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
    GARBAGE_BUFFER_SIZE = 4*(2**10) # 4kB

    # If you pass a string to this, you should #dup that string
    # unless you want it to be modified by #read and #write
    #--
    # this behavior is no longer required. If you wish to change it
    # go ahead, just make sure the specs pass
    def initialize(buffer = nil)
      @buf = buffer || ''
      @index = 0
    end

    def open?
      return true
    end

    def open
    end

    def close
    end

    def peek
      @index < @buf.size
    end

    # this method does not use the passed object directly but copies it
    def reset_buffer(new_buf = '')
      @buf.replace new_buf
      @index = 0
    end

    def available
      @buf.length - @index
    end

    def read(len)
      data = @buf.slice(@index, len)
      @index += len
      @index = @buf.size if @index > @buf.size
      if @index >= GARBAGE_BUFFER_SIZE
        @buf = @buf.slice(@index..-1)
        @index = 0
      end
      data
    end

    def write(wbuf)
      @buf << wbuf
    end

    def flush
    end

    # For fast binary protocol access
    def borrow(size = nil)
      if size.nil?
        @buf[@index..-1]
      else
        if size > available
          raise EOFError # Memory buffers only get one shot.
        else
          @buf[@index, size]
        end
      end
    end

    alias_method :consume!, :read
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

    def open?; not @input.closed? or not @output.closed? end
    def read(sz); @input.read(sz) end
    def write(buf); @output.write(buf) end
    def close; @input.close; @output.close end
    def to_io; @input end # we're assuming this is used in a IO.select for reading
  end
  deprecate_class! :TIOStreamTransport => IOStreamTransport
end
