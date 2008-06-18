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

class TTransportException < Thrift::Exception

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

# TTransport is basically an abstract class, but isn't raising NotImplementedError
# TODO: Think about if this is the right thing - Kevin Clark - 3/27/08
class TTransport
  def open?; end
  deprecate! :isOpen => :open?
  deprecate! :is_open? => :open?

  def open; end

  def close; end

  def read(sz); end

  def read_all(size)
    buff = ''
    have = 0
    
    while (have < size)
      chunk = read(size - have)
      have += chunk.length
      buff << chunk
    end
    
    buff
  end
  deprecate! :readAll => :read_all
  
  def write(buf); end

  def flush; end
end

class TServerTransport
  def listen(); nil; end

  def accept(); nil; end

  def close(); nil; end

end

class TTransportFactory
  def getTransport(trans)
    return trans
  end
end

class TBufferedTransport < TTransport
  def initialize(transport)
    @transport = transport
    @wbuf = ''
  end

  def isOpen()
    return @transport.isOpen()
  end

  def open()
    @transport.open()
  end

  def close()
    @transport.close()
  end

  def read(sz)
    return @transport.read(sz)
  end

  def write(buf)
    @wbuf << buf
  end

  def flush()
    @transport.write(@wbuf)
    @transport.flush()
    @wbuf = ''
  end
end

class TBufferedTransportFactory < TTransportFactory
  def getTransport(transport)
    return TBufferedTransport.new(transport)
  end
end


class TFramedTransport < TTransport
  def initialize(transport, read=true, write=true)
    @transport = transport
    @rbuf      = ''
    @wbuf      = ''
    @read      = read
    @write     = write
  end

  def open?()
    return @transport.open?
  end

  def open()
    @transport.open
  end

  def close()
    @transport.close
  end

  def read(sz)
    if !@read
      return @transport.read(sz)
    end

    if (sz <= 0)
      return ''
    end

    if (@rbuf.length == 0)
      read_frame
    end

    # return full buf
    if (sz > @rbuf.length)
      out = @rbuf
      @rbuf = ''
      return out
    end

    # Return substr
    out = @rbuf.slice(0, sz)
    @rbuf = @rbuf.slice(sz, @rbuf.length)
    return out

  end

  def write(buf,sz=nil)

    if !@write
      return @transport.write(buf);
    end

    if (sz != nil && sz < buf.length)
      buf = buf.slice(0,sz)
    end

    @wbuf << buf

  end

  #
  # Writes the output buffer to the stream in the format of a 4-byte length
  # followed by the actual data.
  #
  def flush
    if !@write
      return @transport.flush
    end

    out = [@wbuf.length].pack('N')
    out << @wbuf
    @transport.write(out)
    @transport.flush
    @wbuf = ''
  end

  private

  def read_frame
    buf  = @transport.readAll(4)
    val  = buf.unpack('N')
    sz   = val[0]

    @rbuf = @transport.readAll(sz)
  end

end


class TFramedTransportFactory < TTransportFactory
  def getTransport(transport)
    return TFramedTransport.new(transport)
  end
end

class TMemoryBuffer < TTransport
  def initialize(sz=1024)
    @buf = ''
    @sz  = sz
    @wpos = 0
    @rpos = 0
  end

  def open?
    return 1
  end

  def open
  end

  def close
  end

  def peek
    return @rpos < @wpos
  end

  def get_buffer
    return @buf
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
    avail = available()

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
    @wpos += wbuf.length()
  end

  def flush
  end
end

## Very very simple implementation of wrapping two objects, one with a #read
## method and one with a #write method, into a transport for thrift.
##
## Assumes both objects are open, remain open, don't require flushing, etc.
class TIOStreamTransport < TTransport
  def initialize(input, output)
    @input = input
    @output = output
  end

  def isOpen; true end
  def read(sz); @input.read(sz) end
  def write(buf); @output.write(buf) end
end
