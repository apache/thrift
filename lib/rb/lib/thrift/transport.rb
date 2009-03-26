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

  class Transport
    def open?; end
    
    def open; end

    def close; end

    def read(sz)
      raise NotImplementedError
    end

    def read_all(size)
      buf = ''
    
      while (buf.length < size)
        chunk = read(size - buf.length)
        buf << chunk
      end
    
      buf
    end
  
    def write(buf); end
    alias_method :<<, :write

    def flush; end
  end

  class ServerTransport
    def listen
      raise NotImplementedError
    end

    def accept
      raise NotImplementedError
    end
      
    def close; nil; end

    def closed?
      raise NotImplementedError
    end
  end

  class TransportFactory
    def get_transport(trans)
      return trans
    end
  end

  class BufferedTransport < Transport
    DEFAULT_BUFFER = 4096
    
    def initialize(transport)
      @transport = transport
      @wbuf = ''
      @rbuf = ''
      @index = 0
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
      @index += sz
      ret = @rbuf.slice(@index - sz, sz) || ''

      if ret.length == 0
        @rbuf = @transport.read([sz, DEFAULT_BUFFER].max)
        @index = sz
        ret = @rbuf.slice(0, sz) || ''
      end

      ret
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

  class BufferedTransportFactory < TransportFactory
    def get_transport(transport)
      return BufferedTransport.new(transport)
    end
  end

  class FramedTransport < Transport
    def initialize(transport, read=true, write=true)
      @transport = transport
      @rbuf      = ''
      @wbuf      = ''
      @read      = read
      @write     = write
      @index      = 0
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

      read_frame if @index >= @rbuf.length

      @index += sz
      @rbuf.slice(@index - sz, sz) || ''
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

      @index = 0
      @rbuf = @transport.read_all(sz)
    end
  end

  class FramedTransportFactory < TransportFactory
    def get_transport(transport)
      return FramedTransport.new(transport)
    end
  end

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

    def inspect_buffer
      out = []
      for idx in 0...(@buf.size)
        # if idx != 0
        #   out << " "
        # end
        
        if idx == @index
          out << ">"
        end
        
        out << @buf[idx].to_s(16)
      end
      out.join(" ")
    end
  end

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
end
