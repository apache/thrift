# encoding: ascii-8bit
# frozen_string_literal: true
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
  class BufferedTransport < BaseTransport
    DEFAULT_BUFFER = 4096

    # By default writes are held until flush. A positive write_buffer_size opts
    # into earlier downstream writes, bounding this layer's staging in bytes.
    # It does not bound buffers owned by the caller or the underlying transport.
    # Small writes are coalesced; writes at least this size bypass staging. Only
    # explicit flush (including close) flushes downstream. Writes and their
    # errors can become observable before flush when this option is enabled.
    def initialize(transport, write_buffer_size: nil)
      unless write_buffer_size.nil? || (write_buffer_size.is_a?(Integer) && write_buffer_size > 0)
        raise ArgumentError, "write_buffer_size must be a positive Integer or nil"
      end

      @transport = transport
      @write_buffer_size = write_buffer_size
      @wbuf = Bytes.empty_byte_buffer
      @rbuf = Bytes.empty_byte_buffer
      @index = 0
    end

    def open?
      @transport.open?
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
      ret = @rbuf.slice(@index - sz, sz) || Bytes.empty_byte_buffer

      if ret.length == 0
        @rbuf = @transport.read([sz, DEFAULT_BUFFER].max)
        @index = sz
        ret = @rbuf.slice(0, sz) || Bytes.empty_byte_buffer
      end

      ret
    ensure
      clear_consumed_read_buffer if @rbuf && @index >= @rbuf.size
    end

    def read_byte
      if @index < @rbuf.size - 1
        @index += 1
        return Bytes.get_string_byte(@rbuf, @index - 1)
      end

      begin
        # If the read buffer is exhausted, try to read up to DEFAULT_BUFFER more bytes into it.
        if @index >= @rbuf.size
          @rbuf = @transport.read(DEFAULT_BUFFER)
          @index = 0
        end

        # The read buffer has some data now, read a single byte. Using get_string_byte() avoids
        # allocating a temp string of size 1 unnecessarily.
        @index += 1
        Bytes.get_string_byte(@rbuf, @index - 1)
      ensure
        clear_consumed_read_buffer if @rbuf && @index >= @rbuf.size
      end
    end

    # Reads a number of bytes from the transport into the buffer passed.
    #
    # buffer - The String (byte buffer) to write data to; this is assumed to have a BINARY encoding.
    # size   - The number of bytes to read from the transport and write to the buffer.
    #
    # Returns the number of bytes read.
    def read_into_buffer(buffer, size)
      i = 0
      while i < size
        # If the read buffer is exhausted, try to read up to DEFAULT_BUFFER more bytes into it.
        if @index >= @rbuf.size
          @rbuf = @transport.read(DEFAULT_BUFFER)
          @index = 0
        end

        # The read buffer has some data now, so copy bytes over to the output buffer.
        byte = Bytes.get_string_byte(@rbuf, @index)
        Bytes.set_string_byte(buffer, i, byte)
        @index += 1
        i += 1
      end
      i
    ensure
      clear_consumed_read_buffer if @rbuf && @index >= @rbuf.size
    end

    def write(buf)
      buf = Bytes.force_binary_encoding(buf)
      if @write_buffer_size
        drain_write_buffer if @wbuf.bytesize + buf.bytesize > @write_buffer_size
        if buf.bytesize >= @write_buffer_size
          # Keep caller ownership even if the underlying transport retains or
          # modifies the string. Large writes do not enter our staging buffer.
          @transport.write(String.new(buf))
          return @wbuf
        end
      end

      @wbuf << buf
    end

    def flush
      drain_write_buffer
      @transport.flush
    end

    def to_s
      "buffered(#{@transport})"
    end

    private

    # Replace exhausted storage; clearing it in place could change returned slices.
    def clear_consumed_read_buffer
      unless @rbuf.empty?
        @rbuf = Bytes.empty_byte_buffer
        @index = 0
      end
    end

    # Draining must not flush downstream: that could end a frame or HTTP request.
    # Keep staged data if write raises, as for an explicit flush. A failed direct
    # write is not staged; the caller must handle it. Partial downstream writes
    # followed by an exception cannot be safely retried at this layer.
    def drain_write_buffer
      unless @wbuf.empty?
        @transport.write(@wbuf)
        @wbuf = Bytes.empty_byte_buffer
      end
    end
  end

  class BufferedTransportFactory < BaseTransportFactory
    def initialize(write_buffer_size: nil)
      unless write_buffer_size.nil? || (write_buffer_size.is_a?(Integer) && write_buffer_size > 0)
        raise ArgumentError, "write_buffer_size must be a positive Integer or nil"
      end

      @write_buffer_size = write_buffer_size
    end

    def get_transport(transport)
      BufferedTransport.new(transport, write_buffer_size: @write_buffer_size)
    end

    def to_s
      "buffered"
    end
  end
end
