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
  class FramedTransport < BaseTransport
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

  class FramedTransportFactory < BaseTransportFactory
    def get_transport(transport)
      return FramedTransport.new(transport)
    end
  end
end