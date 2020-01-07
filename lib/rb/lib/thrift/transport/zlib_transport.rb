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
require 'zlib'

module Thrift
  class ZlibTransport < BaseTransport
    def initialize(transport, level=Zlib::BEST_COMPRESSION)
      @transport = transport
      @dez = Zlib::Deflate.new(level)
      @inz = Zlib::Inflate.new
      @buf = Bytes.empty_byte_buffer
    end

    def open?
      @transport.open?
    end

    def open
      @transport.open
    end

    def close
      begin
        @transport.write(@dez.finish)
      rescue => e
        raise EOFError, "Failed to finish data: #{e}"
      end
      @transport.flush
      @transport.close
    end

    def read(sz)
      while @buf.size < sz do
        begin
          @buf << @inz.inflate(@transport.read(sz))
        rescue => e
          raise EOFError, "Failed to inflate data: #{e}"
        end
      end
      @buf.slice!(0, sz)
    end

    def write(buf)
      begin
        @transport.write(@dez.deflate(buf))
      rescue => e
        raise EOFError, "Failed to deflate data: #{e}"
      end
    end

    def flush
      begin
        @transport.write(@dez.flush)
      rescue => e
        raise EOFError, "Failed to flush data: #{e}"
      end
      @transport.flush
    end

    def to_s
      "zlib(#{@transport.to_s})"
    end
  end

  class ZlibTransportFactory < BaseTransportFactory
    def get_transport(transport)
      ZlibTransport.new(transport)
    end

    def to_s
      "zlib"
    end
  end
end
