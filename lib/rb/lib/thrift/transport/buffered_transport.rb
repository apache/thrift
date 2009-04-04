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
  class BufferedTransport < BaseTransport
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

  class BufferedTransportFactory < BaseTransportFactory
    def get_transport(transport)
      return BufferedTransport.new(transport)
    end
  end
end