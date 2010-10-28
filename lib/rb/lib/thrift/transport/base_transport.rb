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

  class BaseTransport
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
  
  class BaseTransportFactory
    def get_transport(trans)
      return trans
    end
  end
end