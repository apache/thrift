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

# this require is to make generated struct definitions happy
require 'set'

module Thrift
  class ProtocolException < Exception

    UNKNOWN = 0
    INVALID_DATA = 1
    NEGATIVE_SIZE = 2
    SIZE_LIMIT = 3
    BAD_VERSION = 4

    attr_reader :type

    def initialize(type=UNKNOWN, message=nil)
      super(message)
      @type = type
    end
  end

  class BaseProtocol

    attr_reader :trans

    def initialize(trans)
      @trans = trans
    end

    def native?
      puts "wrong method is being called!"
      false
    end

    def write_message_begin(name, type, seqid)
      raise NotImplementedError
    end

    def write_message_end; nil; end

    def write_struct_begin(name)
      raise NotImplementedError
    end

    def write_struct_end; nil; end

    def write_field_begin(name, type, id)
      raise NotImplementedError
    end

    def write_field_end; nil; end

    def write_field_stop
      raise NotImplementedError
    end

    def write_map_begin(ktype, vtype, size)
      raise NotImplementedError
    end

    def write_map_end; nil; end

    def write_list_begin(etype, size)
      raise NotImplementedError
    end

    def write_list_end; nil; end

    def write_set_begin(etype, size)
      raise NotImplementedError
    end

    def write_set_end; nil; end

    def write_bool(bool)
      raise NotImplementedError
    end

    def write_byte(byte)
      raise NotImplementedError
    end

    def write_i16(i16)
      raise NotImplementedError
    end

    def write_i32(i32)
      raise NotImplementedError
    end

    def write_i64(i64)
      raise NotImplementedError
    end

    def write_double(dub)
      raise NotImplementedError
    end

    def write_string(str)
      raise NotImplementedError
    end

    def read_message_begin
      raise NotImplementedError
    end

    def read_message_end; nil; end

    def read_struct_begin
      raise NotImplementedError
    end

    def read_struct_end; nil; end

    def read_field_begin
      raise NotImplementedError
    end

    def read_field_end; nil; end

    def read_map_begin
      raise NotImplementedError
    end

    def read_map_end; nil; end

    def read_list_begin
      raise NotImplementedError
    end

    def read_list_end; nil; end

    def read_set_begin
      raise NotImplementedError
    end

    def read_set_end; nil; end

    def read_bool
      raise NotImplementedError
    end

    def read_byte
      raise NotImplementedError
    end

    def read_i16
      raise NotImplementedError
    end

    def read_i32
      raise NotImplementedError
    end

    def read_i64
      raise NotImplementedError
    end

    def read_double
      raise NotImplementedError
    end

    def read_string
      raise NotImplementedError
    end

    def write_field(name, type, fid, value)
      write_field_begin(name, type, fid)
      write_type(type, value)
      write_field_end
    end

    def write_type(type, value)
      case type
      when Types::BOOL
        write_bool(value)
      when Types::BYTE
        write_byte(value)
      when Types::DOUBLE
        write_double(value)
      when Types::I16
        write_i16(value)
      when Types::I32
        write_i32(value)
      when Types::I64
        write_i64(value)
      when Types::STRING
        write_string(value)
      when Types::STRUCT
        value.write(self)
      else
        raise NotImplementedError
      end
    end

    def read_type(type)
      case type
      when Types::BOOL
        read_bool
      when Types::BYTE
        read_byte
      when Types::DOUBLE
        read_double
      when Types::I16
        read_i16
      when Types::I32
        read_i32
      when Types::I64
        read_i64
      when Types::STRING
        read_string
      else
        raise NotImplementedError
      end
    end

    def skip(type)
      case type
      when Types::STOP
        nil
      when Types::BOOL
        read_bool
      when Types::BYTE
        read_byte
      when Types::I16
        read_i16
      when Types::I32
        read_i32
      when Types::I64
        read_i64
      when Types::DOUBLE
        read_double
      when Types::STRING
        read_string
      when Types::STRUCT
        read_struct_begin
        while true
          name, type, id = read_field_begin
          break if type == Types::STOP
          skip(type)
          read_field_end
        end
        read_struct_end
      when Types::MAP
        ktype, vtype, size = read_map_begin
        size.times do
          skip(ktype)
          skip(vtype)
        end
        read_map_end
      when Types::SET
        etype, size = read_set_begin
        size.times do
          skip(etype)
        end
        read_set_end
      when Types::LIST
        etype, size = read_list_begin
        size.times do
          skip(etype)
        end
        read_list_end
      end
    end
  end

  class BaseProtocolFactory
    def get_protocol(trans)
      raise NotImplementedError
    end
  end
end