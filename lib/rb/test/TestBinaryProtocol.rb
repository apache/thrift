#!/usr/bin/env ruby
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

require 'thrift'
require 'test/unit'

class TestBinaryProtocol < Test::Unit::TestCase
  def test_defferent_data_types
    begin
      port = ARGV[0] || 9090
      pid = Process.fork
      if pid.nil?then
        buf = String.new('1234567')
        socket = Thrift::ServerSocket.new(port)
        socket.listen()
        client = socket.accept()
        transport = Thrift::BufferedTransport.new(client)
        protocol = Thrift::BinaryProtocol.new(transport)
        
        acc_bool = protocol.read_bool()     
        assert_equal(true,acc_bool)
       
        acc_boolf = protocol.read_bool()
        assert_equal(false,acc_boolf)
      
        acc_byte = protocol.read_byte()
        assert_equal(123,acc_byte)
        
        acc_i16 = protocol.read_i16()
        assert_equal(4203,acc_i16)
       
        acc_i32 = protocol.read_i32()
        assert_equal(2000000032,acc_i32)
       
        acc_i64 = protocol.read_i64()
        assert_equal(1844674407370955161,acc_i64)
       
        acc_double = protocol.read_double()
        assert_equal(3.1415926,acc_double)
        assert_kind_of(Float,acc_double)
       
        acc_string = protocol.read_string()
        assert_equal("hello_world123456789!@#$%&",acc_string)
       
        acc_binary = protocol.read_binary()
        ret = acc_binary.bytes.to_a
        assert_equal((0...256).reverse_each.to_a,ret)
       
        acc_message = protocol.read_message_begin()
        protocol.read_message_end()
        assert_equal(["hello_world",123,455536],acc_message)
       
        acc_field = protocol.read_field_begin()
        protocol.read_field_end()
        assert_equal([nil,123,25536],acc_field)
       
        acc_map = protocol.read_map_begin()
        protocol.read_map_end()
        assert_equal([56,123,2000000032],acc_map)
       
        acc_list = protocol.read_list_begin()
        protocol.read_list_end()
        assert_equal([125,2000000032],acc_list)
       
        acc_set = protocol.read_set_begin()
        protocol.read_set_end()
        assert_equal([-124,2000000022],acc_set)
       
        exit(0) 
      else  
        sleep(2)
        socket = Thrift::Socket.new('localhost',port)
        transport = Thrift::BufferedTransport.new(socket)
        transport.open()
        protocol = Thrift::BinaryProtocol.new(transport)
       
        protocol.write_bool(true)
        transport.flush()

        protocol.write_bool(false)
        transport.flush()

        protocol.write_byte(123)
        transport.flush()

        protocol.write_i16(4203)
        transport.flush()

        protocol.write_i32(2000000032)	
        transport.flush()

        protocol.write_i64(1844674407370955161)
        transport.flush()

        protocol.write_double(3.1415926)
        transport.flush()	

        protocol.write_string("hello_world123456789!@#$%&")
        transport.flush()

        val = (0...256).reverse_each.to_a
        protocol.write_binary(val.pack('C*'))
        transport.flush()

        protocol.write_message_begin("hello_world",123,455536)
        protocol.write_message_end()
        transport.flush()

        protocol.write_field_begin("hello_world",123,25536)
        protocol.write_field_end()
        transport.flush()

        protocol.write_map_begin(56,123,2000000032)
        protocol.write_map_end()
        transport.flush()

        protocol.write_list_begin(125,2000000032)
        protocol.write_list_end()
        transport.flush()

        protocol.write_set_begin(-124,2000000022)
        protocol.write_set_end()
        transport.flush()
        Process.wait(pid)
      end
    end
  end
end 
