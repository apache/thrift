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

class TestJsonProtocol < Test::Unit::TestCase
  def test_defferent_data_types
    begin
      port = ARGV[0] || 7070
      pid = Process.fork
      if pid.nil?then
        buf = String.new('1234567')
        socket = Thrift::ServerSocket.new(port)
        socket.listen()
        client = socket.accept()
        transport = Thrift::BufferedTransport.new(client)
        protocol = Thrift::JsonProtocol.new(transport)

        acc_json_string = protocol.read_json_string()
        assert_equal('hello_world123!@#$%',acc_json_string)
     
        acc_json_base64 = protocol.read_json_base64()
        assert_equal('hello_world12233!@#$%',acc_json_base64)

        protocol.read_json_array_start()
        acc_json_integer = protocol.read_json_integer()
        protocol.read_json_array_end()
        assert_equal(2553369689,acc_json_integer)
	
	
        protocol.read_json_array_start()
        acc_json_double = protocol.read_json_double()
        protocol.read_json_array_end()	
        assert_equal(3.1415926,acc_json_double)

        acc_message = protocol.read_message_begin()
        protocol.read_message_end()
        assert_equal("[\"hello_world\", 155510, 102020]","#{acc_message}")

        acc_field = protocol.read_field_begin()
        protocol.read_field_end()
        assert_equal([nil ,6 ,12],acc_field)
        
        acc_map = protocol.read_map_begin()
        protocol.read_map_end()
        assert_equal([10, 8, 20],acc_map)
	
        acc_list = protocol.read_list_begin()
        protocol.read_list_end()
        assert_equal([6, 10],acc_list)	
 
        acc_set = protocol.read_set_begin()
        protocol.read_set_end()
        assert_equal([8, 20],acc_set)	
	
        protocol.read_json_object_start()
        acc_bool = protocol.read_bool()
        protocol.read_json_object_end()
        assert_equal(true,acc_bool)
	
        protocol.read_json_object_start()
        acc_byte = protocol.read_byte()
        protocol.read_json_object_end()
        assert_equal(123,acc_byte)
 
        protocol.read_json_object_start()
        acc_i16 = protocol.read_i16()
        protocol.read_json_object_end()
        assert_equal(4203,acc_i16)


        protocol.read_json_object_start()
        acc_i32 = protocol.read_i32()
        protocol.read_json_object_end()
        assert_equal(2000000032,acc_i32)

        protocol.read_json_object_start()
        acc_i64 = protocol.read_i64()
        protocol.read_json_object_end()
        assert_equal(1844674407370955161,acc_i64)

        protocol.read_json_object_start()
        acc_double = protocol.read_double()
        protocol.read_json_object_end()
        assert_equal(3.1415926,acc_double)
	 
        protocol.read_json_object_start()
        acc_string = protocol.read_string()
        protocol.read_json_object_end()
        assert_equal("sello_world123456789!@#$%",acc_string)
	
        protocol.read_json_object_start()
        acc_binary = protocol.read_binary()
        ret =acc_binary.bytes.to_a
        protocol.read_json_object_end()
        assert_equal((0...256).reverse_each.to_a,ret)

	exit(0) 
      else  
        sleep(2)
        socket = Thrift::Socket.new('localhost',port)
        transport = Thrift::BufferedTransport.new(socket)
        transport.open()
        protocol = Thrift::JsonProtocol.new(transport)
       
        protocol.write_json_string('hello_world123!@#$%')
        transport.flush()
       
        protocol.write_json_base64('hello_world12233!@#$%')	
        transport.flush()
        
        protocol.write_json_array_start()
        protocol.write_json_integer(2553369689)
        protocol.write_json_array_end()
        transport.flush()

        protocol.write_json_array_start()
        protocol.write_json_double(3.1415926)
        protocol.write_json_array_end()
        transport.flush()

        protocol.write_message_begin('hello_world',155510,102020)
        protocol.write_message_end()
        transport.flush()

        protocol.write_field_begin("hello_world",Thrift::Types::I16,12)
        protocol.write_field_end()
        transport.flush()

        protocol.write_map_begin(Thrift::Types::I64,Thrift::Types::I32,20)
        protocol.write_map_end()
        transport.flush()

        protocol.write_list_begin(Thrift::Types::I16,10)
        protocol.write_list_end()
        transport.flush()

        protocol.write_set_begin(Thrift::Types::I32,20)
        protocol.write_set_end()
        transport.flush()

        protocol.write_json_object_start()
        protocol.write_bool(true)
        protocol.write_json_object_end()
        transport.flush()

        protocol.write_json_object_start()
        protocol.write_byte(123)
        protocol.write_json_object_end()
        transport.flush()


        protocol.write_json_object_start()
        protocol.write_i16(4203)
        protocol.write_json_object_end()
        transport.flush()

        protocol.write_json_object_start()
        protocol.write_i32(2000000032)
        protocol.write_json_object_end()
        transport.flush()

        protocol.write_json_object_start()
        protocol.write_i64(1844674407370955161)
        protocol.write_json_object_end()
        transport.flush()
	
        protocol.write_json_object_start()
        protocol.write_double(3.1415926)
        protocol.write_json_object_end()
        transport.flush()

        protocol.write_json_object_start()
        protocol.write_string("sello_world123456789!@#$%")
        protocol.write_json_object_end()
        transport.flush()

        protocol.write_json_object_start()
        val = (0...256).reverse_each.to_a
        protocol.write_binary(val.pack('C*'))
        protocol.write_json_object_end()
        transport.flush()


        transport.close()
        Process.wait(pid)
      end
    end
  end
end
