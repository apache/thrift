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
require File.expand_path('../../test_helper', __dir__)
require 'thrift'
require 'thread'

class TestJsonProtocol < Test::Unit::TestCase
  def test_different_data_types
    port = (ARGV[0] || 7070).to_i
    server_ready = Queue.new

    server_thread = Thread.new do
      socket = Thrift::ServerSocket.new(port)
      transport = nil

      socket.listen
      server_ready << true
      client = socket.accept
      transport = Thrift::BufferedTransport.new(client)
      protocol = Thrift::JsonProtocol.new(transport)

      results = {}
      results[:acc_json_string]  = protocol.read_json_string
      results[:acc_json_base64]  = protocol.read_json_base64

      protocol.read_json_array_start
      results[:acc_json_integer] = protocol.read_json_integer
      protocol.read_json_array_end

      protocol.read_json_array_start
      results[:acc_json_double] = protocol.read_json_double
      protocol.read_json_array_end

      results[:acc_message] = protocol.read_message_begin
      protocol.read_message_end

      results[:acc_field] = protocol.read_field_begin
      protocol.read_field_end

      results[:acc_map] = protocol.read_map_begin
      protocol.read_map_end

      results[:acc_map2] = protocol.read_map_begin
      protocol.read_map_end

      results[:acc_list] = protocol.read_list_begin
      protocol.read_list_end

      results[:acc_set] = protocol.read_set_begin
      protocol.read_set_end

      protocol.read_json_object_start
      results[:acc_bool] = protocol.read_bool
      protocol.read_json_object_end

      protocol.read_json_object_start
      results[:acc_byte] = protocol.read_byte
      protocol.read_json_object_end

      protocol.read_json_object_start
      results[:acc_i16] = protocol.read_i16
      protocol.read_json_object_end

      protocol.read_json_object_start
      results[:acc_i32] = protocol.read_i32
      protocol.read_json_object_end

      protocol.read_json_object_start
      results[:acc_i64] = protocol.read_i64
      protocol.read_json_object_end

      protocol.read_json_object_start
      results[:acc_double] = protocol.read_double
      protocol.read_json_object_end

      protocol.read_json_object_start
      results[:acc_string] = protocol.read_string
      protocol.read_json_object_end

      protocol.read_json_object_start
      acc_binary = protocol.read_binary
      results[:acc_binary] = acc_binary.bytes.to_a
      protocol.read_json_object_end

      results
    ensure
      transport&.close
      socket&.close
    end

    server_ready.pop

    socket = Thrift::Socket.new('localhost', port)
    transport = Thrift::BufferedTransport.new(socket)
    transport.open
    protocol = Thrift::JsonProtocol.new(transport)

    # acc_json_string
    protocol.write_json_string('hello_world123!@#$%')
    transport.flush

    # acc_json_base64
    protocol.write_json_base64('hello_world12233!@#$%')
    transport.flush

    protocol.write_json_array_start
    # acc_json_integer
    protocol.write_json_integer(2553369689)
    protocol.write_json_array_end
    transport.flush

    protocol.write_json_array_start
    # acc_json_double
    protocol.write_json_double(3.1415926)
    protocol.write_json_array_end
    transport.flush

    # acc_message
    protocol.write_message_begin('hello_world', 4, 455536)
    protocol.write_message_end
    transport.flush

    # acc_field
    protocol.write_field_begin("hello_world", Thrift::Types::I16, 5)
    protocol.write_field_end
    transport.flush

    # acc_map
    protocol.write_map_begin(Thrift::Types::I16, Thrift::Types::I32, 12)
    protocol.write_map_end
    transport.flush

    # acc_map2
    protocol.write_map_begin(Thrift::Types::I16, Thrift::Types::I32, 0)
    protocol.write_map_end
    transport.flush

    # acc_list
    protocol.write_list_begin(Thrift::Types::I32, 12)
    protocol.write_list_end
    transport.flush

    # acc_set
    protocol.write_set_begin(Thrift::Types::I32, 5)
    protocol.write_set_end
    transport.flush

    protocol.write_json_object_start
    # acc_bool
    protocol.write_bool(true)
    protocol.write_json_object_end
    transport.flush

    protocol.write_json_object_start
    # acc_byte
    protocol.write_byte(123)
    protocol.write_json_object_end
    transport.flush

    protocol.write_json_object_start
    # acc_i16
    protocol.write_i16(4203)
    protocol.write_json_object_end
    transport.flush

    protocol.write_json_object_start
    # acc_i32
    protocol.write_i32(2000000032)
    protocol.write_json_object_end
    transport.flush

    protocol.write_json_object_start
    # acc_i64
    protocol.write_i64(1844674407370955161)
    protocol.write_json_object_end
    transport.flush

    protocol.write_json_object_start
    # acc_double
    protocol.write_double(3.1415926)
    protocol.write_json_object_end
    transport.flush

    protocol.write_json_object_start
    # acc_string
    protocol.write_string("hello_world123456789!@#$%&")
    protocol.write_json_object_end
    transport.flush

    protocol.write_json_object_start
    val = (0...256).reverse_each.to_a
    # acc_binary
    protocol.write_binary(val.pack('C*'))
    protocol.write_json_object_end
    transport.flush

    transport.close

    server_results = server_thread.value
    assert_equal('hello_world123!@#$%', server_results[:acc_json_string])
    assert_equal('hello_world12233!@#$%', server_results[:acc_json_base64])
    assert_equal(2553369689, server_results[:acc_json_integer])
    assert_equal(3.1415926, server_results[:acc_json_double])
    assert_equal("[\"hello_world\", 4, 455536]", "#{server_results[:acc_message]}")
    assert_equal([nil, 6, 5], server_results[:acc_field])
    assert_equal("[6, 8, 12]", "#{server_results[:acc_map]}")
    assert_equal("[6, 8, 0]", "#{server_results[:acc_map2]}")
    assert_equal("[8, 12]", "#{server_results[:acc_list]}")
    assert_equal("[8, 5]", "#{server_results[:acc_set]}")
    assert_equal(true, server_results[:acc_bool])
    assert_equal(123, server_results[:acc_byte])
    assert_equal(4203, server_results[:acc_i16])
    assert_equal(2000000032, server_results[:acc_i32])
    assert_equal(1844674407370955161, server_results[:acc_i64])
    assert_equal(3.1415926, server_results[:acc_double])
    assert_equal("hello_world123456789!@#$%&", server_results[:acc_string])
    assert_equal((0...256).reverse_each.to_a, server_results[:acc_binary])
  end
end
