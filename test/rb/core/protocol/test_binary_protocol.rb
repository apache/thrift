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

class TestBinaryProtocol < Test::Unit::TestCase
  def test_different_data_types
    port = (ARGV[0] || 9090).to_i
    server_ready = Queue.new

    server_thread = Thread.new do
      socket = Thrift::ServerSocket.new(port)
      transport = nil

      socket.listen
      server_ready << true
      client = socket.accept
      transport = Thrift::BufferedTransport.new(client)
      protocol = Thrift::BinaryProtocol.new(transport)

      results = {}
      results[:acc_bool]   = protocol.read_bool
      results[:acc_boolf]  = protocol.read_bool
      results[:acc_byte]   = protocol.read_byte
      results[:acc_i16]    = protocol.read_i16
      results[:acc_i32]    = protocol.read_i32
      results[:acc_i64]    = protocol.read_i64
      results[:acc_double] = protocol.read_double
      results[:acc_string] = protocol.read_string

      acc_binary = protocol.read_binary
      results[:acc_binary] = acc_binary.bytes.to_a

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

      results
    ensure
      transport&.close
      socket&.close
    end

    server_ready.pop

    socket = Thrift::Socket.new('localhost', port)
    transport = Thrift::BufferedTransport.new(socket)
    transport.open
    protocol = Thrift::BinaryProtocol.new(transport)

    # acc_bool
    protocol.write_bool(true)
    transport.flush

    # acc_boolf
    protocol.write_bool(false)
    transport.flush

    # acc_byte
    protocol.write_byte(123)
    transport.flush

    # acc_i16
    protocol.write_i16(4203)
    transport.flush

    # acc_i32
    protocol.write_i32(2000000032)
    transport.flush

    # acc_i64
    protocol.write_i64(1844674407370955161)
    transport.flush

    # acc_double
    protocol.write_double(3.1415926)
    transport.flush

    # acc_string
    protocol.write_string("hello_world123456789!@#$%&")
    transport.flush

    val = (0...256).reverse_each.to_a
    # acc_binary
    protocol.write_binary(val.pack('C*'))
    transport.flush

    # acc_message
    protocol.write_message_begin("hello_world", 4, 455536)
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

    transport.close

    server_results = server_thread.value
    assert_equal(true, server_results[:acc_bool])
    assert_equal(false, server_results[:acc_boolf])
    assert_equal(123, server_results[:acc_byte])
    assert_equal(4203, server_results[:acc_i16])
    assert_equal(2000000032, server_results[:acc_i32])
    assert_equal(1844674407370955161, server_results[:acc_i64])
    assert_equal(3.1415926, server_results[:acc_double])
    assert_kind_of(Float, server_results[:acc_double])
    assert_equal("hello_world123456789!@#$%&", server_results[:acc_string])
    assert_equal((0...256).reverse_each.to_a, server_results[:acc_binary])
    assert_equal(["hello_world", 4, 455536], server_results[:acc_message])
    assert_equal([nil, 6, 5], server_results[:acc_field])
    assert_equal("[6, 8, 12]", "#{server_results[:acc_map]}")
    assert_equal("[6, 8, 0]", "#{server_results[:acc_map2]}")
    assert_equal("[8, 12]", "#{server_results[:acc_list]}")
    assert_equal("[8, 5]", "#{server_results[:acc_set]}")
  end
end
