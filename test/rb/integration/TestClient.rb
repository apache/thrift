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

$:.push File.dirname(__FILE__) + '/..'

require 'test_helper'
require 'thrift'
require 'thrift_test'

$protocolType = "binary"
$host = "localhost"
$port = 9090
$transport = "buffered"
ARGV.each do|a|
  if a == "--help"
    puts "Allowed options:"
    puts "\t -h [ --help ] \t produce help message"
    puts "\t--host arg (=localhost) \t Host to connect"
    puts "\t--port arg (=9090) \t Port number to listen"
    puts "\t--protocol arg (=binary) \t protocol: binary, accel"
    puts "\t--transport arg (=buffered) transport: buffered, framed, http"
    exit
  elsif a.start_with?("--host")
    $host = a.split("=")[1]
  elsif a.start_with?("--protocol")
    $protocolType = a.split("=")[1]
  elsif a.start_with?("--transport")
    $transport = a.split("=")[1]
  elsif a.start_with?("--port")
    $port = a.split("=")[1].to_i 
  end
end
ARGV=[]

class SimpleClientTest < Test::Unit::TestCase
  def setup 
    unless @socket
      @socket   = Thrift::Socket.new($host, $port)
      transportFactory = Thrift::BufferedTransport.new(@socket)
      if $transport == "buffered"
        transportFactory = Thrift::BufferedTransport.new(@socket)
      elsif $transport == ""
        transportFactory = Thrift::BufferedTransport.new(@socket)
      elsif $transport == "framed"
        transportFactory = Thrift::FramedTransport.new(@socket)
      else
        raise 'Unknown transport type'
      end

      if $protocolType == "binary"
        @protocol = Thrift::BinaryProtocol.new(transportFactory)
      elsif $protocolType == ""
        @protocol = Thrift::BinaryProtocol.new(transportFactory)
      elsif $protocolType == "compact"
        @protocol = Thrift::CompactProtocol.new(transportFactory)
      elsif $protocolType == "json"
        @protocol = Thrift::JsonProtocol.new(transportFactory)
      elsif $protocolType == "accel"
        @protocol = Thrift::BinaryProtocolAccelerated.new(transportFactory)
      else
        raise 'Unknown protocol type'
      end
      @client   = Thrift::Test::ThriftTest::Client.new(@protocol)
      @socket.open
    end
  end
  
  def test_void
    @client.testVoid()
  end

  def test_string
    assert_equal(@client.testString('string'), 'string')
  end

  def test_byte
    val = 8
    assert_equal(@client.testByte(val), val)
    assert_equal(@client.testByte(-val), -val)
  end

  def test_i32
    val = 32
    assert_equal(@client.testI32(val), val)
    assert_equal(@client.testI32(-val), -val)
  end

  def test_i64
    val = 64
    assert_equal(@client.testI64(val), val)
    assert_equal(@client.testI64(-val), -val)
  end

  def test_double
    val = 3.14
    assert_equal(@client.testDouble(val), val)
    assert_equal(@client.testDouble(-val), -val)
    assert_kind_of(Float, @client.testDouble(val))
  end

  def test_map
    val = {1 => 1, 2 => 2, 3 => 3}
    assert_equal(@client.testMap(val), val)
    assert_kind_of(Hash, @client.testMap(val))
  end

  def test_list
    val = [1,2,3,4,5]
    assert_equal(@client.testList(val), val)
    assert_kind_of(Array, @client.testList(val))
  end

  def test_enum
    val = Thrift::Test::Numberz::SIX
    ret = @client.testEnum(val)

    assert_equal(ret, 6)
    assert_kind_of(Fixnum, ret)
  end

  def test_typedef
    #UserId  testTypedef(1: UserId thing),
    assert_equal(@client.testTypedef(309858235082523), 309858235082523)
    assert_kind_of(Fixnum, @client.testTypedef(309858235082523))
    true
  end

  def test_set
    val = Set.new([1,2,3])
    assert_equal(@client.testSet(val), val)
    assert_kind_of(Set, @client.testSet(val))
  end

  def get_struct
    Thrift::Test::Xtruct.new({'string_thing' => 'hi!', 'i32_thing' => 4 })
  end

  def test_struct
    ret = @client.testStruct(get_struct)

    assert_nil(ret.byte_thing, nil)
    assert_nil(ret.i64_thing, nil)
    assert_equal(ret.string_thing, 'hi!')
    assert_equal(ret.i32_thing, 4)
    assert_kind_of(Thrift::Test::Xtruct, ret)
  end

  def test_nest
    struct2 = Thrift::Test::Xtruct2.new({'struct_thing' => get_struct, 'i32_thing' => 10})

    ret = @client.testNest(struct2)

    assert_nil(ret.struct_thing.byte_thing, nil)
    assert_nil(ret.struct_thing.i64_thing, nil)
    assert_equal(ret.struct_thing.string_thing, 'hi!')
    assert_equal(ret.struct_thing.i32_thing, 4)
    assert_equal(ret.i32_thing, 10)

    assert_kind_of(Thrift::Test::Xtruct, ret.struct_thing)
    assert_kind_of(Thrift::Test::Xtruct2, ret)
  end

  def test_insanity
    insane = Thrift::Test::Insanity.new({
      'userMap' => { Thrift::Test::Numberz::ONE => 44 },
      'xtructs' => [get_struct,
        Thrift::Test::Xtruct.new({
          'string_thing' => 'hi again',
          'i32_thing' => 12
        })
      ]
    })

    ret = @client.testInsanity(insane)

    assert_not_nil(ret[44])
    assert_not_nil(ret[44][1])

    struct = ret[44][1]

    assert_equal(struct.userMap[Thrift::Test::Numberz::ONE], 44)
    assert_equal(struct.xtructs[1].string_thing, 'hi again')
    assert_equal(struct.xtructs[1].i32_thing, 12)

    assert_kind_of(Hash, struct.userMap)
    assert_kind_of(Array, struct.xtructs)
    assert_kind_of(Thrift::Test::Insanity, struct)
  end

  def test_map_map
    ret = @client.testMapMap(4)
    assert_kind_of(Hash, ret)
    assert_equal(ret, { 4 => { 4 => 4}})
  end

  def test_exception
    assert_raise Thrift::Test::Xception do
      @client.testException('Xception')
    end
#    assert_raise Thrift::TException do
#      @client.testException('TException')
#    end
    assert_equal( @client.testException('test'), "test")
  end

  def test_multi_exception
    assert_raise Thrift::Test::Xception do
      @client.testMultiException("Xception", "test 1")
    end
    assert_raise Thrift::Test::Xception2 do
      @client.testMultiException("Xception2", "test 2")
    end
    assert_equal( @client.testMultiException("Success", "test 3").string_thing, "test 3")
  end

  def test_oneway
    time1 = Time.now.to_f
    @client.testOneway(3)
    time2 = Time.now.to_f
    assert_equal((time2-time1)*1000000<400, true)
  end

end

