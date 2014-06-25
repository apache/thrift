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

class SimpleHandler
  [:testVoid, :testString, :testByte, :testI32, :testI64, :testDouble,
   :testStruct, :testMap, :testSet, :testList, :testNest,
   :testEnum, :testTypedef, :testMultiException].each do |meth|

    define_method(meth) do |thing|
      thing
    end

  end

  def testVoid()
  end

  def testInsanity(thing)
    num, uid = thing.userMap.find { true }
    return {uid => {num => thing}}
  end

  def testMapMap(thing)
    return {thing => {thing => thing}}
  end

  def testEnum(thing)
    return thing
  end

  def testTypedef(thing)
    return thing
  end

  def testException(thing)
    if thing == "Xception"
      raise Thrift::Test::Xception, :message => thing
    elsif thing == "TException"
      raise Thrift::Test::TException, :message => thing
    else
      return arg1
    end
  end

  def testMultiException(arg0, arg1)
    if arg0 == "Xception2"
      raise Thrift::Test::Xception2, :message => 'This is an Xception2'
    elsif arg0 == "Xception"
      raise Thrift::Test::Xception, :message => 'This is an Xception'
    else
      return arg1
    end
  end

end

protocol = "binary"
port = 9090
transport = "buffered"
@transportFactory = Thrift::BufferedTransportFactory.new
@protocolFactory = Thrift::BinaryProtocolFactory.new
ARGV.each do|a|
  if a == "--help"
    puts "Allowed options:"
    puts "\t -h [ --help ] \t produce help message"
    puts "\t--port arg (=9090) \t Port number to listen"
    puts "\t--protocol arg (=binary) \t protocol: binary, accel"
    puts "\t--transport arg (=buffered) transport: buffered, framed, http"
    exit
  elsif a.start_with?("--protocol")
    protocol = a.split("=")[1]
  elsif a.start_with?("--transport")
    transport = a.split("=")[1]
  elsif a.start_with?("--port")
    port = a.split("=")[1].to_i 
  end
end

if protocol == "binary"
  @protocolFactory = Thrift::BinaryProtocolFactory.new
elsif protocol == ""
  @protocolFactory = Thrift::BinaryProtocolFactory.new
elsif protocol == "compact"
  @protocolFactory = Thrift::CompactProtocolFactory.new
elsif protocol == "json"
  @protocolFactory = Thrift::JsonProtocolFactory.new
elsif protocol == "accel"
  @protocolFactory = Thrift::BinaryProtocolAcceleratedFactory.new
else
  raise 'Unknown protocol type'
end

if transport == "buffered"
  @transportFactory = Thrift::BufferedTransportFactory.new
elsif transport == ""
  @transportFactory = Thrift::BufferedTransportFactory.new
elsif transport == "framed"
  @transportFactory = Thrift::FramedTransportFactory.new
else
  raise 'Unknown transport type'
end

@handler   = SimpleHandler.new
@processor = Thrift::Test::ThriftTest::Processor.new(@handler)
@transport = Thrift::ServerSocket.new(port)
@server    = Thrift::ThreadedServer.new(@processor, @transport, @transportFactory, @protocolFactory)
@server.serve
