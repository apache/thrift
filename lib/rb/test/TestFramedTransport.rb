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


class TestFramedTransport < Test::Unit::TestCase
   
  def test_open_and_close
    begin
      port = ARGV[0] || 9090
      pid = Process.fork
      if pid.nil?then
        buf = String.new('1234567')
        transport = Thrift::ServerSocket.new(port)
        transport.listen()
        client = transport.accept()
        buffertransport = Thrift::FramedTransport.new(client)
        exit(0)
      else
        sleep(1)
        socket = Thrift::Socket.new('localhost',port) 
        transport = Thrift::FramedTransport.new(socket)
        transport.open()
        assert(transport.open?)
        transport.close()
        assert(!transport.open?)
        Process.wait(pid)
      end
    end
  end
 
  def test_read_and_write
    begin
      port = ARGV[0] || 9090
      pid = Process.fork
      if pid.nil?then
        buf = String.new('1234567')
        transport = Thrift::ServerSocket.new(port)
        transport.listen()
        client = transport.accept()
        buffertransport = Thrift::FramedTransport.new(client)
        buffertransport.read_into_buffer(buf2,7)
        assert_equal(buf2,buf1)
        exit(0)
      else
        sleep(1)
        socket = Thrift::Socket.new('localhost',port) 
        transport = Thrift::FramedTransport.new(socket)
        transport.open()
        buf1='hello123!@#$'
        transport.write(buf1)
        transport.close()
        Process.wait(pid)
      end
    end
  end
end
