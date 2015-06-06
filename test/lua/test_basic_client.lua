-- Licensed to the Apache Software Foundation (ASF) under one                                                                                                                                                                         
-- or more contributor license agreements. See the NOTICE file                                                                                                                                                                        
-- distributed with this work for additional information                                                                                                                                                                              
-- regarding copyright ownership. The ASF licenses this file                                                                                                                                                                          
-- to you under the Apache License, Version 2.0 (the                                                                                                                                                                                  
-- "License"); you may not use this file except in compliance                                                                                                                                                                         
-- with the License. You may obtain a copy of the License at                                                                                                                                                                          
                                                                                                                                                                                                                                   
--   http://www.apache.org/licenses/LICENSE-2.0                                                                                                                                                                                       
                                                                                                                                                                                                                                   
-- Unless required by applicable law or agreed to in writing,                                                                                                                                                                         
-- software distributed under the License is distributed on an                                                                                                                                                                        
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY                                                                                                                                                                             
-- KIND, either express or implied. See the License for the                                                                                                                                                                           
-- specific language governing permissions and limitations                                                                                                                                                                            
-- under the License.                                                                                                                                                                                                                 


require('TSocket')
require('TBinaryProtocol')
require('ThriftTest_ThriftTest')
require('liblualongnumber')

local client

function teardown()
  if client then
    -- Shuts down the server
    client:testVoid()

    -- close the connection
    client:close()
  end
end

function assertEqual(val1, val2, msg)
  assert(val1 == val2, msg)
end

function testBasicClient()
  local socket = TSocket:new{
    port = 9090
  }
  assert(socket, 'Failed to create client socket')
  socket:setTimeout(5000)

  local protocol = TBinaryProtocol:new{
    trans = socket
  }
  assert(protocol, 'Failed to create binary protocol')

  client = ThriftTestClient:new{
    protocol = protocol
  }
  assert(client, 'Failed to create client')

  -- Open the socket
  local status, _ = pcall(socket.open, socket)
  assert(status, 'Failed to connect to server')

  -- String
  assertEqual(client:testString('lala'),  'lala',  'Failed testString')
  assertEqual(client:testString('wahoo'), 'wahoo', 'Failed testString')

  -- Byte
  assertEqual(client:testByte(0x01), 1,    'Failed testByte 1')
  assertEqual(client:testByte(0x40), 64,   'Failed testByte 2')
  assertEqual(client:testByte(0x7f), 127,  'Failed testByte 3')
  assertEqual(client:testByte(0x80), -128, 'Failed testByte 4')
  assertEqual(client:testByte(0xbf), -65,  'Failed testByte 5')
  assertEqual(client:testByte(0xff), -1,   'Failed testByte 6')
  assertEqual(client:testByte(128), -128,  'Failed testByte 7')
  assertEqual(client:testByte(255), -1,    'Failed testByte 8')

  -- I32
  assertEqual(client:testI32(0x00000001), 1,           'Failed testI32 1')
  assertEqual(client:testI32(0x40000000), 1073741824,  'Failed testI32 2')
  assertEqual(client:testI32(0x7fffffff), 2147483647,  'Failed testI32 3')
  assertEqual(client:testI32(0x80000000), -2147483648, 'Failed testI32 4')
  assertEqual(client:testI32(0xbfffffff), -1073741825, 'Failed testI32 5')
  assertEqual(client:testI32(0xffffffff), -1,          'Failed testI32 6')
  assertEqual(client:testI32(2147483648), -2147483648, 'Failed testI32 7')
  assertEqual(client:testI32(4294967295), -1,          'Failed testI32 8')

  -- I64 (lua only supports 16 decimal precision so larger numbers are
  -- initialized by their string value)
  local long = liblualongnumber.new
  assertEqual(client:testI64(long(0x0000000000000001)),
                   long(1),
                   'Failed testI64 1')
  assertEqual(client:testI64(long(0x4000000000000000)),
                   long(4611686018427387904),
                   'Failed testI64 2')
  assertEqual(client:testI64(long('0x7fffffffffffffff')),
                   long('9223372036854775807'),
                   'Failed testI64 3')
  assertEqual(client:testI64(long(0x8000000000000000)),
                   long(-9223372036854775808),
                   'Failed testI64 4')
  assertEqual(client:testI64(long('0xbfffffffffffffff')),
                   long('-4611686018427387905'),
                   'Failed testI64 5')
  assertEqual(client:testI64(long('0xffffffffffffffff')),
                   long(-1),
                   'Failed testI64 6')

  -- Double
  assertEqual(
      client:testDouble(1.23456789), 1.23456789, 'Failed testDouble 1')
  assertEqual(
      client:testDouble(0.123456789), 0.123456789, 'Failed testDouble 2')
  assertEqual(
      client:testDouble(0.123456789), 0.123456789, 'Failed testDouble 3')

  -- TODO testBinary() ...
	  
  -- Accuracy of 16 decimal digits (rounds)
  local a, b = 1.12345678906666663, 1.12345678906666661
  assertEqual(a, b)
  assertEqual(client:testDouble(a), b, 'Failed testDouble 5')

  -- Struct
  local a = {
    string_thing = 'Zero',
    byte_thing = 1,
    i32_thing = -3,
    i64_thing = long(-5)
  }

  -- TODO fix client struct equality
  --assertEqual(client:testStruct(a), a, 'Failed testStruct')

  -- Call the void function and end the test (handler stops server)
  client:testVoid()
end

testBasicClient()
teardown()