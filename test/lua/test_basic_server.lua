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

require('ThriftTest_ThriftTest')
require('TSocket')
require('TBufferedTransport')
require('TFramedTransport')
require('THttpTransport')
require('TCompactProtocol')
require('TJsonProtocol')
require('TBinaryProtocol')
require('TServer')
local liblualongnumber = require('liblualongnumber')

--------------------------------------------------------------------------------
-- Handler
TestHandler = ThriftTestIface:new{}

-- Stops the server
function TestHandler:testVoid()
end

function TestHandler:testString(str)
  return str
end

function TestHandler:testBool(bool)
  return bool
end

function TestHandler:testByte(byte)
  return byte
end

function TestHandler:testI32(i32)
  return i32
end

function TestHandler:testI64(i64)
  return i64
end

function TestHandler:testDouble(d)
  return d
end

function TestHandler:testBinary(by)
  return by
end

function TestHandler:testUuid(uuid)
  return uuid
end

function TestHandler:testNest(thing)
  return thing
end

function TestHandler:testStruct(thing)
  return thing
end

function TestHandler:testMap(thing)
  return thing
end

function TestHandler:testStringMap(thing)
  return thing
end

function TestHandler:testSet(thing)
  return thing
end

function TestHandler:testList(thing)
  return thing
end

function TestHandler:testEnum(thing)
  return thing
end

function TestHandler:testTypedef(thing)
  return thing
end

function TestHandler:testMapMap(hello)
  return {
    ["-4"] = {
      ["-4"] = -4,
      ["-3"] = -3,
      ["-2"] = -2,
      ["-1"] = -1
    },
    ["4"] = {
      ["1"] = 1,
      ["2"] = 2,
      ["3"] = 3,
      ["4"] = 4
    }
  }
end

function TestHandler:testInsanity(argument)
  local first_map = {
    [Numberz.TWO] = argument,
    [Numberz.THREE] = argument
  };
  local second_map = {
    [Numberz.SIX] = Insanity:new {
      userMap = {},
      xtructs = {}
    }
  }

  return {
    ["1"] = first_map,
    ["2"] = second_map
  };
end

function TestHandler:testMulti(arg0, arg1, arg2, arg3, arg4, arg5)
  return Xtruct:new {}
end

function TestHandler:testException(arg)
  if arg == "Xception" then
    return Xception:new {
      errorCode = 1001,
      message = arg
    }
  elseif arg == "TException" then
    error("")
  end
end

function TestHandler:testMultiException(arg0, arg1)
  if arg0 == "Xception" then
    return Xception:new {
      errorCode = 1001,
      message = "This is an Xception"
    }
  elseif arg0 == "Xception2" then
    return Xception2:new {
      errorCode = 2002,
      struct_thing = Xtruct:new {
        string_thing = "This is an Xception2"
      }
    }
  elseif arg0 == "TException" then
    error("")
  end
  return Xtruct:new {
    string_thing = arg1
  }
end

function TestHandler:testOneway(secondsToSleep)
  print("testOneway secondsToSleep:", secondsToSleep)
end

--------------------------------------------------------------------------------
-- Test
local server

function teardown()
  if server then
    server:close()
  end
end

function parseArgs(rawArgs)
  local opt = {
    protocol='binary',
    transport='buffered',
    port='9090',
  }
  for i, str in pairs(rawArgs) do
    if i > 0 then
      k, v = string.match(str, '--(%w+)=(%w+)')
      assert(opt[k] ~= nil, 'Unknown argument')
      opt[k] = v
    end
  end
  return opt
end

function testBasicServer(rawArgs)
  local opt = parseArgs(rawArgs)
  -- Handler & Processor
  local handler = TestHandler:new{}
  assert(handler, 'Failed to create handler')
  local processor = ThriftTestProcessor:new{
    handler = handler
  }
  assert(processor, 'Failed to create processor')

  -- Server Socket
  local socket = TServerSocket:new{
    port = opt.port
  }
  assert(socket, 'Failed to create server socket')

  -- Transport & Factory
  local transports = {
    buffered = TBufferedTransportFactory,
    framed = TFramedTransportFactory,
    http = THttpTransportFactory,
  }
  assert(transports[opt.transport], 'Failed to create framed transport factory')
  local trans_factory = transports[opt.transport]:new{}
  local protocols = {
    binary = TBinaryProtocolFactory,
    compact = TCompactProtocolFactory,
    json = TJSONProtocolFactory,
  }
  local prot_factory = protocols[opt.protocol]:new{}
  assert(prot_factory, 'Failed to create binary protocol factory')

  -- Simple Server
  server = TSimpleServer:new{
    processor = processor,
    serverTransport = socket,
    transportFactory = trans_factory,
    protocolFactory = prot_factory
  }
  assert(server, 'Failed to create server')
  server:setExceptionHandler(function (err) error(err) end)

  -- Serve
  server:serve()
  server = nil
end

testBasicServer(arg)
teardown()
