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

require "rpc_RpcService"
require "TFramedTransport"
--require "TBinaryProtocol"
require "TCompactProtocol"
require "TSocket"
require('TServer')

-- Handler
TestHandler = RpcServiceIface:new{}

function TestHandler:funCall(argStruct, argByte, argI16, argI32, argI64,
  argDouble, argString, paramMapStrStr, paramMapI32Str, paramSetStr,
  paramSetI64, paramListStr, argBool)
  print(argStruct.argByte, argStruct.argI16,
    argStruct.argI32, argStruct.argI64,
    argStruct.argDouble, argStruct.argString)
  print(argByte, argI16, argI32, argI64, argDouble, argString)
  for k,v in pairs(paramMapStrStr) do
    print(k, v)
  end
  for k,v in pairs(paramMapI32Str) do
    print(k, v)
  end
  for _,v in pairs(paramSetStr) do
    print(v)
  end
  for _,v in pairs(paramListStr) do
    print(v)
  end
  return {"return 1 by FunCall.","return 2 by FunCall."}
end

-- Test
local server

function teardown()
  if server then
    server:close()
  end
end

function testBasicServer()
  -- Handler & Processor
  local handler = TestHandler:new{}
  assert(handler, 'Failed to create handler')
  local processor = RpcServiceProcessor:new{
    handler = handler
  }
  assert(processor, 'Failed to create processor')

  -- Server Socket
  local socket = TServerSocket:new{
    host="0.0.0.0",
    port = 9090
  }
  assert(socket, 'Failed to create server socket')

  -- Transport & Factory
  --local trans_factory = TFramedTransportFactory:new{}
  --assert(trans_factory, 'Failed to create framed transport factory')
  local prot_factory = TCompactProtocolFactory:new{}
  --local prot_factory = TBinaryProtocolFactory:new{}
  assert(prot_factory, 'Failed to create compact protocol factory')

  -- Simple Server
  server = TSimpleServer:new{
    processor = processor,
    serverTransport = socket,
    protocolFactory = prot_factory
  }
  assert(server, 'Failed to create server')

  -- Serve
  server:serve()
  server = nil
end

testBasicServer()
teardown()