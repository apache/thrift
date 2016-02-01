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

function demoFunc()
    local socket = TSocket:new{
        host='127.0.0.1',
        port=9090
    }
    --local protocol = TBinaryProtocol:new{
    -- local protocol = TCompactProtocol:new{
        --trans = socket
    --}
    local protocol = TCompactProtocolFactory:getProtocol(socket)
    client = RpcServiceClient:new{
        protocol = protocol
    }
    local argStruct = ArgStruct:new{
      argByte = 53,
      argString = "str value",
      argI16 = 54,
      argI32 = 12,
      argI64 = 43,
      argDouble = 11.22,
      argBool = true
    }
    -- Open the socket  
    socket:open()
    pmap = {}
    pmap.name = "namess"
    pmap.pass = "vpass"
    pistrmap = {}
    pistrmap[10] = "val10"
    pistrmap[20] = "val20"
    ret = client:funCall(argStruct, 53, 54, 12, 34, 11.22, "login", pmap,
        pistrmap,
        {"ele1", "ele2", "ele3"},
        {11,22,33},
        {"l1.","l2."}, false);
    for k,v in pairs(ret)
    do
        print(k, v)
    end
end
demoFunc()
