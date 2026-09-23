--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--
-- TSocket:write() hands its buffer to the socket handle's send(), which
-- answers 1 once all of it is sent, or nil and a message when it could not
-- send it. A send that fails has to reach the caller as a transport error,
-- the same way a failed receive does in TSocket:read().
--
--   lua lib/lua/test/test_socket_write.lua

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' .. package.path

-- Nothing here opens a socket: each TSocket gets a stand-in handle, so the C
-- socket module is never called and an empty table stands in for it.
package.preload['libluasocket'] = function()
  return {}
end
package.preload['libluabitwise'] = function()
  return {
    bor    = function(a, b) return a | b end,
    band   = function(a, b) return a & b end,
    bxor   = function(a, b) return a ~ b end,
    shiftl = function(a, n) return (a << n) & 0xFFFFFFFF end,
    shiftr = function(a, n) return a >> n end,
  }
end

require('Thrift')
require('TTransport')
require('TSocket')

local failures = 0

local function check(condition, message)
  if condition then
    print('ok - ' .. message)
  else
    failures = failures + 1
    print('not ok - ' .. message)
  end
end

-- Answers send() the way the C module does, and records what it was given.
local function handle_answering(...)
  local answer = table.pack(...)
  local handle = {sent = {}}
  function handle.send(self, _, data)
    self.sent[#self.sent + 1] = data
    return table.unpack(answer, 1, answer.n)
  end
  return handle
end

local data = string.rep('0123456789', 1000)

do
  local handle = handle_answering(1)
  local socket = TSocket:new{handle = handle}
  local ok, err = pcall(function() socket:write(data) end)
  check(ok, 'a write the socket sends returns normally: ' .. tostring(err))
  check(#handle.sent == 1 and handle.sent[1] == data,
        'the whole buffer is handed to send() once')
end

for _, message in ipairs({'Timeout', 'Connection Closed', 'Broken pipe'}) do
  local handle = handle_answering(nil, message)
  local socket = TSocket:new{handle = handle}
  local ok, err = pcall(function() socket:write(data) end)
  check(not ok and tostring(err):find('TTransportException', 1, true) ~= nil,
        'a send that fails with "' .. message .. '" raises a transport ' ..
        'error: ' .. tostring(err))
  check(#handle.sent == 1 and handle.sent[1] == data,
        '... after handing the whole buffer to send() once')
end

if failures > 0 then
  print(failures .. ' failure(s)')
  os.exit(1)
end
print('all ok')
