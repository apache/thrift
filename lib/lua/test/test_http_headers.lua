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

-- THttpTransport accumulates the header block four bytes at a time until it
-- sees the blank line that ends it. A peer that never sends one decides how
-- much is accumulated, and how often the block is rescanned looking for it.
--
--   lua lib/lua/test/test_http_headers.lua

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' .. package.path

-- The header handling under test is pure Lua. The C extensions only do byte
-- packing, which Lua 5.3+ provides natively, so functional pure-Lua stand-ins
-- let the test run without building them.
package.preload['libluabitwise'] = function()
  return {
    bor    = function(a, b) return a | b end,
    band   = function(a, b) return a & b end,
    bxor   = function(a, b) return a ~ b end,
    shiftl = function(a, n) return (a << n) & 0xFFFFFFFF end,
    shiftr = function(a, n) return a >> n end,
  }
end
package.preload['libluabpack'] = function()
  local fmt = {c = '>i1', C = '>I1', s = '>i2', S = '>I2',
               i = '>i4', I = '>I4', l = '>i8', d = '>d'}
  return {
    bpack   = function(code, val) return string.pack(fmt[code], val) end,
    bunpack = function(code, data) return (string.unpack(fmt[code], data)) end,
  }
end
package.preload['liblualongnumber'] = function()
  return {new = function(_, v) return v or 0 end, tonumber = function(v) return v end}
end

require('Thrift')
require('TTransport')
require('TMemoryBuffer')
require('THttpTransport')

local failures = 0

local function check(condition, message)
  if condition then
    print('ok - ' .. message)
  else
    failures = failures + 1
    print('not ok - ' .. message)
  end
end

local function transportFor(wire)
  local buffer = TMemoryBuffer:new{}
  buffer:resetBuffer(wire)
  return THttpTransport:new{trans = buffer, isServer = true}
end

-- An ordinary request is still read.
do
  local wire = 'POST / HTTP/1.1\r\n' ..
               'X-Filler: ' .. string.rep('A', 4000) .. '\r\n' ..
               'Content-Length: 5\r\n\r\nhello'
  local http = transportFor(wire)
  local ok, body = pcall(function() return http:read(5) end)
  check(ok and body == 'hello',
        'a request with a large but bounded header block is read')
end

-- A header block that never ends is refused rather than accumulated.
do
  local wire = 'POST / HTTP/1.1\r\nX-Filler: ' .. string.rep('A', 200000)
  local http = transportFor(wire)
  http.maxHeaderSize = 16 * 1024
  local ok, err = pcall(function() return http:read(5) end)
  check(not ok, 'headers without an end are refused')
  check(ok or tostring(err):find('maximum', 1, true) ~= nil,
        'the refusal names the maximum: ' .. tostring(err))
end

-- A peer that stops sending does not leave the loop spinning.
do
  local http = transportFor('POST / HTTP/1.1\r\nX-Filler: short')
  local ok = pcall(function() return http:read(5) end)
  check(not ok, 'a truncated header block is refused')
end

if failures > 0 then
  print(failures .. ' failure(s)')
  os.exit(1)
end
print('all ok')
