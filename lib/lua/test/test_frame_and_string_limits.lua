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
-- Frame lengths and string lengths arrive from the peer, and readAll() will
-- keep asking until it has that many bytes. These hold both to a maximum, and
-- check that readAll() builds its result in a way whose cost is linear in what
-- it reads rather than quadratic.
--
--   lua lib/lua/test/test_frame_and_string_limits.lua

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' .. package.path

-- Pure-Lua stand-ins for the C extensions, as in test_recursion_depth.lua: the
-- code under test is pure Lua and the extensions only pack bytes.
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
require('TFramedTransport')
require('TBinaryProtocol')
require('TMemoryBuffer')

local failures = 0
local checks = 0

local function check(ok, what)
  checks = checks + 1
  if ok then
    print('ok   - ' .. what)
  else
    failures = failures + 1
    print('FAIL - ' .. what)
  end
end

-- A transport that serves a fixed string and records how much was asked of it.
local CountingTransport = TTransportBase:new{__type = 'CountingTransport'}

function CountingTransport:new(obj)
  obj = obj or {}
  obj.pos = 1
  obj.bytesRequested = 0
  return __TObject.new(self, obj)
end

function CountingTransport:isOpen() return true end
function CountingTransport:open() end
function CountingTransport:close() end

function CountingTransport:read(len)
  self.bytesRequested = self.bytesRequested + len
  local chunk = string.sub(self.data, self.pos, self.pos + len - 1)
  self.pos = self.pos + string.len(chunk)
  return chunk
end

function CountingTransport:write(buf) end
function CountingTransport:flush() end

local function framed_over(declared, payload)
  local inner = CountingTransport:new{
    data = string.pack('>i4', declared) .. (payload or '')
  }
  return TFramedTransport:new{trans = inner}, inner
end

--------------------------------------------------------------------------
-- Frame size
--------------------------------------------------------------------------

local trans, inner = framed_over(0x7FFFFFFF)
local ok, err = pcall(function() return trans:read(1) end)
check(not ok, 'a frame declaring 2 GB is refused')
check(inner.bytesRequested == 4,
      'the declared size is never asked of the transport (asked for ' ..
      inner.bytesRequested .. ')')

trans, inner = framed_over(-1)
ok = pcall(function() return trans:read(1) end)
check(not ok, 'a negative frame size is refused')

trans, inner = framed_over(33, string.rep('x', 33))
trans.maxFrameSize = 32
ok = pcall(function() return trans:read(1) end)
check(not ok, 'a frame over a lowered maximum is refused')

local payload = 'hallo world'
trans, inner = framed_over(string.len(payload), payload)
ok, err = pcall(function() return trans:read(string.len(payload)) end)
check(ok and err == payload, 'a frame within the maximum still reads')

--------------------------------------------------------------------------
-- String length
--------------------------------------------------------------------------

local strInner = CountingTransport:new{data = string.pack('>i4', 0x7FFFFFFF)}
local proto = TBinaryProtocol:new{trans = strInner}
ok = pcall(function() return proto:readString() end)
check(not ok, 'a string declaring 2 GB is refused')
check(strInner.bytesRequested == 4,
      'the declared string length is never asked of the transport (asked for ' ..
      strInner.bytesRequested .. ')')

local text = 'a readable string'
strInner = CountingTransport:new{
  data = string.pack('>i4', string.len(text)) .. text
}
proto = TBinaryProtocol:new{trans = strInner}
ok, err = pcall(function() return proto:readString() end)
check(ok and err == text, 'a string within the maximum still reads')

--------------------------------------------------------------------------
-- readAll() cost
--------------------------------------------------------------------------

-- A transport that hands back one byte at a time, so readAll() takes n passes.
-- Concatenating onto an immutable string copies the whole accumulator each
-- pass, which is quadratic; collecting into a table and joining once is not.
-- Timed rather than asserted on implementation, so it holds for any linear one.
local DripTransport = TTransportBase:new{__type = 'DripTransport'}
function DripTransport:new(obj)
  obj = obj or {}
  obj.served = 0
  return __TObject.new(self, obj)
end
function DripTransport:isOpen() return true end
function DripTransport:read(len)
  if self.served >= self.total then return '' end
  self.served = self.served + 1
  return 'x'
end

local function time_read_all(n)
  local t = DripTransport:new{total = n}
  local started = os.clock()
  t:readAll(n)
  return os.clock() - started
end

time_read_all(2000)  -- warm up
local small = time_read_all(20000)
local large = time_read_all(80000)
-- Four times the input. Linear would be ~4x, quadratic ~16x. Ten is a wide
-- margin either side of that, so this is not a flaky timing assertion.
local ratio = large / math.max(small, 1e-6)
check(ratio < 10,
      'readAll cost grows about linearly with the bytes read (4x input took ' ..
      string.format('%.1f', ratio) .. 'x the time)')

--------------------------------------------------------------------------
-- HTTP body length
--------------------------------------------------------------------------

require('THttpTransport')

local function http_over(content_length)
  local head = 'POST / HTTP/1.1\r\nContent-Length: ' .. content_length ..
               '\r\n\r\n'
  local inner = CountingTransport:new{data = head}
  return THttpTransport:new{trans = inner, isServer = true}, inner
end

local httpTrans, httpInner = http_over(0x7FFFFFFF)
ok = pcall(function() return httpTrans:read(1) end)
check(not ok, 'an HTTP body declaring 2 GB is refused')
check(httpInner.bytesRequested < 1000,
      'the declared body length is never asked of the transport (asked for ' ..
      httpInner.bytesRequested .. ')')

print(string.format('\n%d checks, %d failures', checks, failures))
os.exit(failures == 0 and 0 or 1)
