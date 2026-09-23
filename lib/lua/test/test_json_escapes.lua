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

-- TJsonProtocol writes string and binary values and reads them back. This
-- exercises the escaping of strings that hold quotes, backslashes, control
-- bytes and \u00XX escapes, and the base64 encoding of binary of every
-- length and byte value, over a memory buffer.
--
--   lua lib/lua/test/test_json_escapes.lua

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' .. package.path

-- The C extensions only do byte packing, which Lua 5.3+ provides natively, so
-- functional pure-Lua stand-ins let the test run without building them.
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
require('TProtocol')
require('TJsonProtocol')

local failures = 0

local function check(condition, message)
  if condition then
    print('ok - ' .. message)
  else
    failures = failures + 1
    print('not ok - ' .. message)
  end
end

local function encode(method, value)
  local buf = TMemoryBuffer:new{}
  local proto = TJSONProtocol:new{trans = buf}
  proto:resetContext()
  proto[method](proto, value)
  return buf:getBuffer()
end

local function decode(method, wire)
  local buf = TMemoryBuffer:new{}
  buf:resetBuffer(wire)
  local proto = TJSONProtocol:new{trans = buf}
  proto:resetContext()
  return proto[method](proto)
end

local function readFrom(method, wire)
  local buf = TMemoryBuffer:new{}
  buf:resetBuffer(wire)
  local proto = TJSONProtocol:new{trans = buf}
  proto:resetContext()
  return proto[method](proto)
end

local function hex(s)
  return (s:gsub('.', function(c) return string.format('%02x', string.byte(c)) end))
end

--------------------------------------------------------------------------
-- String values round-trip
--------------------------------------------------------------------------

local allBytes = {}
for i = 0, 255 do allBytes[#allBytes + 1] = string.char(i) end
allBytes = table.concat(allBytes)

local stringCases = {
  {'empty', ''},
  {'a backslash', 'a\\b'},
  {'a quote', 'say "hi"'},
  {'a backslash then a quote', '\\"'},
  {'a newline and a tab', 'line\nnext\tend'},
  {'a carriage return', 'a\rb'},
  {'every control byte 0x00-0x1f', (function()
     local t = {}
     for i = 0, 0x1f do t[#t + 1] = string.char(i) end
     return table.concat(t)
   end)()},
  {'printable ASCII', 'The quick brown fox: 0123456789!'},
  {'all 256 byte values', allBytes},
  {'a forward slash', 'a/b/c'},
}

for _, case in ipairs(stringCases) do
  local name, value = case[1], case[2]
  local ok, got = pcall(function() return decode('readString', encode('writeString', value)) end)
  check(ok and got == value,
        'string round-trips: ' .. name ..
        (ok and (got == value and '' or (' (got ' .. hex(got) .. ')'))
              or (' (error ' .. tostring(got) .. ')')))
end

--------------------------------------------------------------------------
-- Binary values round-trip for every length and byte value
--------------------------------------------------------------------------

-- A length that is not a multiple of three exercises the base64 tail; the
-- lengths here cover both remainders and lengths on either side of the block
-- boundary as well as a value holding every byte.
local function sample(n)
  local t = {}
  for i = 1, n do t[i] = string.char((i * 37 + 11) % 256) end
  return table.concat(t)
end

for _, n in ipairs({0, 1, 2, 3, 4, 5, 6, 7, 8, 255, 256, 257, 300}) do
  local value = sample(n)
  local ok, got = pcall(function() return decode('readBinary', encode('writeBinary', value)) end)
  check(ok and got == value,
        'binary of ' .. n .. ' bytes round-trips' ..
        (ok and (got == value and '' or (' (got ' .. #got .. ' bytes)'))
              or (' (error ' .. tostring(got) .. ')')))
end

do
  local ok, got = pcall(function() return decode('readBinary', encode('writeBinary', allBytes)) end)
  check(ok and got == allBytes, 'binary holding every byte value round-trips')
end

--------------------------------------------------------------------------
-- The base64 the writer emits is valid
--------------------------------------------------------------------------

-- Base64 uses four output characters per three input bytes, so a value of n
-- bytes must encode to a quoted string of ceil(n/3)*4 characters.
for _, n in ipairs({1, 2, 3, 4, 5, 300}) do
  local wire = encode('writeBinary', sample(n))
  local body = wire:match('^"(.*)"$')
  local expected = math.ceil(n / 3) * 4
  check(body ~= nil and #body == expected,
        'base64 of ' .. n .. ' bytes is ' .. expected .. ' characters (got ' ..
        (body and #body or 'nil') .. ')')
end

--------------------------------------------------------------------------
-- \u00XX escapes are read back as the byte they name
--------------------------------------------------------------------------

do
  local ok, got = pcall(function() return readFrom('readString', '"\\u0041\\u0000\\u00ff"') end)
  local want = 'A' .. string.char(0) .. string.char(0xff)
  check(ok and got == want, '\\u00XX escapes decode to their bytes' ..
        (ok and '' or (' (error ' .. tostring(got) .. ')')))
end

--------------------------------------------------------------------------
-- Malformed escapes are refused, not crashed on
--------------------------------------------------------------------------

-- The escape character after a backslash is compared, not used as a pattern,
-- so a name with pattern punctuation is rejected cleanly.
for _, case in ipairs({
  {'"\\("', 'a punctuation escape'},
  {'"\\."', 'a dot escape'},
  {'"\\z"', 'an unknown letter escape'},
}) do
  local ok, err = pcall(function() return readFrom('readString', case[1]) end)
  check(not ok and tostring(err):find('control char', 1, true) ~= nil,
        case[2] .. ' is refused with a protocol error (' .. tostring(err) .. ')')
end

-- A \u escape outside the \u00XX range is refused cleanly.
do
  local ok, err = pcall(function() return readFrom('readString', '"\\u1234"') end)
  check(not ok and tostring(err):find('Expected', 1, true) ~= nil,
        'a \\u escape above 0x00ff is refused with a protocol error (' ..
        tostring(err) .. ')')
end

--------------------------------------------------------------------------
-- The string size limit still applies to what is read
--------------------------------------------------------------------------

do
  local buf = TMemoryBuffer:new{}
  buf:resetBuffer('"' .. string.rep('A', 64) .. '"')
  local proto = TJSONProtocol:new{trans = buf, maxStringSize = 32}
  proto:resetContext()
  local ok, err = pcall(function() return proto:readString() end)
  check(not ok and tostring(err):find('32', 1, true) ~= nil,
        'a string longer than the configured maximum is refused (' .. tostring(err) .. ')')
end

if failures > 0 then
  print(failures .. ' failure(s)')
  os.exit(1)
end
print('all ok')
