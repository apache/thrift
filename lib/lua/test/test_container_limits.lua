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
-- A list, set or map declares its element count up front, and the generated
-- code reads one element per declared element. These hold the declared count
-- to a maximum in the binary, compact and JSON protocols, before any element
-- is read.
--
--   lua lib/lua/test/test_container_limits.lua

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' .. package.path

-- Pure-Lua stand-ins for the C extensions, as in test_frame_and_string_limits.lua:
-- the code under test is pure Lua and the extensions only pack bytes.
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
    bpack   = function(code, val)
      -- luabpack.c stores 'c' through an int8_t, so the compact protocol's
      -- header and varint bytes above 127 wrap around.
      if code == 'c' then
        val = ((math.tointeger(val) + 128) % 256) - 128
      end
      return string.pack(fmt[code], val)
    end,
    bunpack = function(code, data) return (string.unpack(fmt[code], data)) end,
    -- As luabpack.c: the value as an unsigned 32-bit varint.
    toVarint32 = function(n)
      n = math.tointeger(n) & 0xFFFFFFFF
      local out = ''
      while n > 0x7F do
        out = out .. string.char((n & 0x7F) | 0x80)
        n = n >> 7
      end
      return out .. string.char(n)
    end,
  }
end
package.preload['liblualongnumber'] = function()
  return {new = function(_, v) return v or 0 end, tonumber = function(v) return v end}
end

require('Thrift')
require('TTransport')
require('TMemoryBuffer')
require('TBinaryProtocol')
require('TCompactProtocol')
require('TJsonProtocol')

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

local protocols = {
  {name = 'binary', class = TBinaryProtocol},
  {name = 'compact', class = TCompactProtocol},
  -- The JSON reader looks one character past a number to find its end.
  {name = 'JSON', class = TJSONProtocol, lookahead = 1},
}
local kinds = {'list', 'set', 'map'}

-- The container header alone, as the protocol writes it. With withElements, a
-- complete container follows: n one-byte elements (keys and values for a map),
-- so every declared element really is on the wire.
local function encode(class, kind, n, withElements)
  local buffer = TMemoryBuffer:new{}
  local out = class:new{trans = buffer}
  if kind == 'list' then
    out:writeListBegin(TType.BYTE, n)
  elseif kind == 'set' then
    out:writeSetBegin(TType.BYTE, n)
  else
    out:writeMapBegin(TType.BYTE, TType.BYTE, n)
  end
  if withElements then
    for i = 1, n do
      out:writeByte(i)
      if kind == 'map' then
        out:writeByte(i)
      end
    end
    if kind == 'list' then
      out:writeListEnd()
    elseif kind == 'set' then
      out:writeSetEnd()
    else
      out:writeMapEnd()
    end
  end
  return buffer:getBuffer()
end

-- Reads a container the way the generated code does: the header, then one
-- element per declared element. Answers how many elements it read.
local function read_container(proto, kind)
  local size
  if kind == 'list' then
    size = select(2, proto:readListBegin())
  elseif kind == 'set' then
    size = select(2, proto:readSetBegin())
  else
    size = select(3, proto:readMapBegin())
  end
  for _ = 1, size do
    proto:readByte()
    if kind == 'map' then
      proto:readByte()
    end
  end
  if kind == 'list' then
    proto:readListEnd()
  elseif kind == 'set' then
    proto:readSetEnd()
  else
    proto:readMapEnd()
  end
  return size
end

-- terror() raises the exception's text, so a refusal is recognised by it.
local function refused(err, size, limit)
  return tostring(err):find('Container size ' .. size .. ' exceeds maximum ' .. limit,
                            1, true) ~= nil
end

-- A complete container of n elements, read with the given maxContainerSize
-- (nil: the default applies).
local function try_read(p, kind, n, maxContainerSize)
  local inner = CountingTransport:new{data = encode(p.class, kind, n, true)}
  local proto = p.class:new{trans = inner, maxContainerSize = maxContainerSize}
  local ok, result = pcall(read_container, proto, kind)
  return ok, result, inner
end

-- Refused as over the given maximum, with nothing asked of the transport
-- beyond the header.
local function check_refused(p, kind, n, maxContainerSize, limit, what)
  local ok, result, inner = try_read(p, kind, n, maxContainerSize)
  local allowed = string.len(encode(p.class, kind, n, false)) + (p.lookahead or 0)
  check(not ok and refused(result, n, limit),
        what .. ' is refused (' ..
        (ok and ('accepted, ' .. result .. ' elements read') or tostring(result)) .. ')')
  check(inner.bytesRequested <= allowed,
        what .. ' is refused before any element is read (asked for ' ..
        inner.bytesRequested .. ' bytes, the header is ' .. allowed .. ')')
end

local function check_read(p, kind, n, maxContainerSize, what)
  local ok, result = try_read(p, kind, n, maxContainerSize)
  check(ok and result == n,
        what .. ' still reads (' .. (ok and (result .. ' elements') or tostring(result)) .. ')')
end

--------------------------------------------------------------------------
-- A maximum set on the protocol
--------------------------------------------------------------------------

for _, p in ipairs(protocols) do
  for _, kind in ipairs(kinds) do
    local label = p.name .. ' ' .. kind
    check_read(p, kind, 8, 8, 'a ' .. label .. ' at the maximum')
    check_refused(p, kind, 9, 8, 8, 'a complete ' .. label .. ' of one element over the maximum')
  end
end

--------------------------------------------------------------------------
-- The default, and switching the limit off
--------------------------------------------------------------------------

check(DEFAULT_MAX_CONTAINER_SIZE == 16384000,
      'the default container maximum is 16384000 (' ..
      tostring(DEFAULT_MAX_CONTAINER_SIZE) .. ')')

-- With no maxContainerSize set, DEFAULT_MAX_CONTAINER_SIZE applies. Lowered for
-- these reads, to keep the containers small.
local savedDefault = DEFAULT_MAX_CONTAINER_SIZE
DEFAULT_MAX_CONTAINER_SIZE = 8
for _, p in ipairs(protocols) do
  for _, kind in ipairs(kinds) do
    local label = p.name .. ' ' .. kind
    check_read(p, kind, 8, nil, 'a ' .. label .. ' at the default maximum')
    check_refused(p, kind, 9, nil, 8, 'a ' .. label .. ' over the default maximum')
    -- Zero or less switches the limit off, the default included.
    check_read(p, kind, 9, 0, 'a ' .. label .. ' with maxContainerSize = 0')
    check_read(p, kind, 9, -1, 'a ' .. label .. ' with maxContainerSize = -1')
  end
end
DEFAULT_MAX_CONTAINER_SIZE = savedDefault

-- At the real default, one element over is refused from the header alone, and
-- the header at the default passes the check.
local default = 16384000
for _, p in ipairs(protocols) do
  local header = encode(p.class, 'list', default + 1, false) .. ','
  local inner = CountingTransport:new{data = header}
  local proto = p.class:new{trans = inner}
  local ok, err = pcall(proto.readListBegin, proto)
  check(not ok and refused(err, default + 1, default),
        'a ' .. p.name .. ' list header declaring ' .. (default + 1) ..
        ' elements is refused (' .. (ok and 'accepted' or tostring(err)) .. ')')

  header = encode(p.class, 'list', default, false) .. ','
  proto = p.class:new{trans = CountingTransport:new{data = header}}
  local size
  ok, err = pcall(function()
    size = select(2, proto:readListBegin())
  end)
  check(ok and size == default,
        'a ' .. p.name .. ' list header declaring ' .. default ..
        ' elements passes (' .. (ok and tostring(size) or tostring(err)) .. ')')
end

--------------------------------------------------------------------------
-- Skipping an unknown field goes through the same check
--------------------------------------------------------------------------

for _, p in ipairs(protocols) do
  for _, kind in ipairs(kinds) do
    local ttype = ({list = TType.LIST, set = TType.SET, map = TType.MAP})[kind]
    local inner = CountingTransport:new{data = encode(p.class, kind, 9, true)}
    local proto = p.class:new{trans = inner, maxContainerSize = 8}
    local ok, err = pcall(proto.skip, proto, ttype)
    check(not ok and refused(err, 9, 8),
          'skipping a ' .. p.name .. ' ' .. kind .. ' over the maximum is refused (' ..
          (ok and 'skipped' or tostring(err)) .. ')')
  end
end

print(string.format('\n%d checks, %d failures', checks, failures))
os.exit(failures == 0 and 0 or 1)
