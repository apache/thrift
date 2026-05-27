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

-- Drives the recursion-depth guard through the *generated* read()/write()
-- code over a recursive struct (RecTree), union (RecUnion) and exception
-- (RecError), not the protocol counter in isolation.
--
-- Run after generating the types next to this file:
--   thrift -o lib/lua/test --gen lua lib/lua/test/RecursionDepth.thrift
--   lua lib/lua/test/test_recursion_depth.lua

-- Locate the library (../) and the generated code (gen-lua/) relative to this
-- script so it can be run from anywhere.
local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' ..
               script_dir .. 'gen-lua/?.lua;' .. package.path

-- The recursion guard under test is pure Lua (the generated read/write and the
-- TProtocolBase counter). The C extensions only do byte packing, which Lua 5.3+
-- provides natively, so functional pure-Lua stand-ins let the test run without
-- building them while still exercising the real generated serialization path.
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
  -- i64 is never exercised here (the test types use i16/i32 only).
  return {new = function(_, v) return v or 0 end, tonumber = function(v) return v end}
end

require('Thrift')
require('TTransport')
require('TMemoryBuffer')
require('TProtocol')
require('TBinaryProtocol')
require('RecursionDepth_ttypes')

local LIMIT = DEFAULT_RECURSION_DEPTH

local passed, failed = 0, 0
local function ok(cond, name, detail)
  if cond then
    print('ok - ' .. name)
    passed = passed + 1
  else
    print('not ok - ' .. name .. (detail and ('  # ' .. tostring(detail)) or ''))
    failed = failed + 1
  end
end

local function new_proto()
  return TBinaryProtocol:new{trans = TMemoryBuffer:new{}}
end

-- Build a chain of `depth` nested nodes; each inner node carries a single
-- child, the deepest carries the scalar leaf -- a valid shape for both the
-- struct and the union.
local function make_chain(class, leaf_field, depth)
  local node = class:new{}
  if depth > 1 then
    node.children = {make_chain(class, leaf_field, depth - 1)}
  else
    node[leaf_field] = 1
  end
  return node
end

local function chain_depth(node)
  local depth = 1
  if node.children and #node.children > 0 then
    depth = depth + chain_depth(node.children[1])
  end
  return depth
end

-- Serialize an over-limit payload with raw protocol primitives so the reader
-- recurses through the guarded struct path (field id 1 = list<self>), not the
-- separate (unbounded) skip() path.
local function write_deep(oprot, depth)
  oprot:writeStructBegin('Rec')
  if depth > 1 then
    oprot:writeFieldBegin('children', TType.LIST, 1)
    oprot:writeListBegin(TType.STRUCT, 1)
    write_deep(oprot, depth - 1)
    oprot:writeListEnd()
    oprot:writeFieldEnd()
  end
  oprot:writeFieldStop()
  oprot:writeStructEnd()
end

-- True only for the depth-limit rejection -- an EOF/structural error would
-- carry a different type/message.
local function is_depth_error(err)
  return type(err) == 'string'
     and err:find('TProtocolException') ~= nil
     and err:find('recursion') ~= nil
end

local cases = {
  {kind = 'struct',    class = RecTree,  leaf = 'item'},
  {kind = 'union',     class = RecUnion, leaf = 'leaf'},
  {kind = 'exception', class = RecError, leaf = 'leaf'},
}

for _, case in ipairs(cases) do
  local kind, class, leaf = case.kind, case.class, case.leaf

  -- 1. A chain exactly at the limit round-trips. Also proves the guard does
  --    not double-count (a chain of 64 would be rejected at 32 otherwise).
  do
    local proto = new_proto()
    local chain = make_chain(class, leaf, LIMIT)
    local wok, werr = pcall(function() chain:write(proto) end)
    ok(wok, kind .. ': writing a chain at the depth limit succeeds', werr)

    local decoded = class:new{}
    local rok, rerr = pcall(function() decoded:read(proto) end)
    ok(rok, kind .. ': reading a chain at the depth limit succeeds', rerr)
    ok(chain_depth(decoded) == LIMIT,
       kind .. ': round-trips to the original depth (' .. LIMIT .. ')')
  end

  -- 2. Writing past the limit is rejected with a depth-limit error.
  do
    local proto = new_proto()
    local chain = make_chain(class, leaf, LIMIT + 5)
    local wok, werr = pcall(function() chain:write(proto) end)
    ok(not wok, kind .. ': writing past the limit throws')
    ok(is_depth_error(werr), kind .. ': ... with a recursion-depth error', werr)
  end

  -- 3. Reading an over-limit payload is rejected with a depth-limit error.
  do
    local proto = new_proto()
    write_deep(proto, LIMIT + 5)
    local decoded = class:new{}
    local rok, rerr = pcall(function() decoded:read(proto) end)
    ok(not rok, kind .. ': reading past the limit throws')
    ok(is_depth_error(rerr), kind .. ': ... with a recursion-depth error', rerr)
  end
end

print(string.format('\n%d passed, %d failed', passed, failed))
if failed > 0 then os.exit(1) end
