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

-- __TObject:new takes an optional initial table. Without one it must still
-- return a fresh object; tables that carry a __type (every class definition)
-- must be used as given, and any other argument is an error.
--
--   lua lib/lua/test/test_tobject_new.lua

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' .. package.path

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

local failures = 0

local function check(condition, message)
  if condition then
    print('ok - ' .. message)
  else
    failures = failures + 1
    print('not ok - ' .. message)
  end
end

local ok, obj = pcall(function() return __TObject:new() end)
check(ok and type(obj) == 'table', 'new() without an argument returns a table')
if ok then
  check(obj.__parent == __TObject, 'new() sets __parent')
end

local init = {a = 1}
ok, obj = pcall(function() return __TObject:new(init) end)
check(ok and obj == init and obj.a == 1, 'new{a=1} reuses the given table')

local typed = {__type = 'Typed', b = 2}
ok, obj = pcall(function() return __TObject:new(typed) end)
check(ok and obj == typed and obj.b == 2 and ttype(obj) == 'Typed',
      'new{} keeps a table that has a __type')

for _, bad in ipairs({'text', 42, true}) do
  ok = pcall(function() return __TObject:new(bad) end)
  check(not ok, 'new(' .. tostring(bad) .. ') raises an error')
end

if failures > 0 then
  os.exit(1)
end
