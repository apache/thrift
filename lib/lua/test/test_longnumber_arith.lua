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

-- Division and modulo on the int64 userdata of liblualongnumber: a zero
-- divisor is a Lua error, and the one overflowing quotient (the smallest
-- int64 divided by -1) must not trap. Loads the built C module from .libs.

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.cpath = script_dir .. '../.libs/?.so;' .. package.cpath

local longnumber = require('liblualongnumber')

local failures = 0

local function check(condition, message)
  if condition then
    print('ok - ' .. message)
  else
    failures = failures + 1
    print('not ok - ' .. message)
  end
end

local function fails(fn)
  local ok, err = pcall(fn)
  return not ok, err
end

local function new(value)
  return longnumber.new(value)
end

local MIN = '-9223372036854775808'

check(tostring(new('7') / new('2')) == '3', '7 / 2 == 3')
check(tostring(new('-7') / new('2')) == '-3', '-7 / 2 == -3')
check(tostring(new('7') % new('4')) == '3', '7 % 4 == 3')
check(tostring(new('7') / new('-1')) == '-7', '7 / -1 == -7')
check(tostring(new('7') % new('-1')) == '0', '7 % -1 == 0')

local failed, err = fails(function() return new('1') / new('0') end)
check(failed and tostring(err):find('zero', 1, true), 'division by zero raises an error')
failed, err = fails(function() return new('1') % new('0') end)
check(failed and tostring(err):find('zero', 1, true), 'modulo by zero raises an error')
failed = fails(function() return new('1') / 0 end)
check(failed, 'division by the number 0 raises an error')

check(tostring(new(MIN) % new('-1')) == '0', 'MIN % -1 == 0')
check(tostring(new(MIN) / new('-1')) == MIN, 'MIN / -1 wraps to MIN')

if failures > 0 then
  print(failures .. ' check(s) failed')
  os.exit(1)
end
print('all checks passed')
