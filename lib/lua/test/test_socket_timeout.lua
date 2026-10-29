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
-- A socket timeout is given in milliseconds, and a wait lasts that long also
-- when the timeout is shorter than a second or has a fraction of one:
-- accept() on a server socket waits that long for a connection, and
-- TSocket:open() keeps retrying a refused connection for that long.
--
-- Unlike the other scripts here, this one opens sockets on 127.0.0.1, with
-- the C socket module that make builds into ../.libs.
--
--   lua lib/lua/test/test_socket_timeout.lua

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' .. package.path
package.cpath = script_dir .. '../.libs/?.so;' .. package.cpath

require('Thrift')
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

-- Wall-clock seconds with a fraction. os.time() counts whole seconds, and
-- os.clock() counts processor time, which a blocked wait does not use.
local function now()
  local pipe = assert(io.popen('date +%s.%N'))
  local text = pipe:read('l')
  pipe:close()
  local seconds = tonumber(text)
  assert(seconds and text:find('%.%d'),
         'date +%s.%N gave no fraction of a second: ' .. tostring(text))
  return seconds
end

-- How long fn took, then what pcall(fn) returned.
local function timed(fn)
  local started = now()
  local ok, err = pcall(fn)
  return now() - started, ok, err
end

-- A server socket on 127.0.0.1, at a port the system picks, listening with
-- the given timeout, and that port. The timeout has to be set before listen().
-- getsockinfo() hands the port through a C short, so it can come back negative.
local function listening(timeout)
  local server = TServerSocket:new{host = '127.0.0.1', port = 0}
  server:setTimeout(timeout)
  server:listen()
  return server, math.floor(server:getSocketInfo().port % 65536)
end

-- accept() with nobody connecting waits out the timeout, then reports it.
for _, case in ipairs({{ms = 500, least = 0.3}, {ms = 1500, least = 1.3}}) do
  local server = listening(case.ms)
  local took, ok, err = timed(function() return server:accept() end)
  server:close()
  check(not ok and tostring(err):find('Timeout', 1, true) ~= nil,
        string.format('accept() with setTimeout(%d) and no client reports ' ..
                      'a timeout: %s', case.ms, tostring(err)))
  check(took >= case.least,
        string.format('... after waiting at least %.1f s (waited %.3f s)',
                      case.least, took))
end

-- open() retries a refused connection until its timeout has passed.
do
  local server, port = listening(1000)
  server:close() -- nothing listens at that port any more
  local client = TSocket:new{host = '127.0.0.1', port = port}
  client:setTimeout(500)
  local took, ok, err = timed(function() client:open() end)
  client:close()
  check(not ok and tostring(err):find('Could not connect', 1, true) ~= nil,
        'open() with setTimeout(500) to a port nobody listens at fails: ' ..
        tostring(err))
  check(took >= 0.3,
        string.format('... after retrying for at least 0.3 s (took %.3f s)',
                      took))
end

if failures > 0 then
  print(failures .. ' failure(s)')
  os.exit(1)
end
print('all ok')
