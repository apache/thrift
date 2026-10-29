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
-- TSimpleServer:serve() takes one connection after another until stop() is
-- called. TServerSocket:accept() raises a timeout when no connection arrives
-- within the socket's timeout (1000 ms unless set otherwise). That ends one
-- wait, not the server. Any other error from accept() still ends serve(),
-- with that error.
--
--   lua lib/lua/test/test_simple_server.lua

local script_dir = arg[0]:match('(.*[/\\])') or './'
package.path = script_dir .. '../?.lua;' .. package.path

-- Nothing here opens a socket: each TServerSocket gets a scripted handle, so
-- the C socket module is never called and an empty table stands in for it.
-- The other C extensions get pure-Lua stand-ins, as in test_http_headers.lua.
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
package.preload['libluabpack'] = function()
  local fmt = {c = '>i1', C = '>I1', s = '>i2', S = '>I2',
               i = '>i4', I = '>I4', l = '>i8', d = '>d'}
  return {
    bpack   = function(code, val) return string.pack(fmt[code], val) end,
    bunpack = function(code, data) return (string.unpack(fmt[code], data)) end,
  }
end

require('Thrift')
require('TTransport')
require('TSocket')
require('TServer')

local failures = 0

local function check(condition, message)
  if condition then
    print('ok - ' .. message)
  else
    failures = failures + 1
    print('not ok - ' .. message)
  end
end

-- A TServerSocket whose handle answers accept() from a script, the way the C
-- module does: 'client' is a connection, 'Timeout' is what it reports when
-- no connection arrives in time, and any other string is an error it
-- reports. listen() only installs the handle. Past the end of the script the
-- handle stops the server and reports an error, so that a serve() that keeps
-- going ends.
local function scriptedSocket(script, onAccept)
  local socket = TServerSocket:new{host = '127.0.0.1', port = 0}
  socket.accepts = 0
  socket.closes = 0
  local handle = {}
  function handle.accept()
    socket.accepts = socket.accepts + 1
    if onAccept then
      onAccept()
    end
    local step = script[socket.accepts]
    if step == 'client' then
      return {destroy = function() end}
    elseif step == nil then
      socket.server:stop()
      return nil, 'accept() called more often than the script expects'
    end
    return nil, step
  end
  function handle.destroy()
    socket.closes = socket.closes + 1
  end
  function socket:listen()
    self.handle = handle
  end
  return socket
end

-- A processor that counts its connections. Each one ends at once, as one
-- does when the client hangs up; onClient, if set, runs first.
local function countingProcessor()
  local processor = {handler = {}, clients = 0}
  function processor:process(iprot, oprot)
    self.clients = self.clients + 1
    if self.onClient then
      self.onClient()
    end
    terror(TTransportException:new{errorCode = TTransportException.END_OF_FILE})
  end
  return processor
end

local function serverFor(socket, processor)
  local server = TSimpleServer:new{
    processor = processor,
    serverTransport = socket,
  }
  socket.server = server
  return server
end

-- What TServerSocket:accept() raises for an error the C module reports.
local function acceptError(message)
  return select(2, pcall(terror, message))
end

do
  local socket = scriptedSocket({'Timeout', 'Timeout', 'client'})
  local processor = countingProcessor()
  local server = serverFor(socket, processor)
  processor.onClient = function() server:stop() end
  local ok, err = pcall(server.serve, server)
  check(ok, 'serve() keeps accepting across accept timeouts and returns ' ..
        'after stop(): ' .. tostring(err))
  check(socket.accepts == 3,
        '... after three accept() calls (' .. socket.accepts .. ')')
  check(processor.clients == 1,
        '... having served the connection that came after the timeouts (' ..
        processor.clients .. ')')
  check(socket.closes == 1,
        '... and closed the server socket (' .. socket.closes .. ')')
end

do
  local server
  local socket = scriptedSocket({'Timeout'}, function() server:stop() end)
  server = serverFor(socket, countingProcessor())
  local ok, err = pcall(server.serve, server)
  check(ok, 'a stop() that comes while accept() waits ends serve() at the ' ..
        'timeout: ' .. tostring(err))
  check(socket.accepts == 1,
        '... after one accept() call (' .. socket.accepts .. ')')
  check(socket.closes == 1,
        '... and closed the server socket (' .. socket.closes .. ')')
end

for _, script in ipairs({{'Too many open files'},
                         {'Timeout', 'Too many open files'}}) do
  local socket = scriptedSocket(script)
  local server = serverFor(socket, countingProcessor())
  local ok, err = pcall(server.serve, server)
  local wanted = acceptError('Too many open files')
  check(not ok and err == wanted,
        'accept() failing with "Too many open files" after ' .. (#script - 1) ..
        ' timeout(s) ends serve() with that error: ' .. tostring(err))
  check(socket.accepts == #script,
        '... after ' .. #script .. ' accept() call(s) (' .. socket.accepts ..
        ')')
end

if failures > 0 then
  print(failures .. ' failure(s)')
  os.exit(1)
end
print('all ok')
