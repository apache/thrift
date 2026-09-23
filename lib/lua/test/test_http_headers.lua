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

--------------------------------------------------------------------------
-- Field name and value parsing
--------------------------------------------------------------------------

-- Parse a header block directly. _readMsg drops the request line before it
-- calls _parseHeaders, so the block passed here holds only header lines and
-- the blank line that ends them.
local function parseBlock(block)
  local http = THttpTransport:new{trans = TMemoryBuffer:new{}, isServer = true}
  http.rBuf = block
  http.rPos = 0
  return http:_parseHeaders()
end

-- A field name is matched case-insensitively; the value keeps its trailing
-- whitespace but not its leading whitespace; a value may contain colons; two
-- lines with the same name are joined. A line whose name is not a run of
-- name characters ([A-Za-z0-9-]) is not treated as that header. These match
-- what the previous parser accepted for every well-formed line, and drop only
-- malformed names it used to map onto a suffix of themselves.
for _, case in ipairs({
  {'Content-Length: 5\r\n\r\n',              'content-length', '5'},
  {'Host: localhost:8080\r\n\r\n',           'host',           'localhost:8080'},
  {'Key:val\r\n\r\n',                        'key',            'val'},
  {'Key :  val\r\n\r\n',                     'key',            'val'},
  {'X-Trailer: value  \r\n\r\n',             'x-trailer',      'value  '},
  {'MixedCase: v\r\n\r\n',                   'mixedcase',      'v'},
  {'multi: a: b: c\r\n\r\n',                 'multi',          'a: b: c'},
  {'lower: x\r\nLOWER: y\r\n\r\n',           'lower',          'x, y'},
  {'Set-Cookie: a=1\r\nSet-Cookie: b=2\r\n\r\n', 'set-cookie', 'a=1; b=2'},
}) do
  local h = parseBlock(case[1])
  check(h[case[2]] == case[3],
        'header ' .. string.format('%q', case[1]) .. ' -> ' ..
        case[2] .. '=' .. tostring(h[case[2]]))
end

-- Lines that carry no usable header are ignored.
for _, case in ipairs({
  {'NoColon here\r\n\r\n',   'no header without a colon'},
  {'Empty:\r\n\r\n',         'no header when nothing follows the colon'},
  {'weird key: v\r\n\r\n',   'a name with an interior space is not a header'},
  {'@Junk: v\r\n\r\n',       'a name led by a non-name character is not a header'},
  {'Under_score: v\r\n\r\n', 'a name with an underscore is not a header'},
}) do
  local h = parseBlock(case[1])
  local empty = true
  for _ in pairs(h) do empty = false end
  check(empty, case[2])
end

-- A well-formed request line and body still read through end to end.
do
  local wire = 'POST / HTTP/1.1\r\nHost: example\r\nContent-Length: 3\r\n\r\nabc'
  local http = transportFor(wire)
  local ok, body = pcall(function() return http:read(3) end)
  check(ok and body == 'abc', 'a request with several headers reads its body')
end

--------------------------------------------------------------------------
-- Parsing cost does not grow with the length of a single line
--------------------------------------------------------------------------

-- One long header line is parsed in about the time a well-formed line of the
-- same length takes. A parser that rescans the line for each starting
-- position would spend far longer on the line that never matches; the
-- well-formed line of equal length is the control that rules out the plain
-- cost of a long line.
local function timeParse(line)
  local block = line .. '\r\n\r\n'
  local best
  for _ = 1, 3 do
    local http = THttpTransport:new{trans = TMemoryBuffer:new{}, isServer = true}
    http.rBuf = block
    http.rPos = 0
    local started = os.clock()
    http:_parseHeaders()
    local elapsed = os.clock() - started
    if not best or elapsed < best then best = elapsed end
  end
  return math.max(best, 1e-6)
end

do
  local n = 60000
  local wellFormed = timeParse('X-Filler: ' .. string.rep('A', n))
  timeParse(string.rep('A', n))  -- warm up the worst-case shape
  local worst = timeParse(string.rep('A', n))
  check(worst < 50 * wellFormed,
        n .. '-byte line with no colon parses within 50x a well-formed line ' ..
        'of the same length (' .. string.format('%.1f', worst / wellFormed) ..
        'x)')
end

if failures > 0 then
  print(failures .. ' failure(s)')
  os.exit(1)
end
print('all ok')
