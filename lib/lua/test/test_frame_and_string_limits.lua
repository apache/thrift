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
-- JSON string, number and base64 length
--------------------------------------------------------------------------

require('TJsonProtocol')

-- A JSON string or number carries no length: it ends at the closing quote,
-- or at the first character that cannot be part of the number. The string
-- maximum is applied while the value is read, so reading stops within a byte
-- or two of the maximum however long the value goes on.
local function check_refused(what, read, prefix, value, maxStringSize, limit)
  local jinner = CountingTransport:new{data = prefix .. value}
  local jproto = TJSONProtocol:new{trans = jinner, maxStringSize = maxStringSize}
  local ok, err = pcall(read, jproto)
  check(not ok and
        tostring(err):find('exceeds maximum ' .. limit, 1, true) ~= nil,
        what .. ' over the maximum is refused: ' .. tostring(err))
  local allowed = string.len(prefix) + limit + 2
  check(jinner.bytesRequested <= allowed,
        what .. ' is refused by the time ' .. allowed ..
        ' bytes are asked of the transport (asked for ' ..
        jinner.bytesRequested .. ')')
end

local long = string.rep('a', 64)
check_refused('a JSON string', TJSONProtocol.readString,
              '"', long .. '"', 32, 32)
check_refused('a JSON string with no closing quote', TJSONProtocol.readString,
              '"', string.rep('a', 100000), 32, 32)
check_refused('a JSON number', TJSONProtocol.readI32,
              '', string.rep('1', 64) .. ']', 32, 32)
check_refused('a JSON double', TJSONProtocol.readDouble,
              '', string.rep('1', 64) .. ']', 32, 32)
check_refused('a JSON base64 value', TJSONProtocol.readBinary,
              '"', string.rep('QUJD', 16) .. '"', 32, 32)
check_refused('a JSON message name', TJSONProtocol.readMessageBegin,
              '[1,"', long .. '",1,0]', 32, 32)

-- With no maxStringSize set, DEFAULT_MAX_STRING_SIZE applies, as it does for
-- the binary and compact protocols. Lowered for the one read, to keep it small.
check(DEFAULT_MAX_STRING_SIZE == 16384000,
      'the default string maximum is 16384000 (' ..
      tostring(DEFAULT_MAX_STRING_SIZE) .. ')')
local savedDefault = DEFAULT_MAX_STRING_SIZE
DEFAULT_MAX_STRING_SIZE = 32
check_refused('a JSON string under the default maximum',
              TJSONProtocol.readString, '"', long .. '"', nil, 32)
DEFAULT_MAX_STRING_SIZE = savedDefault

-- Values up to the maximum still read.
do
  local function read_with(read, data)
    local jproto = TJSONProtocol:new{
      trans = CountingTransport:new{data = data}, maxStringSize = 32
    }
    local ok, val = pcall(read, jproto)
    return ok and val
  end
  check(read_with(TJSONProtocol.readString, '"' .. string.rep('s', 32) .. '"')
          == string.rep('s', 32),
        'a JSON string at the maximum still reads')
  check(read_with(TJSONProtocol.readI32, string.rep('0', 30) .. '42]') == 42,
        'a JSON number at the maximum still reads')
  check(read_with(TJSONProtocol.readBinary, '"' .. string.rep('QUJD', 8) .. '"')
          == string.rep('ABC', 8),
        'a JSON base64 value at the maximum still reads')
end

-- A message written with TJSONProtocol reads back the same.
do
  local texts = {'', 'plain', 'with "quotes"', 'new\nline\ttab',
                 string.rep('0123456789', 500), 'utf-8 \xc3\xa4\xe2\x82\xac'}
  local buffer = TMemoryBuffer:new{}
  local out = TJSONProtocol:new{trans = buffer}
  out:writeMessageBegin('someMethod', TMessageType.CALL, 7)
  out:writeStructBegin('args')
  out:writeFieldBegin('texts', TType.LIST, 1)
  out:writeListBegin(TType.STRING, #texts)
  for _, t in ipairs(texts) do
    out:writeString(t)
  end
  out:writeListEnd()
  out:writeFieldEnd()
  out:writeFieldBegin('number', TType.I32, 2)
  out:writeI32(-123456)
  out:writeFieldEnd()
  out:writeFieldBegin('real', TType.DOUBLE, 3)
  out:writeDouble(3.25)
  out:writeFieldEnd()
  out:writeFieldBegin('bytes', TType.STRING, 4)
  out:writeBinary('a\0z')
  out:writeFieldEnd()
  out:writeFieldStop()
  out:writeStructEnd()
  out:writeMessageEnd()

  local jproto = TJSONProtocol:new{trans = buffer}
  local same = true
  local function expect(got, want)
    if got ~= want then
      same = false
    end
  end
  ok, err = pcall(function()
    local name, mtype, seqid = jproto:readMessageBegin()
    expect(name, 'someMethod')
    expect(mtype, TMessageType.CALL)
    expect(seqid, 7)
    jproto:readStructBegin()
    local _, ftype, fid = jproto:readFieldBegin()
    expect(ftype, TType.LIST)
    expect(fid, 1)
    local etype, count = jproto:readListBegin()
    expect(etype, TType.STRING)
    expect(count, #texts)
    for i = 1, count do
      expect(jproto:readString(), texts[i])
    end
    jproto:readListEnd()
    jproto:readFieldEnd()
    _, ftype, fid = jproto:readFieldBegin()
    expect(jproto:readI32(), -123456)
    jproto:readFieldEnd()
    _, ftype, fid = jproto:readFieldBegin()
    expect(jproto:readDouble(), 3.25)
    jproto:readFieldEnd()
    _, ftype, fid = jproto:readFieldBegin()
    expect(jproto:readBinary(), 'a\0z')
    jproto:readFieldEnd()
    _, ftype, fid = jproto:readFieldBegin()
    expect(ftype, TType.STOP)
    jproto:readStructEnd()
    jproto:readMessageEnd()
  end)
  check(ok and same, 'a JSON message reads back as written: ' .. tostring(err))
end

-- Base64 as other implementations write it: RFC 4648's test vectors, and all
-- 256 byte values.
do
  local allBytes = {}
  for i = 0, 255 do
    allBytes[#allBytes + 1] = string.char(i)
  end
  local vectors = {
    {'', ''}, {'f', 'Zg=='}, {'fo', 'Zm8='}, {'foo', 'Zm9v'},
    {'foob', 'Zm9vYg=='}, {'fooba', 'Zm9vYmE='}, {'foobar', 'Zm9vYmFy'},
    {table.concat(allBytes),
     'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEy' ..
     'MzQ1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2Rl' ..
     'ZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeY' ..
     'mZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrL' ..
     'zM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+' ..
     '/w=='}
  }
  local wrong = 0
  for _, v in ipairs(vectors) do
    local jproto = TJSONProtocol:new{
      trans = CountingTransport:new{data = '"' .. v[2] .. '"'}
    }
    local ok2, got = pcall(jproto.readBinary, jproto)
    if not ok2 or got ~= v[1] then
      wrong = wrong + 1
    end
  end
  check(wrong == 0, 'base64 values decode to the bytes they encode (' ..
        wrong .. ' of ' .. #vectors .. ' wrong)')
end

-- Reading a string, number or base64 value costs about the same per byte
-- whatever its length. Timed rather than asserted on implementation, as with
-- readAll() above, and the best of three runs to ride out noise. The
-- transport here costs next to nothing per byte, so the time is the
-- protocol's own.
local function lean_transport(data)
  local pos = 1
  return {
    readAll = function(_, n)
      local bytes = string.sub(data, pos, pos + n - 1)
      pos = pos + n
      return bytes
    end
  }
end

local function time_json_read(read, data)
  local jproto = TJSONProtocol:new{trans = lean_transport(data)}
  local started = os.clock()
  read(jproto)
  return os.clock() - started
end

-- Decoding takes most of the time of a base64 read, and would hide how the
-- decoded bytes are put together. Here readJSONString() and base64_decode()
-- are stood in for by functions that cost next to nothing, which leaves the
-- putting together to be timed.
local function time_base64_assembly(n)
  local text = string.rep('QUJD', n // 4)
  local jproto = TJSONProtocol:new{trans = lean_transport('')}
  jproto.readJSONString = function() return text end
  local decode = base64_decode
  base64_decode = function() return 'ABC' end
  local started = os.clock()
  local ok3, err3 = pcall(jproto.readBinary, jproto)
  local elapsed = os.clock() - started
  base64_decode = decode
  if not ok3 then
    error(err3, 0)
  end
  return elapsed
end

local function best_json_time(time_it, n)
  local best
  for _ = 1, 3 do
    local t = time_it(n)
    if not best or t < best then
      best = t
    end
  end
  return math.max(best, 1e-6)
end

-- Eight times the bytes. Linear is ~8x; twenty leaves wide room for noise.
for _, case in ipairs({
    {'reading a JSON string', 25000, function(n)
      return time_json_read(TJSONProtocol.readString,
                            '"' .. string.rep('a', n) .. '"')
    end},
    {'reading a JSON number', 25000, function(n)
      return time_json_read(TJSONProtocol.readI32, string.rep('1', n) .. ']')
    end},
    {'putting a decoded base64 value together', 100000,
     time_base64_assembly}}) do
  local what, n, time_it = case[1], case[2], case[3]
  time_it(1000)  -- warm up
  local jratio = best_json_time(time_it, 8 * n) / best_json_time(time_it, n)
  check(jratio < 20,
        what .. ' costs about linearly (8x the bytes took ' ..
        string.format('%.1f', jratio) .. 'x the time)')
end

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

--------------------------------------------------------------------------
-- Frame and HTTP body reads
--------------------------------------------------------------------------

-- Bytes that differ from one position to the next, so a byte handed out
-- twice, skipped or out of order fails the comparison.
local function sample(n, seed)
  local bytes = {}
  for i = 1, n do
    bytes[i] = string.char((i * 31 + seed * 7) % 256)
  end
  return table.concat(bytes)
end

local frameSizes = {1, 5, 300, 4096, 17, 70000, 3}
local frames, frameWire = {}, {}
for i, n in ipairs(frameSizes) do
  frames[i] = sample(n, i)
  frameWire[i] = string.pack('>i4', n) .. frames[i]
end

local bodySizes = {5000, 4, 70000, 777}
local bodies, httpWire = {}, {}
for i, n in ipairs(bodySizes) do
  bodies[i] = sample(n, 100 + i)
  httpWire[i] = 'POST / HTTP/1.1\r\nContent-Length: ' .. n .. '\r\n\r\n' ..
                bodies[i]
end

local readSizes = {1, 2, 3, 7, 64, 1000, 5000, 13}

-- read() returns at most what is left of the current frame or body, and
-- starts on the next one once that is used up. Replays readSizes against that
-- rule until every piece is consumed, comparing each result with the bytes
-- the rule says it must return.
local function check_reads(trans, pieces, what)
  local piece, pos, total, expectedTotal, reads, wrong = 1, 0, 0, 0, 0, 0
  for _, p in ipairs(pieces) do
    expectedTotal = expectedTotal + string.len(p)
  end
  while total < expectedTotal do
    reads = reads + 1
    local len = readSizes[(reads - 1) % #readSizes + 1]
    if pos == string.len(pieces[piece]) then
      piece, pos = piece + 1, 0
    end
    local want = string.sub(pieces[piece], pos + 1, pos + len)
    local ok, got = pcall(function() return trans:read(len) end)
    if not ok or got ~= want then
      wrong = wrong + 1
    end
    pos = pos + string.len(want)
    total = total + string.len(want)
  end
  check(wrong == 0,
        what .. ': ' .. reads .. ' reads of assorted sizes return the ' ..
        expectedTotal .. ' bytes in order (' .. wrong .. ' wrong)')
end

check_reads(TFramedTransport:new{
              trans = CountingTransport:new{data = table.concat(frameWire)}
            }, frames, 'frames')
check_reads(THttpTransport:new{
              trans = CountingTransport:new{data = table.concat(httpWire)},
              isServer = true
            }, bodies, 'HTTP bodies')

-- readAll() carries on into the next frame until it has what it asked for.
do
  local trans = TFramedTransport:new{
    trans = CountingTransport:new{data = table.concat(frameWire)}
  }
  local expected = table.concat(frames)
  local got, total, wrong, reads = {}, 0, 0, 0
  while total < string.len(expected) do
    reads = reads + 1
    local len = math.min(readSizes[(reads - 1) % #readSizes + 1] * 3,
                         string.len(expected) - total)
    local chunk = trans:readAll(len)
    if string.len(chunk) ~= len then
      wrong = wrong + 1
    end
    got[#got + 1] = chunk
    total = total + len
  end
  check(wrong == 0 and table.concat(got) == expected,
        'frames: ' .. reads .. ' readAll() calls across frame boundaries ' ..
        'return the ' .. string.len(expected) .. ' bytes in order')
end

-- A list skipped inside a frame leaves the protocol at the field after it.
do
  local buffer = TMemoryBuffer:new{}
  local out = TFramedTransport:new{trans = buffer}
  local proto = TBinaryProtocol:new{trans = out}
  proto:writeListBegin(TType.BYTE, 3000)
  for i = 1, 3000 do
    proto:writeByte(i % 100)
  end
  proto:writeListEnd()
  proto:writeI32(123456789)
  proto:writeString('after the list')
  out:flush()

  proto = TBinaryProtocol:new{trans = TFramedTransport:new{trans = buffer}}
  local marker, text
  ok = pcall(function()
    proto:skip(TType.LIST)
    marker = proto:readI32()
    text = proto:readString()
  end)
  check(ok and marker == 123456789 and text == 'after the list',
        'a list skipped inside a frame leaves the protocol at the next field')
end

-- One-byte reads cost about the same per byte whatever the size of the frame
-- or body they come from. Timed rather than asserted on implementation, as
-- with readAll() above, and the best of three runs to ride out noise.
local function best_of_three(time_it, n)
  local best
  for _ = 1, 3 do
    local t = time_it(n)
    if not best or t < best then
      best = t
    end
  end
  return math.max(best, 1e-6)
end

local function time_frame_reads(n)
  local trans = framed_over(n, string.rep('x', n))
  local started = os.clock()
  for _ = 1, n do
    trans:read(1)
  end
  return os.clock() - started
end

local function time_body_reads(n)
  local inner = CountingTransport:new{
    data = 'POST / HTTP/1.1\r\nContent-Length: ' .. n .. '\r\n\r\n' ..
           string.rep('x', n)
  }
  local trans = THttpTransport:new{trans = inner, isServer = true}
  local started = os.clock()
  for _ = 1, n do
    trans:read(1)
  end
  return os.clock() - started
end

-- Eight times the bytes. Linear is ~8x; twenty leaves wide room for noise.
for _, case in ipairs({{'a frame', time_frame_reads},
                       {'an HTTP body', time_body_reads}}) do
  case[2](2000)  -- warm up
  local small = best_of_three(case[2], 25000)
  local large = best_of_three(case[2], 200000)
  local ratio = large / small
  check(ratio < 20,
        'one-byte reads from ' .. case[1] .. ' cost about linearly (8x the ' ..
        'bytes took ' .. string.format('%.1f', ratio) .. 'x the time)')
end

print(string.format('\n%d checks, %d failures', checks, failures))
os.exit(failures == 0 and 0 or 1)
