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

require 'TTransport'

THttpTransport = TTransportBase:new{
  __type = 'THttpTransport',
  path = '/',
  wBuf = '',
  rBuf = '',
  CRLF = '\r\n',
  VERSION = version,
  isServer = true,
  -- The largest header block that will be accumulated, in bytes. The block
  -- grows because the peer has not sent the blank line that ends it yet, so
  -- how far it grows is the peer's choice unless something bounds it.
  maxHeaderSize = 64 * 1024
}

function THttpTransport:new(obj)
  if ttype(obj) ~= 'table' then
    error(ttype(self) .. 'must be initialized with a table')
  end

  -- Ensure a transport is provided
  if not obj.trans then
    error('You must provide ' .. ttype(self) .. ' with a trans')
  end

  return TTransportBase.new(self, obj)
end

local function THttpHeaders()
    local data = {}
    return setmetatable({}, {
        __index = function(_, key) return data[string.lower(key)] end,
        __newindex = function(_, key, value) data[string.lower(key)] = value end,
        __pairs = function() return pairs(data) end
    })
end

function THttpTransport:isOpen()
  return self.trans:isOpen()
end

function THttpTransport:open()
  return self.trans:open()
end

function THttpTransport:close()
  return self.trans:close()
end

function THttpTransport:readAll(len)
  return self:read(len)
end

function THttpTransport:read(len)
  if string.len(self.rBuf) == 0 then
    self:_readMsg()
  end
  if len > string.len(self.rBuf) then
    local val = self.rBuf
    self.rBuf = ''
    return val
  end

  local val = string.sub(self.rBuf, 0, len)
  self.rBuf = string.sub(self.rBuf, len+1)
  return val
end

function THttpTransport:_readMsg()
  local delimiter = self.CRLF .. self.CRLF
  while true do
    local had = string.len(self.rBuf)
    self.rBuf = self.rBuf .. self.trans:read(4)
    local have = string.len(self.rBuf)
    if have == had then
      error('THttpTransport: no end of headers before the peer stopped sending')
    end
    if have > self.maxHeaderSize then
      error('THttpTransport: headers larger than the maximum of ' ..
            self.maxHeaderSize .. ' bytes')
    end
    -- Only the tail can hold a delimiter that was not there last time round,
    -- so do not rescan the whole block on every pass.
    local from = had - string.len(delimiter) + 2
    if from < 1 then
      from = 1
    end
    if string.find(self.rBuf, delimiter, from, true) then
      break
    end
  end
  if not self.rBuf then
    self.rBuf = ""
    return
  end
  self:getLine()
  local headers = self:_parseHeaders()
  if not headers then
    self.rBuf = ""
    return
  end

  local length = tonumber(headers["Content-Length"])
  if length then
    length = length - string.len(self.rBuf)
    self.rBuf = self.rBuf .. self.trans:readAll(length)
  end
  if self.rBuf == nil then
    self.rBuf = ""
  end
end

function THttpTransport:getLine()
  local a,b = string.find(self.rBuf, self.CRLF)
  local line = ""
  if a and b then
    line = string.sub(self.rBuf, 0, a-1)
    self.rBuf = string.sub(self.rBuf, b+1)
  end
  return line
end

function THttpTransport:_parseHeaders()
  local headers = THttpHeaders()

  repeat
    local line = self:getLine()
    for key, val in string.gmatch(line, "([%w%-]+)%s*:%s*(.+)") do
      if headers[key] then
        local delimiter = ", "
        if string.lower(key) == "set-cookie" then
          delimiter = "; "
        end
        headers[key] = headers[key] .. delimiter .. tostring(val)
      else
        headers[key] = tostring(val)
      end
    end
  until string.find(line, "^%s*$")

  return headers
end

function THttpTransport:write(buf, len)
  if len and len < string.len(buf) then
    buf = string.sub(buf, 0, len)
  end
  self.wBuf = self.wBuf .. buf
end

function THttpTransport:writeHttpHeader(content_len)
  if self.isServer then
    local header =  "HTTP/1.1 200 OK" .. self.CRLF
      .. "Server: Thrift/" .. self.VERSION .. self.CRLF
      .. "Access-Control-Allow-Origin: *" .. self.CRLF
      .. "Content-Type: application/x-thrift" .. self.CRLF
      .. "Content-Length: " .. content_len .. self.CRLF
      .. "Connection: Keep-Alive" .. self.CRLF .. self.CRLF
    self.trans:write(header)
  else
    local header = "POST " .. self.path .. " HTTP/1.1" .. self.CRLF
      .. "Host: " .. self.trans.host .. self.CRLF
      .. "Content-Type: application/x-thrift" .. self.CRLF
      .. "Content-Length: " .. content_len .. self.CRLF
      .. "Accept: application/x-thrift " .. self.CRLF
      .. "User-Agent: Thrift/" .. self.VERSION .. " (Lua/THttpClient)"
      .. self.CRLF .. self.CRLF
    self.trans:write(header)
  end
end

function THttpTransport:flushOneway()
  self.wBuf = ''
  self:writeHttpHeader(0)
  self.trans:flush()
end

function THttpTransport:flush()
  -- If the write fails we still want wBuf to be clear
  local tmp = self.wBuf
  self.wBuf = ''
  local dataLen = string.len(tmp)
  self:writeHttpHeader(dataLen)
  if dataLen > 0 then
    self.trans:write(tmp)
  end
  self.trans:flush()
end

THttpTransportFactory = TTransportFactoryBase:new{
  __type = 'THttpTransportFactory'
}
function THttpTransportFactory:getTransport(trans)
  if not trans then
    terror(TProtocolException:new{
      message = 'Must supply a transport to ' .. ttype(self)
    })
  end
  return THttpTransport:new{trans = trans}
end
