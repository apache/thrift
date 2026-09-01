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

require 'Thrift'

TTransportException = TException:new {
  UNKNOWN             = 0,
  NOT_OPEN            = 1,
  ALREADY_OPEN        = 2,
  TIMED_OUT           = 3,
  END_OF_FILE         = 4,
  INVALID_FRAME_SIZE  = 5,
  INVALID_TRANSFORM   = 6,
  INVALID_CLIENT_TYPE = 7,
  errorCode        = 0,
  __type = 'TTransportException'
}

function TTransportException:__errorCodeToString()
  if self.errorCode == self.NOT_OPEN then
    return 'Transport not open'
  elseif self.errorCode == self.ALREADY_OPEN then
    return 'Transport already open'
  elseif self.errorCode == self.TIMED_OUT then
    return 'Transport timed out'
  elseif self.errorCode == self.END_OF_FILE then
    return 'End of file'
  elseif self.errorCode == self.INVALID_FRAME_SIZE then
    return 'Invalid frame size'
  elseif self.errorCode == self.INVALID_TRANSFORM then
    return 'Invalid transform'
  elseif self.errorCode == self.INVALID_CLIENT_TYPE then
    return 'Invalid client type'
  else
    return 'Default (unknown)'
  end
end

-- The largest frame or field a transport will read by default. Same value the
-- other bindings use. Override per transport or per protocol instance.
DEFAULT_MAX_SIZE = 16384000

TTransportBase = __TObject:new{
  __type = 'TTransportBase',
  maxFrameSize = DEFAULT_MAX_SIZE
}

-- Refuses a length the peer declared before anything is read on account of it.
function TTransportBase:checkDeclaredSize(size, limit)
  if size < 0 then
    terror(TTransportException:new{
      message = 'Read a negative size (' .. tostring(size) .. ')',
      errorCode = TTransportException.INVALID_FRAME_SIZE
    })
  end
  if size > limit then
    terror(TTransportException:new{
      message = 'Size ' .. tostring(size) .. ' exceeds maximum ' .. tostring(limit),
      errorCode = TTransportException.INVALID_FRAME_SIZE
    })
  end
end

function TTransportBase:isOpen() end
function TTransportBase:open() end
function TTransportBase:close() end
function TTransportBase:read(len) end
function TTransportBase:readAll(len)
  -- Collect the chunks and join once. Appending to a Lua string builds a new
  -- one each time and copies everything read so far, so a peer that drips
  -- bytes made the cost quadratic in what it sent.
  local chunks, have, chunk = {}, 0
  while have < len do
    chunk = self:read(len - have)
    have = have + string.len(chunk)
    chunks[#chunks + 1] = chunk

    if string.len(chunk) == 0 then
      terror(TTransportException:new{
        errorCode = TTransportException.END_OF_FILE
      })
    end
  end
  return table.concat(chunks)
end
function TTransportBase:write(buf) end
-- flushOneway is a NOOP for most transport types.
function TTransportBase:flushOneway() end
function TTransportBase:flush() end

TServerTransportBase = __TObject:new{
  __type = 'TServerTransportBase'
}
function TServerTransportBase:listen() end
function TServerTransportBase:accept() end
function TServerTransportBase:close() end

TTransportFactoryBase = __TObject:new{
  __type = 'TTransportFactoryBase'
}
function TTransportFactoryBase:getTransport(trans)
  return trans
end
