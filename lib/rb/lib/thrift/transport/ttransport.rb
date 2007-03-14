#!/usr/bin/env ruby
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#

require 'thrift/thrift'

class TTransportException < TException

  UNKNOWN = 0
  NOT_OPEN = 1
  ALREADY_OPEN = 2
  TIMED_OUT = 3
  END_OF_FILE = 4

  attr_reader :type

  def initialize(type=UNKNOWN, message=nil)
    super(message)
    @type = type
  end

end

class TTransport
  def isOpen(); nil; end
  
  def open(); nil; end
  
  def close(); nil; end
  
  def read(sz); nil; end
  
  def readAll(sz)
    buff = ''
    have = 0
    while (have < sz)
      chunk = read(sz - have)
      have += chunk.length
      buff += chunk
    end
    return buff
  end

  def write(buf); nil; end

  def flush(); nil; end
  
end

class TServerTransport
  def listen(); nil; end
  
  def accept(); nil; end

  def close(); nil; end

end

class TTransportFactory
  def getTransport(trans)
    return trans
  end
end
  
class TBufferedTransport < TTransport
  def initialize(transport)
    @transport = transport
    @wbuf = ''
  end
  
  def isOpen()
    return @transport.isOpen()
  end

  def open()
    @transport.open()
  end

  def close()
    @transport.close()
  end
  
  def read(sz)
    return @transport.read(sz)
  end
  
  def write(buf)
    @wbuf += buf
  end

  def flush()
    @transport.write(@wbuf)
    @transport.flush()
    @wbuf = ''
  end
end

class TBufferedTransportFactory < TTransportFactory
  def getTransport(transport)
    return TBufferedTransport.new(transport)
  end
end
