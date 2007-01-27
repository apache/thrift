require 'thrift/transport/ttransport'
require 'socket'

class TSocket < TTransport
  def initialize(host, port)
    @host = host
    @port = port
    @handle = nil
  end

  def open()
    @handle = TCPSocket.new(@host, @port)
  end

  def isOpen()
    return @handle != nil
  end
  
  def write(str)
    @handle.write(str)
  end

  def read(sz)
    return @handle.recv(sz)
  end

  def close()
    @handle.close() unless @handle.nil?
  end
    
end

class TServerSocket < TServerTransport
  def initialize(port)
    @port = port
    @handle = nil
  end

  def listen()
    @handle = TCPserver.new(nil, @port)
  end

  def accept()
    if (@handle != nil)
      return @handle.accept()
    end
    return nil
  end
   
  def close()
   @handle.close() unless @handle.nil?
  end

end
