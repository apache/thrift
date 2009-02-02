require 'thrift/protocol/binaryprotocol'
require 'thrift_native'

=begin
The only change required for a transport to support TBinaryProtocolAccelerated is to implement 2 methods:
  * borrow(size), which takes an optional argument and returns atleast _size_ bytes from the transport, 
                  or the default buffer size if no argument is given
  * consume!(size), which removes size bytes from the front of the buffer

See TMemoryBuffer and TBufferedTransport for examples.
=end

module Thrift
  class BinaryProtocolAcceleratedFactory < ProtocolFactory
    def get_protocol(trans)
      BinaryProtocolAccelerated.new(trans)
    end
  end
end
