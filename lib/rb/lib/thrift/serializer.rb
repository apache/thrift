module Thrift
  class Serializer
    def initialize(protocolFactory = BinaryProtocolFactory.new)
      @transport = MemoryBuffer.new
      @protocol = protocolFactory.get_protocol(@transport)
    end

    def serialize(base)
      @transport.reset_buffer
      base.write(@protocol)
      @transport.read(@transport.available)
    end
  end

  class Deserializer
    def initialize(protocolFactory = BinaryProtocolFactory.new)
      @transport = MemoryBuffer.new
      @protocol = protocolFactory.get_protocol(@transport)
    end

    def deserialize(base, buffer)
      @transport.reset_buffer(buffer)
      base.read(@protocol)
      base
    end
  end
end
