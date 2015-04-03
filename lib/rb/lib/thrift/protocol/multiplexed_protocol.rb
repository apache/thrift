require 'thrift/protocol/protocol_decorator'

module Thrift
  class MultiplexedProtocol < BaseProtocol

    include ProtocolDecorator

    def initialize(protocol, service_name)
      super(protocol)
      @service_name = service_name
    end

    def write_message_begin(name, type, seqid)
      case type
      when MessageTypes::CALL, MessageTypes::ONEWAY
        @protocol.write_message_begin("#{@service_name}:#{name}", type, seqid)
      else
        @protocol.write_message_begin(name, type, seqid)
      end 
    end
  end
end