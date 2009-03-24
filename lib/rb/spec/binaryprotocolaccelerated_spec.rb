require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/protocol/binaryprotocolaccelerated'
require File.dirname(__FILE__) + '/binaryprotocol_spec_shared'
require File.dirname(__FILE__) + '/gen-rb/ThriftSpec_types'

class ThriftBinaryProtocolAcceleratedSpec < Spec::ExampleGroup
  include Thrift

  describe Thrift::BinaryProtocolAccelerated do
    # since BinaryProtocolAccelerated should be directly equivalent to 
    # BinaryProtocol, we don't need any custom specs!
    it_should_behave_like 'a binary protocol'

    def protocol_class
      BinaryProtocolAccelerated
    end
  end

  describe BinaryProtocolAcceleratedFactory do
    it "should create a BinaryProtocolAccelerated" do
      BinaryProtocolAcceleratedFactory.new.get_protocol(mock("MockTransport")).should be_instance_of(BinaryProtocolAccelerated)
    end
  end
end
