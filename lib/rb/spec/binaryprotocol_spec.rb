require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/protocol/binaryprotocol'
require File.dirname(__FILE__) + '/binaryprotocol_spec_shared'

class ThriftBinaryProtocolSpec < Spec::ExampleGroup
  include Thrift

  describe BinaryProtocol do
    it_should_behave_like 'a binary protocol'

    def protocol_class
      BinaryProtocol
    end

    it "should read a message header" do
      @trans.should_receive(:read_all).exactly(2).times.and_return(
        [protocol_class.const_get(:VERSION_1) | Thrift::MessageTypes::REPLY].pack('N'),
        [42].pack('N')
      )      
      @prot.should_receive(:read_string).and_return('testMessage')
      @prot.read_message_begin.should == ['testMessage', Thrift::MessageTypes::REPLY, 42]
    end

    it "should raise an exception if the message header has the wrong version" do
      @prot.should_receive(:read_i32).and_return(-1)
      lambda { @prot.read_message_begin }.should raise_error(Thrift::ProtocolException, 'Missing version identifier') do |e|
        e.type == Thrift::ProtocolException::BAD_VERSION
      end
    end

    it "should raise an exception if the message header does not exist and strict_read is enabled" do
      @prot.should_receive(:read_i32).and_return(42)
      @prot.should_receive(:strict_read).and_return(true)
      lambda { @prot.read_message_begin }.should raise_error(Thrift::ProtocolException, 'No version identifier, old protocol client?') do |e|        
        e.type == Thrift::ProtocolException::BAD_VERSION
      end
    end
  end

  describe BinaryProtocolFactory do
    it "should create a BinaryProtocol" do
      BinaryProtocolFactory.new.get_protocol(mock("MockTransport")).should be_instance_of(BinaryProtocol)
    end
  end
end
