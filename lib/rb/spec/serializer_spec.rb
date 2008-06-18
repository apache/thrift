require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/serializer'
require 'ThriftSpec_types'

class ThriftSerializerSpec < Spec::ExampleGroup
  include Thrift
  include SpecNamespace

  describe Serializer do
    it "should serialize structs to binary by default" do
      serializer = Serializer.new
      data = serializer.serialize(Hello.new(:greeting => "'Ello guv'nor!"))
      data.should == "\x0B\x00\x01\x00\x00\x00\x0E'Ello guv'nor!\x00"
    end

    it "should serialize structs to the given protocol" do
      protocol = mock("Protocol")
      protocol.should_receive(:write_struct_begin).with("SpecNamespace::Hello")
      protocol.should_receive(:write_field).with("greeting", Types::STRING, 1, "Good day")
      protocol.should_receive(:write_field_stop)
      protocol.should_receive(:write_struct_end)
      protocolFactory = mock("ProtocolFactory")
      protocolFactory.stub!(:get_protocol).and_return(protocol)
      serializer = Serializer.new(protocolFactory)
      serializer.serialize(Hello.new(:greeting => "Good day"))
    end
  end

  describe Deserializer do
    it "should deserialize structs from binary by default" do
      deserializer = Deserializer.new
      data = "\x0B\x00\x01\x00\x00\x00\x0E'Ello guv'nor!\x00"
      deserializer.deserialize(Hello.new, data).should == Hello.new(:greeting => "'Ello guv'nor!")
    end

    it "should deserialize structs from the given protocol" do
      protocol = mock("Protocol")
      protocol.should_receive(:read_struct_begin).and_return("SpecNamespace::Hello")
      protocol.should_receive(:read_field_begin).and_return(["greeting", Types::STRING, 1],
                                                            [nil, Types::STOP, 0])
      protocol.should_receive(:read_type).with(Types::STRING).and_return("Good day")
      protocol.should_receive(:read_field_end)
      protocol.should_receive(:read_struct_end)
      protocolFactory = mock("ProtocolFactory")
      protocolFactory.stub!(:get_protocol).and_return(protocol)
      deserializer = Deserializer.new(protocolFactory)
      deserializer.deserialize(Hello.new, "").should == Hello.new(:greeting => "Good day")
    end
  end
end
