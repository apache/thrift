#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

require File.expand_path("#{File.dirname(__FILE__)}/spec_helper")

class ThriftSerializerSpec < RSpec::Core::ExampleGroup
  include Thrift
  include SpecNamespace

  describe Serializer do
    it "should serialize structs to binary by default" do
      serializer = Serializer.new(Thrift::BinaryProtocolAcceleratedFactory.new)
      data = serializer.serialize(Hello.new(:greeting => "'Ello guv'nor!"))
      data.should == "\x0B\x00\x01\x00\x00\x00\x0E'Ello guv'nor!\x00"
    end

    it "should serialize structs to the given protocol" do
      protocol = BaseProtocol.new(mock("transport"))
      protocol.should_receive(:write_struct_begin).with("SpecNamespace::Hello")
      protocol.should_receive(:write_field_begin).with("greeting", Types::STRING, 1)
      protocol.should_receive(:write_string).with("Good day")
      protocol.should_receive(:write_field_end)
      protocol.should_receive(:write_field_stop)
      protocol.should_receive(:write_struct_end)
      protocol_factory = mock("ProtocolFactory")
      protocol_factory.stub!(:get_protocol).and_return(protocol)
      serializer = Serializer.new(protocol_factory)
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
      protocol = BaseProtocol.new(mock("transport"))
      protocol.should_receive(:read_struct_begin).and_return("SpecNamespace::Hello")
      protocol.should_receive(:read_field_begin).and_return(["greeting", Types::STRING, 1],
                                                            [nil, Types::STOP, 0])
      protocol.should_receive(:read_string).and_return("Good day")
      protocol.should_receive(:read_field_end)
      protocol.should_receive(:read_struct_end)
      protocol_factory = mock("ProtocolFactory")
      protocol_factory.stub!(:get_protocol).and_return(protocol)
      deserializer = Deserializer.new(protocol_factory)
      deserializer.deserialize(Hello.new, "").should == Hello.new(:greeting => "Good day")
    end
  end
end
