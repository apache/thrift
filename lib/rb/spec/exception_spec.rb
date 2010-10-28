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

require File.dirname(__FILE__) + '/spec_helper'

class ThriftExceptionSpec < Spec::ExampleGroup
  include Thrift

  describe Exception do
    it "should have an accessible message" do
      e = Exception.new("test message")
      e.message.should == "test message"
    end
  end

  describe ApplicationException do
    it "should inherit from Thrift::Exception" do
      ApplicationException.superclass.should == Exception
    end

    it "should have an accessible type and message" do
      e = ApplicationException.new
      e.type.should == ApplicationException::UNKNOWN
      e.message.should be_nil
      e = ApplicationException.new(ApplicationException::UNKNOWN_METHOD, "test message")
      e.type.should == ApplicationException::UNKNOWN_METHOD
      e.message.should == "test message"
    end

    it "should read a struct off of a protocol" do
      prot = mock("MockProtocol")
      prot.should_receive(:read_struct_begin).ordered
      prot.should_receive(:read_field_begin).exactly(3).times.and_return(
        ["message", Types::STRING, 1],
        ["type", Types::I32, 2],
        [nil, Types::STOP, 0]
      )
      prot.should_receive(:read_string).ordered.and_return "test message"
      prot.should_receive(:read_i32).ordered.and_return ApplicationException::BAD_SEQUENCE_ID
      prot.should_receive(:read_field_end).exactly(2).times
      prot.should_receive(:read_struct_end).ordered

      e = ApplicationException.new
      e.read(prot)
      e.message.should == "test message"
      e.type.should == ApplicationException::BAD_SEQUENCE_ID
    end

    it "should skip bad fields when reading a struct" do
      prot = mock("MockProtocol")
      prot.should_receive(:read_struct_begin).ordered
      prot.should_receive(:read_field_begin).exactly(5).times.and_return(
        ["type", Types::I32, 2],
        ["type", Types::STRING, 2],
        ["message", Types::MAP, 1],
        ["message", Types::STRING, 3],
        [nil, Types::STOP, 0]
      )
      prot.should_receive(:read_i32).and_return ApplicationException::INVALID_MESSAGE_TYPE
      prot.should_receive(:skip).with(Types::STRING).twice
      prot.should_receive(:skip).with(Types::MAP)
      prot.should_receive(:read_field_end).exactly(4).times
      prot.should_receive(:read_struct_end).ordered

      e = ApplicationException.new
      e.read(prot)
      e.message.should be_nil
      e.type.should == ApplicationException::INVALID_MESSAGE_TYPE
    end

    it "should write a Thrift::ApplicationException struct to the oprot" do
      prot = mock("MockProtocol")
      prot.should_receive(:write_struct_begin).with("Thrift::ApplicationException").ordered
      prot.should_receive(:write_field_begin).with("message", Types::STRING, 1).ordered
      prot.should_receive(:write_string).with("test message").ordered
      prot.should_receive(:write_field_begin).with("type", Types::I32, 2).ordered
      prot.should_receive(:write_i32).with(ApplicationException::UNKNOWN_METHOD).ordered
      prot.should_receive(:write_field_end).twice
      prot.should_receive(:write_field_stop).ordered
      prot.should_receive(:write_struct_end).ordered

      e = ApplicationException.new(ApplicationException::UNKNOWN_METHOD, "test message")
      e.write(prot)
    end

    it "should skip nil fields when writing to the oprot" do
      prot = mock("MockProtocol")
      prot.should_receive(:write_struct_begin).with("Thrift::ApplicationException").ordered
      prot.should_receive(:write_field_begin).with("message", Types::STRING, 1).ordered
      prot.should_receive(:write_string).with("test message").ordered
      prot.should_receive(:write_field_end).ordered
      prot.should_receive(:write_field_stop).ordered
      prot.should_receive(:write_struct_end).ordered

      e = ApplicationException.new(nil, "test message")
      e.write(prot)

      prot = mock("MockProtocol")
      prot.should_receive(:write_struct_begin).with("Thrift::ApplicationException").ordered
      prot.should_receive(:write_field_begin).with("type", Types::I32, 2).ordered
      prot.should_receive(:write_i32).with(ApplicationException::BAD_SEQUENCE_ID).ordered
      prot.should_receive(:write_field_end).ordered
      prot.should_receive(:write_field_stop).ordered
      prot.should_receive(:write_struct_end).ordered

      e = ApplicationException.new(ApplicationException::BAD_SEQUENCE_ID)
      e.write(prot)

      prot = mock("MockProtocol")
      prot.should_receive(:write_struct_begin).with("Thrift::ApplicationException").ordered
      prot.should_receive(:write_field_stop).ordered
      prot.should_receive(:write_struct_end).ordered

      e = ApplicationException.new(nil)
      e.write(prot)
    end
  end

  describe ProtocolException do
    it "should have an accessible type" do
      prot = ProtocolException.new(ProtocolException::SIZE_LIMIT, "message")
      prot.type.should == ProtocolException::SIZE_LIMIT
      prot.message.should == "message"
    end
  end
end
