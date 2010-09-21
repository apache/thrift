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

class ThriftStructSpec < Spec::ExampleGroup
  include Thrift
  include SpecNamespace

  describe Struct do
    it "should iterate over all fields properly" do
      fields = {}
      Foo.new.each_field { |fid,field_info| fields[fid] = field_info }
      fields.should == Foo::FIELDS
    end

    it "should initialize all fields to defaults" do
      validate_default_arguments(Foo.new)
    end

    it "should initialize all fields to defaults and accept a block argument" do
      Foo.new do |f|
        validate_default_arguments(f)
      end
    end

    def validate_default_arguments(object)
      object.simple.should == 53
      object.words.should == "words"
      object.hello.should == Hello.new(:greeting => 'hello, world!')
      object.ints.should == [1, 2, 2, 3]
      object.complex.should be_nil
      object.shorts.should == Set.new([5, 17, 239])
    end

    it "should not share default values between instances" do
      begin
        struct = Foo.new
        struct.ints << 17
        Foo.new.ints.should == [1,2,2,3]
      ensure
        # ensure no leakage to other tests
        Foo::FIELDS[4][:default] = [1,2,2,3]
      end
    end

    it "should properly initialize boolean values" do
      struct = BoolStruct.new(:yesno => false)
      struct.yesno.should be_false
    end

    it "should have proper == semantics" do
      Foo.new.should_not == Hello.new
      Foo.new.should == Foo.new
      Foo.new(:simple => 52).should_not == Foo.new
    end

    it "should print enum value names in inspect" do
      StructWithSomeEnum.new(:some_enum => SomeEnum::ONE).inspect.should == "<SpecNamespace::StructWithSomeEnum some_enum:ONE (0)>"

      StructWithEnumMap.new(:my_map => {SomeEnum::ONE => [SomeEnum::TWO]}).inspect.should == "<SpecNamespace::StructWithEnumMap my_map:{ONE (0): [TWO (1)]}>"
    end

    it "should pretty print binary fields" do
      Foo2.new(:my_binary => "\001\002\003").inspect.should == "<SpecNamespace::Foo2 my_binary:010203>"
    end

    it "should offer field? methods" do
      Foo.new.opt_string?.should be_false
      Foo.new(:simple => 52).simple?.should be_true
      Foo.new(:my_bool => false).my_bool?.should be_true
      Foo.new(:my_bool => true).my_bool?.should be_true
    end

    it "should be comparable" do
      s1 = StructWithSomeEnum.new(:some_enum => SomeEnum::ONE)
      s2 = StructWithSomeEnum.new(:some_enum => SomeEnum::TWO)

      (s1 <=> s2).should == -1
      (s2 <=> s1).should == 1
      (s1 <=> s1).should == 0
      (s1 <=> StructWithSomeEnum.new()).should == -1
    end

    it "should read itself off the wire" do
      struct = Foo.new
      prot = BaseProtocol.new(mock("transport"))
      prot.should_receive(:read_struct_begin).twice
      prot.should_receive(:read_struct_end).twice
      prot.should_receive(:read_field_begin).and_return(
        ['complex', Types::MAP, 5], # Foo
        ['words', Types::STRING, 2], # Foo
        ['hello', Types::STRUCT, 3], # Foo
          ['greeting', Types::STRING, 1], # Hello
          [nil, Types::STOP, 0], # Hello
        ['simple', Types::I32, 1], # Foo
        ['ints', Types::LIST, 4], # Foo
        ['shorts', Types::SET, 6], # Foo
        [nil, Types::STOP, 0] # Hello
      )
      prot.should_receive(:read_field_end).exactly(7).times
      prot.should_receive(:read_map_begin).and_return(
        [Types::I32, Types::MAP, 2], # complex
          [Types::STRING, Types::DOUBLE, 2], # complex/1/value
          [Types::STRING, Types::DOUBLE, 1] # complex/2/value
      )
      prot.should_receive(:read_map_end).exactly(3).times
      prot.should_receive(:read_list_begin).and_return([Types::I32, 4])
      prot.should_receive(:read_list_end)
      prot.should_receive(:read_set_begin).and_return([Types::I16, 2])
      prot.should_receive(:read_set_end)
      prot.should_receive(:read_i32).and_return(
        1, 14,        # complex keys
        42,           # simple
        4, 23, 4, 29  # ints
      )
      prot.should_receive(:read_string).and_return("pi", "e", "feigenbaum", "apple banana", "what's up?")
      prot.should_receive(:read_double).and_return(Math::PI, Math::E, 4.669201609)
      prot.should_receive(:read_i16).and_return(2, 3)
      prot.should_not_receive(:skip)
      struct.read(prot)

      struct.simple.should == 42
      struct.complex.should == {1 => {"pi" => Math::PI, "e" => Math::E}, 14 => {"feigenbaum" => 4.669201609}}
      struct.hello.should == Hello.new(:greeting => "what's up?")
      struct.words.should == "apple banana"
      struct.ints.should == [4, 23, 4, 29]
      struct.shorts.should == Set.new([3, 2])
    end

    it "should serialize false boolean fields correctly" do
      b = BoolStruct.new(:yesno => false)
      prot = BinaryProtocol.new(MemoryBufferTransport.new)
      prot.should_receive(:write_bool).with(false)
      b.write(prot)
    end

    it "should skip unexpected fields in structs and use default values" do
      struct = Foo.new
      prot = BaseProtocol.new(mock("transport"))
      prot.should_receive(:read_struct_begin)
      prot.should_receive(:read_struct_end)
      prot.should_receive(:read_field_begin).and_return(
        ['simple', Types::I32, 1],
        ['complex', Types::STRUCT, 5],
        ['thinz', Types::MAP, 7],
        ['foobar', Types::I32, 3],
        ['words', Types::STRING, 2],
        [nil, Types::STOP, 0]
      )
      prot.should_receive(:read_field_end).exactly(5).times
      prot.should_receive(:read_i32).and_return(42)
      prot.should_receive(:read_string).and_return("foobar")
      prot.should_receive(:skip).with(Types::STRUCT)
      prot.should_receive(:skip).with(Types::MAP)
      # prot.should_receive(:read_map_begin).and_return([Types::I32, Types::I32, 0])
      # prot.should_receive(:read_map_end)
      prot.should_receive(:skip).with(Types::I32)
      struct.read(prot)

      struct.simple.should == 42
      struct.complex.should be_nil
      struct.words.should == "foobar"
      struct.hello.should == Hello.new(:greeting => 'hello, world!')
      struct.ints.should == [1, 2, 2, 3]
      struct.shorts.should == Set.new([5, 17, 239])
    end

    it "should write itself to the wire" do
      prot = BaseProtocol.new(mock("transport")) #mock("Protocol")
      prot.should_receive(:write_struct_begin).with("SpecNamespace::Foo")
      prot.should_receive(:write_struct_begin).with("SpecNamespace::Hello")
      prot.should_receive(:write_struct_end).twice
      prot.should_receive(:write_field_begin).with('ints', Types::LIST, 4)
      prot.should_receive(:write_i32).with(1)
      prot.should_receive(:write_i32).with(2).twice
      prot.should_receive(:write_i32).with(3)
      prot.should_receive(:write_field_begin).with('complex', Types::MAP, 5)
      prot.should_receive(:write_i32).with(5)
      prot.should_receive(:write_string).with('foo')
      prot.should_receive(:write_double).with(1.23)
      prot.should_receive(:write_field_begin).with('shorts', Types::SET, 6)
      prot.should_receive(:write_i16).with(5)
      prot.should_receive(:write_i16).with(17)
      prot.should_receive(:write_i16).with(239)
      prot.should_receive(:write_field_stop).twice
      prot.should_receive(:write_field_end).exactly(6).times
      prot.should_receive(:write_field_begin).with('simple', Types::I32, 1)
      prot.should_receive(:write_i32).with(53)
      prot.should_receive(:write_field_begin).with('hello', Types::STRUCT, 3)
      prot.should_receive(:write_field_begin).with('greeting', Types::STRING, 1)
      prot.should_receive(:write_string).with('hello, world!')
      prot.should_receive(:write_map_begin).with(Types::I32, Types::MAP, 1)
      prot.should_receive(:write_map_begin).with(Types::STRING, Types::DOUBLE, 1)
      prot.should_receive(:write_map_end).twice
      prot.should_receive(:write_list_begin).with(Types::I32, 4)
      prot.should_receive(:write_list_end)
      prot.should_receive(:write_set_begin).with(Types::I16, 3)
      prot.should_receive(:write_set_end)

      struct = Foo.new
      struct.words = nil
      struct.complex = {5 => {"foo" => 1.23}}
      struct.write(prot)
    end

    it "should raise an exception if presented with an unknown container" do
      # yeah this is silly, but I'm going for code coverage here
      struct = Foo.new
      lambda { struct.send :write_container, nil, nil, {:type => "foo"} }.should raise_error(StandardError, "Not a container type: foo")
    end

    it "should support optional type-checking in Thrift::Struct.new" do
      Thrift.type_checking = true
      begin
        lambda { Hello.new(:greeting => 3) }.should raise_error(TypeError, "Expected Types::STRING, received Fixnum for field greeting")
      ensure
        Thrift.type_checking = false
      end
      lambda { Hello.new(:greeting => 3) }.should_not raise_error(TypeError)
    end

    it "should support optional type-checking in field accessors" do
      Thrift.type_checking = true
      begin
        hello = Hello.new
        lambda { hello.greeting = 3 }.should raise_error(TypeError, "Expected Types::STRING, received Fixnum for field greeting")
      ensure
        Thrift.type_checking = false
      end
      lambda { hello.greeting = 3 }.should_not raise_error(TypeError)
    end

    it "should raise an exception when unknown types are given to Thrift::Struct.new" do
      lambda { Hello.new(:fish => 'salmon') }.should raise_error(Exception, "Unknown key given to SpecNamespace::Hello.new: fish")
    end

    it "should support `raise Xception, 'message'` for Exception structs" do
      begin
        raise Xception, "something happened"
      rescue Thrift::Exception => e
        e.message.should == "something happened"
        e.code.should == 1
        # ensure it gets serialized properly, this is the really important part
        prot = BaseProtocol.new(mock("trans"))
        prot.should_receive(:write_struct_begin).with("SpecNamespace::Xception")
        prot.should_receive(:write_struct_end)
        prot.should_receive(:write_field_begin).with('message', Types::STRING, 1)#, "something happened")
        prot.should_receive(:write_string).with("something happened")
        prot.should_receive(:write_field_begin).with('code', Types::I32, 2)#, 1)
        prot.should_receive(:write_i32).with(1)
        prot.should_receive(:write_field_stop)
        prot.should_receive(:write_field_end).twice

        e.write(prot)
      end
    end

    it "should support the regular initializer for exception structs" do
      begin
        raise Xception, :message => "something happened", :code => 5
      rescue Thrift::Exception => e
        e.message.should == "something happened"
        e.code.should == 5
        prot = BaseProtocol.new(mock("trans"))
        prot.should_receive(:write_struct_begin).with("SpecNamespace::Xception")
        prot.should_receive(:write_struct_end)
        prot.should_receive(:write_field_begin).with('message', Types::STRING, 1)
        prot.should_receive(:write_string).with("something happened")
        prot.should_receive(:write_field_begin).with('code', Types::I32, 2)
        prot.should_receive(:write_i32).with(5)
        prot.should_receive(:write_field_stop)
        prot.should_receive(:write_field_end).twice

        e.write(prot)
      end
    end
  end
end
