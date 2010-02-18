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

class ThriftUnionSpec < Spec::ExampleGroup
  include Thrift
  include SpecNamespace

  describe Union do
    it "should return nil value in unset union" do
      union = My_union.new
      union.get_set_field.should == nil
      union.get_value.should == nil
    end

    it "should set a field and be accessible through get_value and the named field accessor" do
      union = My_union.new
      union.integer32 = 25
      union.get_set_field.should == :integer32
      union.get_value.should == 25
      union.integer32.should == 25
    end

    it "should work correctly when instantiated with static field constructors" do
      union = My_union.integer32(5)
      union.get_set_field.should == :integer32
      union.integer32.should == 5
    end

    it "should raise for wrong set field" do
      union = My_union.new
      union.integer32 = 25
      lambda { union.some_characters }.should raise_error(RuntimeError, "some_characters is not union's set field.")
    end
     
    it "should not be equal to nil" do
      union = My_union.new
      union.should_not == nil
    end
     
    it "should not equate two different unions, i32 vs. string" do
      union = My_union.new(:integer32, 25)
      other_union = My_union.new(:some_characters, "blah!")
      union.should_not == other_union
    end

    it "should properly reset setfield and setvalue" do
      union = My_union.new(:integer32, 25)
      union.get_set_field.should == :integer32
      union.some_characters = "blah!"
      union.get_set_field.should == :some_characters
      union.get_value.should == "blah!"
      lambda { union.integer32 }.should raise_error(RuntimeError, "integer32 is not union's set field.")
    end

    it "should not equate two different unions with different values" do
      union = My_union.new(:integer32, 25)
      other_union = My_union.new(:integer32, 400)
      union.should_not == other_union
    end

    it "should not equate two different unions with different fields" do
      union = My_union.new(:integer32, 25)
      other_union = My_union.new(:other_i32, 25)
      union.should_not == other_union
    end

    it "should inspect properly" do
      union = My_union.new(:integer32, 25)
      union.inspect.should == "<SpecNamespace::My_union integer32: 25>"
    end

    it "should not allow setting with instance_variable_set" do
      union = My_union.new(:integer32, 27)
      union.instance_variable_set(:@some_characters, "hallo!")
      union.get_set_field.should == :integer32
      union.get_value.should == 27
      lambda { union.some_characters }.should raise_error(RuntimeError, "some_characters is not union's set field.")
    end

    it "should serialize correctly" do
      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::BinaryProtocol.new(trans)

      union = My_union.new(:integer32, 25)
      union.write(proto)

      other_union = My_union.new(:integer32, 25)
      other_union.read(proto)
      other_union.should == union
    end

    it "should raise when validating unset union" do
      union = My_union.new
      lambda { union.validate }.should raise_error(StandardError, "Union fields are not set.")

      other_union = My_union.new(:integer32, 1)
      lambda { other_union.validate }.should_not raise_error(StandardError, "Union fields are not set.")
    end

    it "should validate an enum field properly" do
      union = TestUnion.new(:enum_field, 3)
      union.get_set_field.should == :enum_field
      lambda { union.validate }.should raise_error(ProtocolException, "Invalid value of field enum_field!")

      other_union = TestUnion.new(:enum_field, 1)
      lambda { other_union.validate }.should_not raise_error(ProtocolException, "Invalid value of field enum_field!")
    end

    it "should properly serialize and match structs with a union" do
      union = My_union.new(:integer32, 26)
      swu = Struct_with_union.new(:fun_union => union)

      trans = Thrift::MemoryBufferTransport.new
      proto = Thrift::CompactProtocol.new(trans)

      swu.write(proto)

      other_union = My_union.new(:some_characters, "hello there")
      swu2 = Struct_with_union.new(:fun_union => other_union)

      swu2.should_not == swu

      swu2.read(proto)
      swu2.should == swu
    end

    it "should support old style constructor" do
      union = My_union.new(:integer32 => 26)
      union.get_set_field.should == :integer32
      union.get_value.should == 26
    end

    it "should not throw an error when inspected and unset" do
      lambda{TestUnion.new().inspect}.should_not raise_error
    end

    it "should print enum value name when inspected" do
      My_union.new(:some_enum => SomeEnum::ONE).inspect.should == "<SpecNamespace::My_union some_enum: ONE (0)>"

      My_union.new(:my_map => {SomeEnum::ONE => [SomeEnum::TWO]}).inspect.should == "<SpecNamespace::My_union my_map: {ONE (0): [TWO (1)]}>" 
    end

    it "should offer field? methods" do
      My_union.new.some_enum?.should be_false
      My_union.new(:some_enum => SomeEnum::ONE).some_enum?.should be_true
      My_union.new(:im_true => false).im_true?.should be_true
      My_union.new(:im_true => true).im_true?.should be_true
    end

    it "should pretty print binary fields" do
      TestUnion.new(:binary_field => "\001\002\003").inspect.should == "<SpecNamespace::TestUnion binary_field: 010203>"
    end

    it "should be comparable" do
      relationships = [
        [0,   -1, -1, -1],
        [1,   0,  -1, -1],
        [1,   1,  0,  -1],
        [1,   1,  1,  0]]

      objs = [
        TestUnion.new(:string_field, "blah"), 
        TestUnion.new(:string_field, "blahblah"),
        TestUnion.new(:i32_field, 1),
        TestUnion.new()]

      for y in 0..3
        for x in 0..3
          # puts "#{objs[y].inspect} <=> #{objs[x].inspect} should == #{relationships[y][x]}"
          (objs[y] <=> objs[x]).should == relationships[y][x]
        end
      end
    end
  end
end
