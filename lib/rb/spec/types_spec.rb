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

class ThriftTypesSpec < Spec::ExampleGroup
  include Thrift

  before(:each) do
    Thrift.type_checking = true
  end

  after(:each) do
    Thrift.type_checking = false
  end

  describe "Type checking" do
    it "should return the proper name for each type" do
      Thrift.type_name(Types::I16).should == "Types::I16"
      Thrift.type_name(Types::VOID).should == "Types::VOID"
      Thrift.type_name(Types::LIST).should == "Types::LIST"
      Thrift.type_name(42).should be_nil
    end

    it "should check types properly" do
      # lambda { Thrift.check_type(nil, Types::STOP) }.should raise_error(TypeError)
      lambda { Thrift.check_type(3,              {:type => Types::STOP},   :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil,            {:type => Types::VOID},   :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(3,              {:type => Types::VOID},   :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type(true,           {:type => Types::BOOL},   :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(3,              {:type => Types::BOOL},   :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type(42,             {:type => Types::BYTE},   :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(42,             {:type => Types::I16},    :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(42,             {:type => Types::I32},    :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(42,             {:type => Types::I64},    :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(3.14,           {:type => Types::I32},    :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type(3.14,           {:type => Types::DOUBLE}, :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(3,              {:type => Types::DOUBLE}, :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type("3",            {:type => Types::STRING}, :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(3,              {:type => Types::STRING}, :foo) }.should raise_error(TypeError)
      hello = SpecNamespace::Hello.new
      lambda { Thrift.check_type(hello,          {:type => Types::STRUCT, :class => SpecNamespace::Hello}, :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type("foo",          {:type => Types::STRUCT}, :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type({:foo => 1},    {:type => Types::MAP},    :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type([1],            {:type => Types::MAP},    :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type([1],            {:type => Types::LIST},   :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type({:foo => 1},    {:type => Types::LIST},   :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type(Set.new([1,2]), {:type => Types::SET},    :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type([1,2],          {:type => Types::SET},    :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type({:foo => true}, {:type => Types::SET},    :foo) }.should raise_error(TypeError)
    end

    it "should error out if nil is passed and skip_types is false" do
      lambda { Thrift.check_type(nil, {:type => Types::BOOL},   :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::BYTE},   :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::I16},    :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::I32},    :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::I64},    :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::DOUBLE}, :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::STRING}, :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::STRUCT}, :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::LIST},   :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::SET},    :foo, false) }.should raise_error(TypeError)
      lambda { Thrift.check_type(nil, {:type => Types::MAP},    :foo, false) }.should raise_error(TypeError)
    end

    it "should check element types on containers" do
      field = {:type => Types::LIST, :element => {:type => Types::I32}}
      lambda { Thrift.check_type([1, 2], field, :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type([1, nil, 2], field, :foo) }.should raise_error(TypeError)
      field = {:type => Types::MAP, :key => {:type => Types::I32}, :value => {:type => Types::STRING}}
      lambda { Thrift.check_type({1 => "one", 2 => "two"}, field, :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type({1 => "one", nil => "nil"}, field, :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type({1 => nil, 2 => "two"}, field, :foo) }.should raise_error(TypeError)
      field = {:type => Types::SET, :element => {:type => Types::I32}}
      lambda { Thrift.check_type(Set.new([1, 2]), field, :foo) }.should_not raise_error(TypeError)
      lambda { Thrift.check_type(Set.new([1, nil, 2]), field, :foo) }.should raise_error(TypeError)
      lambda { Thrift.check_type(Set.new([1, 2.3, 2]), field, :foo) }.should raise_error(TypeError)
      
      field = {:type => Types::STRUCT, :class => SpecNamespace::Hello}
      lambda { Thrift.check_type(SpecNamespace::BoolStruct, field, :foo) }.should raise_error(TypeError)
    end

    it "should give the TypeError a readable message" do
      msg = "Expected Types::STRING, received Fixnum for field foo"
      lambda { Thrift.check_type(3, {:type => Types::STRING}, :foo) }.should raise_error(TypeError, msg)
      msg = "Expected Types::STRING, received Fixnum for field foo.element"
      field = {:type => Types::LIST, :element => {:type => Types::STRING}}
      lambda { Thrift.check_type([3], field, :foo) }.should raise_error(TypeError, msg)
      msg = "Expected Types::I32, received NilClass for field foo.element.key"
      field = {:type => Types::LIST,
               :element => {:type => Types::MAP,
                            :key => {:type => Types::I32},
                            :value => {:type => Types::I32}}}
      lambda { Thrift.check_type([{nil => 3}], field, :foo) }.should raise_error(TypeError, msg)
      msg = "Expected Types::I32, received NilClass for field foo.element.value"
      lambda { Thrift.check_type([{1 => nil}], field, :foo) }.should raise_error(TypeError, msg)
    end
  end
end
