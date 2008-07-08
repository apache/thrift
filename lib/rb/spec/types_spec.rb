require File.dirname(__FILE__) + '/spec_helper'
require File.dirname(__FILE__) + '/gen-rb/ThriftSpec_types'

class ThriftTypesSpec < Spec::ExampleGroup
  include Thrift

  describe "Type checking" do
    it "should return the proper name for each type" do
      Thrift.type_name(Types::I16).should == "Types::I16"
      Thrift.type_name(Types::VOID).should == "Types::VOID"
      Thrift.type_name(Types::LIST).should == "Types::LIST"
      Thrift.type_name(42).should be_nil
    end

    it "should check types properly" do
      Thrift.type_checking = true
      begin
        # lambda { Thrift.check_type(nil, Types::STOP) }.should raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::STOP) }.should raise_error(TypeError)
        lambda { Thrift.check_type(nil, Types::VOID) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::VOID) }.should raise_error(TypeError)
        lambda { Thrift.check_type(true, Types::BOOL) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::BOOL) }.should raise_error(TypeError)
        # lambda { Thrift.check_type(nil, Types::BOOL) }.should raise_error(TypeError)
        lambda { Thrift.check_type(42, Types::BYTE) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(42, Types::I16) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(42, Types::I32) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(42, Types::I64) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3.14, Types::I32) }.should raise_error(TypeError)
        lambda { Thrift.check_type(3.14, Types::DOUBLE) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::DOUBLE) }.should raise_error(TypeError)
        lambda { Thrift.check_type("3", Types::STRING) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::STRING) }.should raise_error(TypeError)
        hello = SpecNamespace::Hello.new
        lambda { Thrift.check_type(hello, Types::STRUCT) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type("foo", Types::STRUCT) }.should raise_error(TypeError)
        lambda { Thrift.check_type({:foo => 1}, Types::MAP) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type([1], Types::MAP) }.should raise_error(TypeError)
        lambda { Thrift.check_type([1], Types::LIST) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type({:foo => 1}, Types::LIST) }.should raise_error(TypeError)
        lambda { Thrift.check_type(Set.new([1,2]), Types::SET) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type([1,2], Types::SET) }.should raise_error(TypeError)
        lambda { Thrift.check_type({:foo => true}, Types::SET) }.should raise_error(TypeError)
      ensure
        Thrift.type_checking = false
      end
    end

    it "should be disabled when Thrift.type_checking = false" do
      pending "disabled, parents should check Thrift.type_checking"
      Thrift.type_checking = false
      lambda { Thrift.check_type(3, Types::STRING) }.should_not raise_error(TypeError)
    end
  end
end
