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
        lambda { Thrift.check_type(3, Types::STOP, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type(nil, Types::VOID, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::VOID, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type(true, Types::BOOL, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::BOOL, :foo) }.should raise_error(TypeError)
        # lambda { Thrift.check_type(nil, Types::BOOL, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type(42, Types::BYTE, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(42, Types::I16, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(42, Types::I32, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(42, Types::I64, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3.14, Types::I32, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type(3.14, Types::DOUBLE, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::DOUBLE, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type("3", Types::STRING, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type(3, Types::STRING, :foo) }.should raise_error(TypeError)
        hello = SpecNamespace::Hello.new
        lambda { Thrift.check_type(hello, Types::STRUCT, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type("foo", Types::STRUCT, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type({:foo => 1}, Types::MAP, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type([1], Types::MAP, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type([1], Types::LIST, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type({:foo => 1}, Types::LIST, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type(Set.new([1,2]), Types::SET, :foo) }.should_not raise_error(TypeError)
        lambda { Thrift.check_type([1,2], Types::SET, :foo) }.should raise_error(TypeError)
        lambda { Thrift.check_type({:foo => true}, Types::SET, :foo) }.should raise_error(TypeError)
      ensure
        Thrift.type_checking = false
      end
    end

    it "should give the TypeError a readable message" do
      Thrift.type_checking = true
      begin
        lambda { Thrift.check_type(3, Types::STRING, :foo) }.should raise_error(TypeError, "Expected Types::STRING, received Fixnum for field foo")
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
