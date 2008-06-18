require File.dirname(__FILE__) + '/spec_helper'

describe Thrift::Exception do
  it "should have an accessible message" do
    e = Thrift::Exception.new("test message")
    e.message.should == "test message"
  end
end

describe Thrift::ApplicationException do
  it "should inherit from Thrift::Exception" do
    Thrift::ApplicationException.superclass.should == Thrift::Exception
  end

  it "should have an accessible type and message" do
    e = Thrift::ApplicationException.new
    e.type.should == Thrift::ApplicationException::UNKNOWN
    e.message.should be_nil
    e = Thrift::ApplicationException.new(Thrift::ApplicationException::UNKNOWN_METHOD, "test message")
    e.type.should == Thrift::ApplicationException::UNKNOWN_METHOD
    e.message.should == "test message"
  end
end
