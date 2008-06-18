require File.join(File.dirname(__FILE__), '../test_helper')

require 'thrift/thrift'

class TestTException < Test::Unit::TestCase
  def test_has_accessible_message
    msg = "hi there thrift"
    assert_equal msg, TException.new(msg).message
  end
  
end

class TestConstRemapping < Test::Unit::TestCase
  def test_remappings
    maps = {
      TException => Thrift::Exception,
      TApplicationException => Thrift::ApplicationException,
      TType => Thrift::Types,
      TMessageType => Thrift::MessageTypes,
      TProcessor => Thrift::Processor,
      ThriftClient => Thrift::Client,
      ThriftStruct => Thrift::Struct,
      TProtocol => Thrift::Protocol,
      TProtocolException => Thrift::ProtocolException
    }
    
    maps.each do |k, v|
      assert_equal k, v
    end
  end
end

