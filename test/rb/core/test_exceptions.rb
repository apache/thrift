require File.join(File.dirname(__FILE__), '../test_helper')

require 'thrift'

class TestException < Test::Unit::TestCase
  def test_has_accessible_message
    msg = "hi there thrift"
    assert_equal msg, Thrift::Exception.new(msg).message
  end
end

