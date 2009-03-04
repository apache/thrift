require File.join(File.dirname(__FILE__), '../test_helper')
require 'ThriftTest'

class TestEnumGeneration < Test::Unit::TestCase
  include Thrift::Test
  def test_enum_valid_values
    assert_equal(Numberz::VALID_VALUES, Set.new([Numberz::ONE, Numberz::TWO, Numberz::THREE, Numberz::FIVE, Numberz::SIX, Numberz::EIGHT]))
  end
end