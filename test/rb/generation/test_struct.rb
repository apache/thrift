require File.join(File.dirname(__FILE__), '../test_helper')
require 'SmallService'

class TestStructGeneration < Test::Unit::TestCase

  def test_default_values
    hello = TestNamespace::Hello.new

    assert_kind_of(TestNamespace::Hello, hello)
    assert_nil(hello.complexer)

    assert_equal(hello.simple, 53)
    assert_equal(hello.words, 'words')

    assert_kind_of(TestNamespace::Goodbyez, hello.thinz)
    assert_equal(hello.thinz.val, 36632)

    assert_kind_of(Hash, hello.complex)
    assert_equal(hello.complex, { 6243 => 632, 2355 => 532, 23 => 532})
    
    bool_passer = TestNamespace::BoolPasser.new(:value => false)
    assert_equal false, bool_passer.value
  end

  def test_goodbyez
    assert_equal(TestNamespace::Goodbyez.new.val, 325)
  end

end
