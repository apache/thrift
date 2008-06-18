require File.join(File.dirname(__FILE__), '../test_helper')
require 'SmallService'

class TestStructGeneration < Test::Unit::TestCase

  def test_default_values
    hello = Hello.new

    assert_kind_of(Hello, hello)
    assert_nil(hello.complexer)

    assert_equal(hello.simple, 53)
    assert_equal(hello.words, 'words')

    assert_kind_of(Goodbyez, hello.thinz)
    assert_equal(hello.thinz.val, 36632)

    assert_kind_of(Hash, hello.complex)
    assert_equal(hello.complex, { 6243 => 632, 2355 => 532, 23 => 532})
  end

  def test_goodbyez
    assert_equal(Goodbyez.new.val, 325)
  end

end
