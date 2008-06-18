require File.join(File.dirname(__FILE__), '../../test_helper')

require 'thrift/transport/ttransport'

class DummyTransport < TTransport
  def initialize(data)
    @data = data
  end
  
  def read(size)
    @data.slice!(0, size)
  end
end

# TTransport is basically an abstract class, but isn't raising NotImplementedError
class TestTTransport < Test::Unit::TestCase
  def setup
    @trans = TTransport.new
  end
  
  def test_isOpen
    assert_nil @trans.isOpen
  end
  
  def test_open
    assert_nil @trans.open
  end
  
  def test_close
    assert_nil @trans.close
  end
  
  def test_read
    assert_nil @trans.read(100) # arbitrary size
  end
  
  # TODO:
  # This doesn't necessarily test he right thing.
  # It _looks_ like read isn't guarenteed to return the length
  # you ask for and readAll is. This means our test needs to check
  # for blocking. -- Kevin Clark 3/27/08
  def test_readAll
    # Implements read
    t = DummyTransport.new("hello")
    assert_equal "hello", t.readAll(5)
  end
  
  def test_write
    assert_nil @trans.write(5) # arbitrary value
  end
  
  def test_flush
    assert_nil @trans.flush
  end
end