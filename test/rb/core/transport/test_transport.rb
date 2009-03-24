require File.join(File.dirname(__FILE__), '../../test_helper')

require 'thrift/transport'

class DummyTransport < Thrift::Transport
  def initialize(data)
    @data = data
  end
  
  def read(size)
    @data.slice!(0, size)
  end
end

# TTransport is basically an abstract class, but isn't raising NotImplementedError
class TestThriftTransport < Test::Unit::TestCase
  def setup
    @trans = Thrift::Transport.new
  end
  
  def test_open?
    assert_nil @trans.open?
  end
  
  def test_open
    assert_nil @trans.open
  end
  
  def test_close
    assert_nil @trans.close
  end
  
  # TODO:
  # This doesn't necessarily test he right thing.
  # It _looks_ like read isn't guarenteed to return the length
  # you ask for and read_all is. This means our test needs to check
  # for blocking. -- Kevin Clark 3/27/08
  def test_read_all
    # Implements read
    t = DummyTransport.new("hello")
    assert_equal "hello", t.read_all(5)
  end
  
  def test_write
    assert_nil @trans.write(5) # arbitrary value
  end
  
  def test_flush
    assert_nil @trans.flush
  end
end
