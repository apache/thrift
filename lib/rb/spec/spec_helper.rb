require 'rubygems'
require 'spec'

require File.dirname(__FILE__) + '/../lib/thrift'

class Object
  # tee is a useful method, so let's let our tests have it
  def tee(&block)
    block.call(self)
    self
  end
end
