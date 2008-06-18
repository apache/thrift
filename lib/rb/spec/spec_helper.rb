require 'rubygems'
require 'spec'

# turn on deprecation so we can test it
module Thrift
  DEPRECATION = true
end

require File.dirname(__FILE__) + '/../lib/thrift'

class Object
  # tee is a useful method, so let's let our tests have it
  def tee(&block)
    block.call(self)
    self
  end
end
