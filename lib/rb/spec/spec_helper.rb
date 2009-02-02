require 'rubygems'
# require at least 1.1.4 to fix a bug with describing Modules
gem 'rspec', '>= 1.1.4'
require 'spec'

$:.unshift File.join(File.dirname(__FILE__), *%w[.. ext])

# pretend we already loaded fastthread, otherwise the nonblockingserver_spec
# will get screwed up
# $" << 'fastthread.bundle'

# turn on deprecation so we can test it
module Thrift
  # squelch any warnings if we happen to get required twice
  remove_const(:DEPRECATION) if const_defined? :DEPRECATION
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

Spec::Runner.configure do |configuration|
  configuration.before(:each) do
    Thrift.type_checking = true
  end
end

require "thrift_native"