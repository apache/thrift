require 'rubygems'
# require at least 1.1.4 to fix a bug with describing Modules
gem 'rspec', '>= 1.1.4'
require 'spec'

$:.unshift File.join(File.dirname(__FILE__), *%w[.. ext])

# pretend we already loaded fastthread, otherwise the nonblockingserver_spec
# will get screwed up
# $" << 'fastthread.bundle'

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

require File.dirname(__FILE__) + "/../debug_proto_test/gen-rb/Srv"

module Fixtures
  COMPACT_PROTOCOL_TEST_STRUCT = CompactProtoTestStruct.new(:a_binary => [0,1,2,3,4,5,6,7,8].pack('c*'))
end