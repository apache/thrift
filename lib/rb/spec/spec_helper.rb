#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

require 'rubygems'
# require at least 1.1.4 to fix a bug with describing Modules
gem 'rspec', '>= 1.1.4'
require 'spec'

$:.unshift File.join(File.dirname(__FILE__), *%w[.. ext])

# pretend we already loaded fastthread, otherwise the nonblocking_server_spec
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

$:.unshift File.join(File.dirname(__FILE__), *%w[.. debug_proto_test gen-rb])
require "srv"
require "debug_proto_test_constants"

$:.unshift File.join(File.dirname(__FILE__), *%w[gen-rb])
require 'thrift_spec_types'
require 'nonblocking_service'

module Fixtures
  COMPACT_PROTOCOL_TEST_STRUCT = COMPACT_TEST.dup
  COMPACT_PROTOCOL_TEST_STRUCT.a_binary = [0,1,2,3,4,5,6,7,8].pack('c*')
  COMPACT_PROTOCOL_TEST_STRUCT.set_byte_map = nil
  COMPACT_PROTOCOL_TEST_STRUCT.map_byte_map = nil
end