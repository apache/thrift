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

$:.unshift File.expand_path('../../lib', __dir__)
$:.unshift File.expand_path('../../ext', __dir__)
require 'thrift'
$:.unshift File.dirname(__FILE__) + "/gen-rb"
require 'fuzz_test_constants'

require 'coverage'
Coverage.start(branches: true) unless Coverage.respond_to?(:running?) && Coverage.running?
require 'ruzzy'
# Ruzzy.enable_branch_coverage_hooks

def ignorable_fuzz_exception?(error)
  return true if error.is_a?(Thrift::ProtocolException) ||
    error.is_a?(EOFError) ||
    error.is_a?(Encoding::UndefinedConversionError)

  [
    /don't know what (?:c)?type/,
    /Too many fields for union/,
    /too big to convert to '(?:int|long)'/,
    /bignum too big to convert into 'long'/,
    /negative array size/,
    /Union fields are not set/
  ].any? { |pattern| error.message =~ pattern }
end

def read_fuzz_test(protocol, read_message_begin)
  protocol.read_message_begin if read_message_begin
  obj = Fuzz::FuzzTest.new
  obj.read(protocol)
  protocol.read_message_end if read_message_begin
  obj
end

def write_fuzz_test(protocol, obj, write_message_begin)
  if write_message_begin
    protocol.write_message_begin('fuzz', Thrift::MessageTypes::CALL, 0)
  end
  obj.write(protocol)
  protocol.write_message_end if write_message_begin
end

def create_parser_fuzzer(protocol_factory_class, read_message_begin: false)
  lambda do |data|
    transport = Thrift::MemoryBufferTransport.new(data)
    protocol = protocol_factory_class.new.get_protocol(transport)
    read_fuzz_test(protocol, read_message_begin)
    0
  rescue StandardError => e
    # We're looking for memory corruption, not Ruby exceptions
    raise unless ignorable_fuzz_exception?(e)
  end
end

def create_roundtrip_fuzzer(protocol_factory_class, read_message_begin: false)
  lambda do |data|
    transport = Thrift::MemoryBufferTransport.new(data)
    protocol = protocol_factory_class.new.get_protocol(transport)
    obj = read_fuzz_test(protocol, read_message_begin)

    serialized_data = +""
    transport2 = Thrift::MemoryBufferTransport.new(serialized_data)
    protocol2 = protocol_factory_class.new.get_protocol(transport2)
    write_fuzz_test(protocol2, obj, read_message_begin)

    transport3 = Thrift::MemoryBufferTransport.new(serialized_data)
    protocol3 = protocol_factory_class.new.get_protocol(transport3)
    deserialized_obj = read_fuzz_test(protocol3, read_message_begin)

    raise "Roundtrip mismatch" unless obj == deserialized_obj
    0
  rescue StandardError => e
    # We're looking for memory corruption, not Ruby exceptions
    raise unless ignorable_fuzz_exception?(e)
  end
end
