# frozen_string_literal: true
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

require "set"

module Thrift
  module Types
    STOP = 0
    VOID = 1
    BOOL = 2
    BYTE = 3
    DOUBLE = 4
    I16 = 6
    I32 = 8
    I64 = 10
    STRING = 11
    STRUCT = 12
    MAP = 13
    SET = 14
    LIST = 15
    UUID = 16
  end

  class << self
    attr_accessor :type_checking
  end

  class TypeError < Exception
  end

  def self.check_type(value, field, name, skip_nil = true)
    return if value.nil? and skip_nil
    valid = case field[:type]
    when Types::VOID
      NilClass === value
    when Types::BOOL
      TrueClass === value || FalseClass === value
    when Types::BYTE, Types::I16, Types::I32, Types::I64
      Integer === value
    when Types::DOUBLE
      Float === value
    when Types::STRING
      String === value
    when Types::UUID
      String === value
    when Types::STRUCT
      Struct === value || Union === value
    when Types::MAP
      Hash === value
    when Types::SET
      Set === value
    when Types::LIST
      Array === value
    end
    raise TypeError, "Expected #{type_name(field[:type])}, received #{value.class} for field #{name}" unless valid
    # check elements now
    case field[:type]
    when Types::MAP
      key_name = value_name = nil
      value.each_pair do |k, v|
        check_type(k, field[:key], key_name ||= "#{name}.key", false)
        check_type(v, field[:value], value_name ||= "#{name}.value", false)
      end
    when Types::SET, Types::LIST
      element_name = nil
      value.each do |el|
        check_type(el, field[:element], element_name ||= "#{name}.element", false)
      end
    when Types::STRUCT
      raise TypeError, "Expected #{field[:class]}, received #{value.class} for field #{name}" unless field[:class] == value.class
    end
  end

  def self.type_name(type)
    Types.constants.each do |const|
      return "Types::#{const}" if Types.const_get(const) == type
    end
    nil
  end

  module MessageTypes
    CALL = 1
    REPLY = 2
    EXCEPTION = 3
    ONEWAY = 4
  end
end

Thrift.type_checking = false if Thrift.type_checking.nil?
