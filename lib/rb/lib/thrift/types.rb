require 'set'

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
  end
  deprecate_module! :TType => Types

  class << self
    attr_accessor :type_checking
  end

  class TypeError < Exception
  end

  def self.check_type(value, type)
    return unless Thrift.type_checking and not value.nil?
    klasses = case type
              when Types::VOID
                NilClass
              when Types::BOOL
                [TrueClass, FalseClass]
              when Types::BYTE, Types::I16, Types::I32, Types::I64
                Integer
              when Types::DOUBLE
                Float
              when Types::STRING
                String
              when Types::STRUCT
                Struct
              when Types::MAP
                Hash
              when Types::SET
                Set
              when Types::LIST
                Array
              end
    valid = klasses && [*klasses].any? { |klass| klass === value }
    raise TypeError, "Expected #{type_name(type)}, received #{value.class}" unless valid
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
  end
  deprecate_module! :TMessageType => MessageTypes
end

Thrift.type_checking = false if Thrift.type_checking.nil?
