require 'thrift/types'
require 'set'

module Thrift
  module Struct
    def initialize(d={})
      each_field do |fid, type, name, default|
        value = d.delete(name.to_s) { d.delete(name.to_sym) { default.dup rescue default } }
        Thrift.check_type(value, type)
        instance_variable_set("@#{name}", value)
      end
      raise Exception, "Unknown keys given to #{self.class}.new: #{d.keys.join(", ")}" unless d.empty?
    end

    def struct_fields
      self.class.const_get(:FIELDS)
    end

    def each_field
      struct_fields.each do |fid, data|
        yield fid, data[:type], data[:name], data[:default]
      end
    end

    def read(iprot)
      # TODO(kevinclark): Make sure transport is C readable
      if iprot.respond_to?(:decode_binary)
        iprot.decode_binary(self, iprot.trans)
      else
        iprot.read_struct_begin
        loop do
          fname, ftype, fid = iprot.read_field_begin
          break if (ftype == Types::STOP)
          handle_message(iprot, fid, ftype)
          iprot.read_field_end
        end
        iprot.read_struct_end
      end
    end

    def write(oprot)
      if oprot.respond_to?(:encode_binary)
        # TODO(kevinclark): Clean this so I don't have to access the transport.
        oprot.trans.write oprot.encode_binary(self)
      else
        oprot.write_struct_begin(self.class.name)
        each_field do |fid, type, name|
          unless (value = instance_variable_get("@#{name}")).nil?
            if is_container? type
              oprot.write_field_begin(name, type, fid)
              write_container(oprot, value, struct_fields[fid])
              oprot.write_field_end
            else
              oprot.write_field(name, type, fid, value)
            end
          end
        end
        oprot.write_field_stop
        oprot.write_struct_end
      end
    end

    def ==(other)
      return false unless other.is_a?(self.class)
      each_field do |fid, type, name, default|
        return false unless self.instance_variable_get("@#{name}") == other.instance_variable_get("@#{name}")
      end
      true
    end

    def self.field_accessor(klass, *fields)
      fields.each do |field|
        klass.send :attr_reader, field
        klass.send :define_method, "#{field}=" do |value|
          Thrift.check_type(value, klass::FIELDS.values.find { |f| f[:name].to_s == field.to_s }[:type] )
          instance_variable_set("@#{field}", value)
        end
      end
    end

    protected

    def handle_message(iprot, fid, ftype)
      field = struct_fields[fid]
      if field and field[:type] == ftype
        value = read_field(iprot, field)
        instance_variable_set("@#{field[:name]}", value)
      else
        iprot.skip(ftype)
      end
    end

    def read_field(iprot, field = {})
      case field[:type]
      when Types::STRUCT
        value = field[:class].new
        value.read(iprot)
      when Types::MAP
        key_type, val_type, size = iprot.read_map_begin
        value = {}
        size.times do
          k = read_field(iprot, field_info(field[:key]))
          v = read_field(iprot, field_info(field[:value]))
          value[k] = v
        end
        iprot.read_map_end
      when Types::LIST
        e_type, size = iprot.read_list_begin
        value = Array.new(size) do |n|
          read_field(iprot, field_info(field[:element]))
        end
        iprot.read_list_end
      when Types::SET
        e_type, size = iprot.read_set_begin
        value = Set.new
        size.times do
          element = read_field(iprot, field_info(field[:element]))
          value << element
        end
        iprot.read_set_end
      else
        value = iprot.read_type(field[:type])
      end
      value
    end

    def write_data(oprot, value, field)
      if is_container? field[:type]
        write_container(oprot, value, field)
      else
        oprot.write_type(field[:type], value)
      end
    end

    def write_container(oprot, value, field = {})
      case field[:type]
      when Types::MAP
        oprot.write_map_begin(field[:key][:type], field[:value][:type], value.size)
        value.each do |k, v|
          write_data(oprot, k, field[:key])
          write_data(oprot, v, field[:value])
        end
        oprot.write_map_end
      when Types::LIST
        oprot.write_list_begin(field[:element][:type], value.size)
        value.each do |elem|
          write_data(oprot, elem, field[:element])
        end
        oprot.write_list_end
      when Types::SET
        oprot.write_set_begin(field[:element][:type], value.size)
        value.each do |v,| # the , is to preserve compatibility with the old Hash-style sets
          write_data(oprot, v, field[:element])
        end
        oprot.write_set_end
      else
        raise "Not a container type: #{field[:type]}"
      end
    end

    def is_container?(type)
      [Types::LIST, Types::MAP, Types::SET].include? type
    end

    def field_info(field)
      { :type => field[:type],
        :class => field[:class],
        :key => field[:key],
        :value => field[:value],
        :element => field[:element] }
    end
  end
  deprecate_module! :ThriftStruct => Struct
end
