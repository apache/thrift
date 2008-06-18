require 'set'

module Thrift
  module Struct
    def initialize(d={})
      each_field do |fid, type, name, default|
        instance_variable_set("@#{name}", d[name.to_s] || d[name.intern] || default)
      end
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
      iprot.read_struct_begin
      loop do
        fname, ftype, fid = iprot.read_field_begin
        break if (ftype === Types::STOP)
        handle_message(iprot, fid, ftype)
        iprot.read_field_end
      end
      iprot.read_struct_end
    end

    def write(oprot)
      oprot.write_struct_begin(self.class.name)
      each_field do |fid, type, name|
        if ((value = instance_variable_get("@#{name}")) != nil)
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

    def ==(other)
      return false unless other.is_a?(self.class)
      each_field do |fid, type, name, default|
        return false unless self.instance_variable_get("@#{name}") == other.instance_variable_get("@#{name}")
      end
      true
    end

    protected

    def handle_message(iprot, fid, ftype)
      field = struct_fields[fid]
      if field && field[:type] == ftype
        value = read_field(iprot, field)
        instance_variable_set("@#{field[:name]}", value)
      else
        iprot.skip(ftype)
      end
    end

    def read_field(iprot, field = {})
      if field[:type] == Types::STRUCT
        value = field[:class].new
        value.read(iprot)
      elsif field[:type] == Types::MAP
        key_type, val_type, size = iprot.read_map_begin
        value = {}
        size.times do
          k = read_field(iprot, field_info(field[:key]))
          v = read_field(iprot, field_info(field[:value]))
          value[k] = v
        end
        iprot.read_map_end
      elsif field[:type] == Types::LIST
        e_type, size = iprot.read_list_begin
        value = Array.new(size) do |n|
          read_field(iprot, field_info(field[:element]))
        end
        iprot.read_list_end
      elsif field[:type] == Types::SET
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
      if field[:type] == Types::MAP
        oprot.write_map_begin(field[:key][:type], field[:value][:type], value.size)
        value.each do |k, v|
          write_data(oprot, k, field[:key])
          write_data(oprot, v, field[:value])
        end
        oprot.write_map_end
      elsif field[:type] == Types::LIST
        oprot.write_list_begin(field[:element][:type], value.size)
        value.each do |elem|
          write_data(oprot, elem, field[:element])
        end
        oprot.write_list_end
      elsif field[:type] == Types::SET
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
