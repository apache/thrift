require 'thrift/types'
require 'set'

module Thrift
  module Struct
    def initialize(d={})
      # get a copy of the default values to work on, removing defaults in favor of arguments
      fields_with_defaults = fields_with_default_values.dup
      
      # check if the defaults is empty, or if there are no parameters for this 
      # instantiation, and if so, don't bother overriding defaults.
      unless fields_with_defaults.empty? || d.empty?
        d.each_key do |name|
          fields_with_defaults.delete(name.to_s)
        end
      end
      
      # assign all the user-specified arguments
      unless d.empty?
        d.each do |name, value|
          unless name_to_id(name.to_s)
            raise Exception, "Unknown key given to #{self.class}.new: #{name}"
          end
          Thrift.check_type(value, struct_fields[name_to_id(name.to_s)], name) if Thrift.type_checking
          instance_variable_set("@#{name}", value)
        end
      end
      
      # assign all the default values
      unless fields_with_defaults.empty?
        fields_with_defaults.each do |name, default_value|
          instance_variable_set("@#{name}", (default_value.dup rescue default_value))
        end
      end
    end

    def fields_with_default_values
      fields_with_default_values = self.class.instance_variable_get("@fields_with_default_values")
      unless fields_with_default_values
        fields_with_default_values = {}
        struct_fields.each do |fid, field_def|
          if field_def[:default]
            fields_with_default_values[field_def[:name]] = field_def[:default]
          end
        end
        self.class.instance_variable_set("@fields_with_default_values", fields_with_default_values)
      end
      fields_with_default_values
    end

    def name_to_id(name)
      names_to_ids = self.class.instance_variable_get("@names_to_ids")
      unless names_to_ids
        names_to_ids = {}
        struct_fields.each do |fid, field_def|
          names_to_ids[field_def[:name]] = fid
        end
        self.class.instance_variable_set("@names_to_ids", names_to_ids)
      end
      names_to_ids[name]
    end

    # Obsoleted by THRIFT-246, which generates this method inline
    # TODO: Should be removed at some point. -- Kevin Clark
    def struct_fields
      self.class.const_get(:FIELDS)
    end

    def each_field
      struct_fields.each do |fid, data|
        yield fid, data[:type], data[:name], data[:default], data[:optional]
      end
    end

    def inspect(skip_optional_nulls = true)
      fields = []
      each_field do |fid, type, name, default, optional|
        value = instance_variable_get("@#{name}")
        unless skip_optional_nulls && optional && value.nil?
          fields << "#{name}:#{value.inspect}"
        end
      end
      "<#{self.class} #{fields.join(", ")}>"
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
      validate
    end

    def write(oprot)
      validate
      # if oprot.respond_to?(:encode_binary)
      #   # TODO(kevinclark): Clean this so I don't have to access the transport.
      #   oprot.trans.write oprot.encode_binary(self)
      # else
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
      # end
    end

    def ==(other)
      return false unless other.is_a?(self.class)
      each_field do |fid, type, name, default|
        return false unless self.instance_variable_get("@#{name}") == other.instance_variable_get("@#{name}")
      end
      true
    end

    def eql?(other)
      self.class == other.class && self == other
    end

    # for the time being, we're ok with a naive hash. this could definitely be improved upon.
    def hash
      0
    end

    def self.field_accessor(klass, *fields)
      fields.each do |field|
        klass.send :attr_reader, field
        klass.send :define_method, "#{field}=" do |value|
          Thrift.check_type(value, klass::FIELDS.values.find { |f| f[:name].to_s == field.to_s }, field) if Thrift.type_checking
          instance_variable_set("@#{field}", value)
        end
      end
    end

    protected

    def self.append_features(mod)
      if mod.ancestors.include? ::Exception
        mod.send :class_variable_set, :'@@__thrift_struct_real_initialize', mod.instance_method(:initialize)
        super
        # set up our custom initializer so `raise Xception, 'message'` works
        mod.send :define_method, :struct_initialize, mod.instance_method(:initialize)
        mod.send :define_method, :initialize, mod.instance_method(:exception_initialize)
      else
        super
      end
    end

    def exception_initialize(*args, &block)
      if args.size == 1 and args.first.is_a? Hash
        # looks like it's a regular Struct initialize
        method(:struct_initialize).call(args.first)
      else
        # call the Struct initializer first with no args
        # this will set our field default values
        method(:struct_initialize).call()
        # now give it to the exception
        self.class.send(:class_variable_get, :'@@__thrift_struct_real_initialize').bind(self).call(*args, &block) if args.size > 0
        # self.class.instance_method(:initialize).bind(self).call(*args, &block)
      end
    end

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
