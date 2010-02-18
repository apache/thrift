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

module Thrift
  class Union
    def initialize(name=nil, value=nil)
      if name
        if name.is_a? Hash
          if name.size > 1
            raise "#{self.class} cannot be instantiated with more than one field!"
          end

          name, value = name.keys.first, name.values.first
        end

        if value.nil?
          raise Exception, "Union #{self.class} cannot be instantiated with setfield and nil value!"
        end

        Thrift.check_type(value, struct_fields[name_to_id(name.to_s)], name) if Thrift.type_checking
      elsif !value.nil?
        raise Exception, "Value provided, but no name!"
      end
      @setfield = name
      @value = value
    end

    def inspect
      "<#{self.class} #{@setfield}: #{inspect_field(@value, struct_fields[name_to_id(@setfield.to_s)])}>"
    end

    def read(iprot)
      iprot.read_struct_begin
      fname, ftype, fid = iprot.read_field_begin
      handle_message(iprot, fid, ftype)
      iprot.read_field_end

      fname, ftype, fid = iprot.read_field_begin
      raise "Too many fields for union" unless (ftype == Types::STOP) 

      iprot.read_struct_end
      validate
    end

    def write(oprot)
      validate
      oprot.write_struct_begin(self.class.name)

      fid = self.name_to_id(@setfield.to_s)

      field_info = struct_fields[fid]
      type = field_info[:type]
      if is_container? type
        oprot.write_field_begin(@setfield, type, fid)
        write_container(oprot, @value, field_info)
        oprot.write_field_end
      else
        oprot.write_field(@setfield, type, fid, @value)
      end

      oprot.write_field_stop
      oprot.write_struct_end
    end

    def ==(other)
      other != nil && @setfield == other.get_set_field && @value == other.get_value
    end

    def eql?(other)
      self.class == other.class && self == other
    end

    def hash
      [self.class.name, @setfield, @value].hash
    end

    def self.field_accessor(klass, *fields)
      fields.each do |field|
        klass.send :define_method, "#{field}" do
          if field == @setfield
            @value
          else 
            raise RuntimeError, "#{field} is not union's set field."
          end
        end

        klass.send :define_method, "#{field}=" do |value|
          Thrift.check_type(value, klass::FIELDS.values.find {|f| f[:name].to_s == field.to_s }, field) if Thrift.type_checking
          @setfield = field
          @value = value
        end
      end
    end

    # get the symbol that indicates what the currently set field type is. 
    def get_set_field
      @setfield
    end

    # get the current value of this union, regardless of what the set field is.
    # generally, you should only use this method when you don't know in advance
    # what field to expect.
    def get_value
      @value
    end

    protected

    def handle_message(iprot, fid, ftype)
      field = struct_fields[fid]
      if field and field[:type] == ftype
        @value = read_field(iprot, field)
        name = field[:name].to_sym
        @setfield = name
      else
        iprot.skip(ftype)
      end
    end
  end
end