module ThriftStruct
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
    iprot.readStructBegin()
    loop do
      fname, ftype, fid = iprot.readFieldBegin()
      break if (ftype === TType::STOP)
      handle_message(iprot, fid, ftype)
      iprot.readFieldEnd()
    end
    iprot.readStructEnd()
  end

  def write(oprot)
    oprot.writeStructBegin(self.class.name)
    each_field do |fid, type, name|
      if ((value = instance_variable_get("@#{name}")) != nil)
        if is_container? type
          oprot.writeFieldBegin(name, type, fid)
          write_container(oprot, value, struct_fields[fid])
          oprot.writeFieldEnd
        else
          oprot.write_field(name, type, fid, value)
        end
      end
    end
    oprot.writeFieldStop()
    oprot.writeStructEnd()
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
    if field[:type] == TType::STRUCT
      value = field[:class].new
      value.read(iprot)
    elsif field[:type] == TType::MAP
      key_type, val_type, size = iprot.readMapBegin
      value = {}
      size.times do
        k = read_field(iprot, field_info(field[:key]))
        v = read_field(iprot, field_info(field[:value]))
        value[k] = v
      end
      iprot.readMapEnd
    elsif field[:type] == TType::LIST
      e_type, size = iprot.readListBegin
      value = Array.new(size) do |n|
        read_field(iprot, field_info(field[:element]))
      end
      iprot.readListEnd
    elsif field[:type] == TType::SET
      e_type, size = iprot.readSetBegin
      value = {}
      size.times do
        element = read_field(iprot, field_info(field[:element]))
        value[element] = true
      end
      iprot.readSetEnd
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
    if field[:type] == TType::MAP
      oprot.writeMapBegin(field[:key][:type], field[:value][:type], value.size)
      value.each do |k, v|
        write_data(oprot, k, field[:key])
        write_data(oprot, v, field[:value])
      end
      oprot.writeMapEnd
    elsif field[:type] == TType::LIST
      oprot.writeListBegin(field[:element][:type], value.size)
      value.each do |elem|
        write_data(oprot, elem, field[:element])
      end
      oprot.writeListEnd
    elsif field[:type] == TType::SET
      oprot.writeSetBegin(field[:element][:type], value.size)
      value.each do |k, v|
        write_data(oprot, k, field[:element])
      end
      oprot.writeSetEnd
    else
      raise "Not a container type: #{field[:type]}"
    end
  end

  def is_container?(type)
    [TType::LIST, TType::MAP, TType::SET].include? type
  end

  def field_info(field)
    { :type => field[:type],
      :class => field[:class],
      :key => field[:key],
      :value => field[:value],
      :element => field[:element] }
  end
end
