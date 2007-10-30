#!/usr/bin/env ruby
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#

class TType
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

class TMessageType
  CALL = 1
  REPLY = 2
  EXCEPTION = 3
end

module TProcessor
  def initialize(handler)
    @handler = handler
  end

  def process(iprot, oprot)
    name, type, seqid  = iprot.readMessageBegin()
    if respond_to?("process_#{name}")
      send("process_#{name}", seqid, iprot, oprot)
      return true
    else
      iprot.skip(TType::STRUCT)
      iprot.readMessageEnd()
      x = TApplicationException.new(TApplicationException::UNKNOWN_METHOD, 'Unknown function '+name)
      oprot.writeMessageBegin(name, TMessageType::EXCEPTION, seqid)
        x.write(oprot)
      oprot.writeMessageEnd()
      oprot.trans.flush()
      return
    end
  end
  
  def read_args(iprot, args_class)
    args = args_class.new
    args.read(iprot)
    iprot.readMessageEnd
    args
  end
  
  def write_result(result, oprot, name, seqid)
    oprot.writeMessageBegin(name, TMessageType::REPLY, seqid)
    result.write(oprot)
    oprot.writeMessageEnd()
    oprot.trans.flush()
  end
end

class TException < StandardError
  def initialize(message)
    super(message)
    @message = message
  end

  attr_reader :message
end

class TApplicationException < TException

  UNKNOWN = 0
  UNKNOWN_METHOD = 1
  INVALID_MESSAGE_TYPE = 2
  WRONG_METHOD_NAME = 3
  BAD_SEQUENCE_ID = 4
  MISSING_RESULT = 5
  
  attr_reader :type

  def initialize(type=UNKNOWN, message=nil)
    super(message)
    @type = type
  end

  def read(iprot)
    iprot.readStructBegin()
    while true
      fname, ftype, fid = iprot.readFieldBegin()
      if (ftype === TType::STOP)
        break
      end
      if (fid == 1)
        if (ftype === TType::STRING)
          @message = iprot.readString();
        else
          iprot.skip(ftype)
        end
      elsif (fid == 2)
        if (ftype === TType::I32)
          @type = iprot.readI32();
        else
          iprot.skip(ftype)
        end
      else
        iprot.skip(ftype)
      end
      iprot.readFieldEnd()
    end
    iprot.readStructEnd()
  end

  def write(oprot)
    oprot.writeStructBegin('TApplicationException')
    if (@message != nil)
      oprot.writeFieldBegin('message', TType::STRING, 1)
      oprot.writeString(@message)
      oprot.writeFieldEnd()
    end
    if (@type != nil)
      oprot.writeFieldBegin('type', TType::I32, 2)
      oprot.writeI32(@type)
      oprot.writeFieldEnd()
    end
    oprot.writeFieldStop()
    oprot.writeStructEnd()
  end

end

module ThriftClient
  def initialize(iprot, oprot=nil)
    @iprot = iprot
    @oprot = oprot || iprot
    @seqid = 0
  end

  def send_message(name, args_class, args = {})
    @oprot.writeMessageBegin(name, TMessageType::CALL, @seqid)
    data = args_class.new
    args.each do |k, v|
      data.send("#{k.to_s}=", v)
    end
    data.write(@oprot)
    @oprot.writeMessageEnd()
    @oprot.trans.flush()
  end
  
  def receive_message(result_klass)
    fname, mtype, rseqid = @iprot.readMessageBegin()
    handle_exception(mtype)
    result = result_klass.new
    result.read(@iprot)
    @iprot.readMessageEnd()
    return result
  end
  
  def handle_exception(mtype)
    if mtype == TMessageType::EXCEPTION
      x = TApplicationException.new()
      x.read(@iprot)
      @iprot.readMessageEnd()
      raise x
    end
  end
end

module ThriftStruct
  def initialize(d={})
    each_field do |fid, type, name|
      instance_variable_set("@#{name}", d[name.to_s])
    end
  end
  
  def fields
    self.class.const_get(:FIELDS)
  end
  
  def each_field
    fields.each do |fid, data|
      yield fid, data[:type], data[:name]
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
          write_container(oprot, value, fields[fid])
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
    field = fields[fid]
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
