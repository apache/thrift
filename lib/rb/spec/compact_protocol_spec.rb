require File.dirname(__FILE__) + '/spec_helper'
require "thrift/protocol/compact_protocol"

describe Thrift::CompactProtocol do
  TESTS = {
    :byte => (-127..127).to_a,
    :i16 => (0..14).map {|shift| [1 << shift, -(1 << shift)]}.flatten.sort,
    :i32 => (0..30).map {|shift| [1 << shift, -(1 << shift)]}.flatten.sort,
    :i64 => (0..62).map {|shift| [1 << shift, -(1 << shift)]}.flatten.sort,
    :string => ["", "1", "short", "fourteen123456", "fifteen12345678", "1" * 127, "1" * 3000],
    :binary => ["", "\001", "\001" * 5, "\001" * 14, "\001" * 15, "\001" * 127, "\001" * 3000],
    :double => [0.0, 1.0, -1.0, 1.1, -1.1, 10000000.1, 1.0/0.0, -1.0/0.0],
    :bool => [true, false]
  }
  
  it "should encode and decode naked primitives correctly" do
    TESTS.each_pair do |primitive_type, test_values|
      test_values.each do |value|
        # puts "testing #{value}" if primitive_type == :i64
        trans = Thrift::MemoryBuffer.new
        proto = Thrift::CompactProtocol.new(trans)
        
        proto.send(writer(primitive_type), value)
        # puts "buf: #{trans.inspect_buffer}" if primitive_type == :i64
        read_back = proto.send(reader(primitive_type))
        read_back.should == value
      end
    end
  end
  
  it "should encode and decode primitives in fields correctly" do
    TESTS.each_pair do |primitive_type, test_values|
      final_primitive_type = primitive_type == :binary ? :string : primitive_type
      thrift_type = Thrift::Types.const_get(final_primitive_type.to_s.upcase)
      # puts primitive_type
      test_values.each do |value|
        trans = Thrift::MemoryBuffer.new
        proto = Thrift::CompactProtocol.new(trans)

        proto.write_field_begin(nil, thrift_type, 15)
        proto.send(writer(primitive_type), value)
        proto.write_field_end

        proto = Thrift::CompactProtocol.new(trans)
        name, type, id = proto.read_field_begin
        type.should == thrift_type
        id.should == 15
        read_back = proto.send(reader(primitive_type))
        read_back.should == value
        proto.read_field_end
      end
    end
  end

  it "should encode and decode a monster struct correctly" do
    trans = Thrift::MemoryBuffer.new
    proto = Thrift::CompactProtocol.new(trans)

    struct = CompactProtoTestStruct.new
    # sets and maps don't hash well... not sure what to do here.
    struct.set_byte_map = nil
    struct.map_byte_map = nil
    struct.write(proto)
    
    # puts trans.inspect
    
    struct2 = CompactProtoTestStruct.new
    struct2.instance_variables.each do |ivar|
      struct2.instance_variable_set(ivar, nil)
    end
    
    struct2.should_not == struct

    struct2.read(proto)
    
    struct2.should == struct
  end

  it "should make method calls correctly" do
    client_out_trans = Thrift::MemoryBuffer.new
    client_out_proto = Thrift::CompactProtocol.new(client_out_trans)
    
    client_in_trans = Thrift::MemoryBuffer.new
    client_in_proto = Thrift::CompactProtocol.new(client_in_trans)
    
    processor = Srv::Processor.new(JankyHandler.new)
    
    client = Srv::Client.new(client_in_proto, client_out_proto)
    client.send_Janky(1)
    # puts client_out_trans.inspect_buffer
    processor.process(client_out_proto, client_in_proto)
    client.recv_Janky.should == 2
  end
  
  class JankyHandler
    def Janky(i32arg)
      i32arg * 2
    end
  end
  
  def writer(sym)
    sym = sym == :binary ? :string : sym
    "write_#{sym.to_s}"
  end
  
  def reader(sym)
    sym = sym == :binary ? :string : sym
    "read_#{sym.to_s}"
  end
end