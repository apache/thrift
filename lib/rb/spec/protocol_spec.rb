require File.dirname(__FILE__) + '/spec_helper'

class ThriftSpec < Spec::ExampleGroup
  include Thrift

  before(:each) do
    @prot = Protocol.new(mock("MockTransport"))
  end

  describe Protocol do
    # most of the methods are stubs, so we can ignore them
    it "should write out a field nicely" do
      @prot.should_receive(:write_field_begin).with('field', 'type', 'fid').ordered
      @prot.should_receive(:write_type).with('type', 'value').ordered
      @prot.should_receive(:write_field_end).ordered
      @prot.write_field('field', 'type', 'fid', 'value')
    end

    it "should write out the different types" do
      @prot.should_receive(:write_bool).with('bool').ordered
      @prot.should_receive(:write_byte).with('byte').ordered
      @prot.should_receive(:write_double).with('double').ordered
      @prot.should_receive(:write_i16).with('i16').ordered
      @prot.should_receive(:write_i32).with('i32').ordered
      @prot.should_receive(:write_i64).with('i64').ordered
      @prot.should_receive(:write_string).with('string').ordered
      struct = mock('Struct')
      struct.should_receive(:write).with(@prot).ordered
      @prot.write_type(Types::BOOL, 'bool')
      @prot.write_type(Types::BYTE, 'byte')
      @prot.write_type(Types::DOUBLE, 'double')
      @prot.write_type(Types::I16, 'i16')
      @prot.write_type(Types::I32, 'i32')
      @prot.write_type(Types::I64, 'i64')
      @prot.write_type(Types::STRING, 'string')
      @prot.write_type(Types::STRUCT, struct)
      # all other types are not implemented
      [Types::STOP, Types::VOID, Types::MAP, Types::SET, Types::LIST].each do |type|
        lambda { @prot.write_type(type, type.to_s) }.should raise_error(NotImplementedError)
      end
    end

    it "should read the different types" do
      @prot.should_receive(:read_bool).ordered
      @prot.should_receive(:read_byte).ordered
      @prot.should_receive(:read_i16).ordered
      @prot.should_receive(:read_i32).ordered
      @prot.should_receive(:read_i64).ordered
      @prot.should_receive(:read_double).ordered
      @prot.should_receive(:read_string).ordered
      @prot.read_type(Types::BOOL)
      @prot.read_type(Types::BYTE)
      @prot.read_type(Types::I16)
      @prot.read_type(Types::I32)
      @prot.read_type(Types::I64)
      @prot.read_type(Types::DOUBLE)
      @prot.read_type(Types::STRING)
      # all other types are not implemented
      [Types::STOP, Types::VOID, Types::MAP, Types::SET, Types::LIST].each do |type|
        lambda { @prot.read_type(type) }.should raise_error(NotImplementedError)
      end
    end

    it "should skip the basic types" do
      @prot.should_receive(:read_bool).ordered
      @prot.should_receive(:read_byte).ordered
      @prot.should_receive(:read_i16).ordered
      @prot.should_receive(:read_i32).ordered
      @prot.should_receive(:read_i64).ordered
      @prot.should_receive(:read_double).ordered
      @prot.should_receive(:read_string).ordered
      @prot.read_type(Types::BOOL)
      @prot.read_type(Types::BYTE)
      @prot.read_type(Types::I16)
      @prot.read_type(Types::I32)
      @prot.read_type(Types::I64)
      @prot.read_type(Types::DOUBLE)
      @prot.read_type(Types::STRING)
    end

    it "should skip structs" do
      real_skip = @prot.method(:skip)
      @prot.should_receive(:read_struct_begin).ordered
      @prot.should_receive(:read_field_begin).exactly(4).times.and_return(
        ['field 1', Types::STRING, 1],
        ['field 2', Types::I32, 2],
        ['field 3', Types::MAP, 3],
        [nil, Types::STOP, 0]
      )
      @prot.should_receive(:skip).with(Types::STRING).ordered
      @prot.should_receive(:skip).with(Types::I32).ordered
      @prot.should_receive(:skip).with(Types::MAP).ordered
      @prot.should_receive(:read_field_end).exactly(3).times
      @prot.should_receive(:read_struct_end).ordered
      real_skip.call(Types::STRUCT)
    end
  end
end
