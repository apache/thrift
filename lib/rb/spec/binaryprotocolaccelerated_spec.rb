require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/protocol/binaryprotocolaccelerated'
require File.dirname(__FILE__) + '/binaryprotocol_spec_shared'
require File.dirname(__FILE__) + '/gen-rb/ThriftSpec_types'

class ThriftBinaryProtocolAcceleratedSpec < Spec::ExampleGroup
  include Thrift

  describe Thrift::BinaryProtocolAccelerated do
    # since BinaryProtocolAccelerated should be directly equivalent to 
    # BinaryProtocol, we don't need any custom specs!
    it_should_behave_like 'a binary protocol'

    def protocol_class
      BinaryProtocolAccelerated
    end

#     before(:each) do
#       @buffer = ""
#       @trans.stub!(:borrow).and_return { @buffer }
#       @trans.stub!(:consume!).and_return do |*args|
#         n = args.first || 0
#         @buffer.slice!(0,n)
#       end
#     end
# 
# 
#     it "should raise an exception if the message header has the wrong version" do
#       @buffer = "\000\000\000\v"
#       # @prot.should_receive(:read_i32).and_return(42)
#       lambda { @prot.read_message_begin }.should raise_error(Thrift::ProtocolException, 'Missing version identifier') do |e|
#         e.type == Thrift::ProtocolException::BAD_VERSION
#       end
#     end
# 
#     it "should encode a string with null bytes in it" do
#       foo = SpecNamespace::Hello.new(:greeting => "Hello\000World!")
#       @prot.encode_binary(foo).should == "\v\000\001\000\000\000\fHello\000World!\000"
#     end
# 
#     it "should decode a string with null bytes in it" do
#       trans = Thrift::MemoryBuffer.new("\v\000\001\000\000\000\fHello\000World!\000")
#       @prot.decode_binary(SpecNamespace::Hello.new, trans).should == SpecNamespace::Hello.new(:greeting => "Hello\000World!")
#     end
# 
#     it "should error when encoding a struct with a nil value in a list" do
#       Thrift.type_checking = false
#       sl = SpecNamespace::SimpleList
#       hello = SpecNamespace::Hello
#       # nil counts as false for bools
#       # lambda { @prot.encode_binary(sl.new(:bools => [true, false, nil, false])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:bytes => [1, 2, nil, 3])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:i16s => [1, 2, nil, 3])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:i32s => [1, 2, nil, 3])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:i64s => [1, 2, nil, 3])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:doubles => [1.0, 2.0, nil, 3.0])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:strings => ["one", "two", nil, "three"])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:lists => [[1, 2], nil, [3, 4]])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:maps => [{1 => 2}, nil, {3 => 4}])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:sets => [Set.new([1, 2]), nil, Set.new([3, 4])])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:structs => [hello.new, nil, hello.new(:greeting => "hi")])) }.should raise_error
#     end
# 
#     it "should error when encoding a non-nil, non-correctly-typed value in a list" do
#       Thrift.type_checking = false
#       sl = SpecNamespace::SimpleList
#       hello = SpecNamespace::Hello
#       # bool should accept any value
#       # lambda { @prot.encode_binary(sl.new(:bools => [true, false, 3])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:bytes => [1, 2, "3", 5])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:i16s => ["one", 2, 3])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:i32s => [[1,2], 3, 4])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:i64s => [{1 => 2}, 3, 4])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:doubles => ["one", 2.3, 3.4])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:strings => ["one", "two", 3, 4])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:lists => [{1 => 2}, [3, 4]])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:maps => [{1 => 2}, [3, 4]])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:sets => [Set.new([1, 2]), 3, 4])) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:structs => [3, "four"])) }.should raise_error
#     end
# 
#     it "should error when encoding an improper object where a container is expected" do
#       Thrift.type_checking = false
#       sl = SpecNamespace::SimpleList
#       lambda { @prot.encode_binary(sl.new(:strings => {"one" => "two", nil => "three"})) }.should raise_error
#       lambda { @prot.encode_binary(sl.new(:maps => [[1, 2]])) }.should raise_error
#     end
# 
#     it "should accept arrays and hashes as sets" do
#       Thrift.type_checking = false
#       sl = SpecNamespace::SimpleList
#       lambda { @prot.encode_binary(sl.new(:sets => [[1, 2], {3 => true, 4 => true}])) }.should_not raise_error
#     end
  end

  describe BinaryProtocolAcceleratedFactory do
    it "should create a BinaryProtocolAccelerated" do
      BinaryProtocolAcceleratedFactory.new.get_protocol(mock("MockTransport")).should be_instance_of(BinaryProtocolAccelerated)
    end
  end
end
