require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/protocol/binaryprotocol'

class ThriftBinaryProtocolSpec < Spec::ExampleGroup
  include Thrift

  describe BinaryProtocol do
    before(:each) do
      @trans = mock("MockTransport", :null_object => true)
      @prot = BinaryProtocol.new(@trans)
    end

    it "should define the proper VERSION_1 and VERSION_MASK" do
      BinaryProtocol::VERSION_MASK.should == 0xffff0000
      BinaryProtocol::VERSION_1.should == 0x80010000
    end

    it "should write the message header" do
      @prot.should_receive(:write_i32).with(BinaryProtocol::VERSION_1 | MessageTypes::CALL).ordered
      @prot.should_receive(:write_string).with('testMessage').ordered
      @prot.should_receive(:write_i32).with(17).ordered
      @prot.write_message_begin('testMessage', MessageTypes::CALL, 17)
    end

    # message footer is a noop

    it "should write the field header" do
      @prot.should_receive(:write_byte).with(Types::DOUBLE).ordered
      @prot.should_receive(:write_i16).with(3).ordered
      @prot.write_field_begin('foo', Types::DOUBLE, 3)
    end

    # field footer is a noop

    it "should write the STOP field" do
      @prot.should_receive(:write_byte).with(Types::STOP)
      @prot.write_field_stop
    end

    it "should write the map header" do
      @prot.should_receive(:write_byte).with(Types::STRING).ordered
      @prot.should_receive(:write_byte).with(Types::LIST).ordered
      @prot.should_receive(:write_i32).with(17).ordered
      @prot.write_map_begin(Types::STRING, Types::LIST, 17)
    end

    # map footer is a noop

    it "should write the list header" do
      @prot.should_receive(:write_byte).with(Types::I16).ordered
      @prot.should_receive(:write_i32).with(42).ordered
      @prot.write_list_begin(Types::I16, 42)
    end

    # list footer is a noop

    it "should write the set header" do
      @prot.should_receive(:write_byte).with(Types::BOOL).ordered
      @prot.should_receive(:write_i32).with(2).ordered
      @prot.write_set_begin(Types::BOOL, 2)
    end

    it "should write a bool" do
      @prot.should_receive(:write_byte).with(1).ordered
      @prot.write_bool(true)
      @prot.should_receive(:write_byte).with(0).ordered
      @prot.write_bool(false)
    end

    it "should write a byte" do
      # byte is small enough, let's check -128..127
      (-128..127).each do |i|
        @trans.should_receive(:write).with([i].pack('c')).ordered
        @prot.write_byte(i)
      end
      (-128..127).each do |i|
      end
      # handing it numbers out of signed range should clip
      @trans.rspec_verify
      (128..255).each do |i|
        @trans.should_receive(:write).with([i].pack('c')).ordered
        @prot.write_byte(i)
      end
      # and lastly, a Bignum is going to error out
      lambda { @prot.write_byte(2**65) }.should raise_error(RangeError)
    end

    it "should write an i16" do
      # try a random scattering of values
      # include the signed i16 minimum/maximum
      @trans.should_receive(:write).with("\200\000").ordered
      @trans.should_receive(:write).with("\374\000").ordered
      @trans.should_receive(:write).with("\000\021").ordered
      @trans.should_receive(:write).with("\000\000").ordered
      @trans.should_receive(:write).with("\330\360").ordered
      @trans.should_receive(:write).with("\006\273").ordered
      @trans.should_receive(:write).with("\177\377").ordered
      [-2**15, -1024, 17, 0, -10000, 1723, 2**15-1].each do |i|
        @prot.write_i16(i)
      end
      # and try something out of signed range, it should clip
      @trans.should_receive(:write).with("\200\005").ordered
      @prot.write_i16(2**15 + 5)
      # a Bignum should error
      lambda { @prot.write_i16(2**65) }.should raise_error(RangeError)
    end

    it "should write an i32" do
      # try a random scattering of values
      # include the signed i32 minimum/maximum
      @trans.should_receive(:write).with("\200\000\000\000").ordered
      @trans.should_receive(:write).with("\377\376\037\r").ordered
      @trans.should_receive(:write).with("\377\377\366\034").ordered
      @trans.should_receive(:write).with("\377\377\377\375").ordered
      @trans.should_receive(:write).with("\000\000\000\000").ordered
      @trans.should_receive(:write).with("\000#\340\203").ordered
      @trans.should_receive(:write).with("\000\0000+").ordered
      @trans.should_receive(:write).with("\177\377\377\377").ordered
      [-2**31, -123123, -2532, -3, 0, 2351235, 12331, 2**31-1].each do |i|
        @prot.write_i32(i)
      end
      # try something out of signed range, it should clip
      @trans.should_receive(:write).with("\200\000\000\005").ordered
      @prot.write_i32(2 ** 31 + 5)
      lambda { @prot.write_i32(2 ** 65 + 5) }.should raise_error(RangeError)
    end

    it "should write an i64" do
      # try a random scattering of values
      # try the signed i64 minimum/maximum
      @trans.should_receive(:write).with("\200\000\000\000\000\000\000\000").ordered
      @trans.should_receive(:write).with("\377\377\364\303\035\244+]").ordered
      @trans.should_receive(:write).with("\377\377\377\377\376\231:\341").ordered
      @trans.should_receive(:write).with("\377\377\377\377\377\377\377\026").ordered
      @trans.should_receive(:write).with("\000\000\000\000\000\000\000\000").ordered
      @trans.should_receive(:write).with("\000\000\000\000\000\000\004\317").ordered
      @trans.should_receive(:write).with("\000\000\000\000\000#\340\204").ordered
      @trans.should_receive(:write).with("\000\000\000\002\340\311~\365").ordered
      @trans.should_receive(:write).with("\177\377\377\377\377\377\377\377").ordered
      [-2**63, -12356123612323, -23512351, -234, 0, 1231, 2351236, 12361236213, 2**63-1].each do |i|
        @prot.write_i64(i)
      end
      # try something out of signed range, it should clip
      @trans.should_receive(:write).with("\200\000\000\000\000\000\000\005").ordered
      @prot.write_i64(2**63 + 5)
      lambda { @prot.write_i64(2 ** 65 + 5) }.should raise_error(RangeError)
    end

    it "should write a double" do
      # try a random scattering of values, including min/max
      @trans.should_receive(:write).with("\000\020\000\000\000\000\000\000").ordered
      @trans.should_receive(:write).with("\300\223<\234\355\221hs").ordered
      @trans.should_receive(:write).with("\300\376\0173\256\024z\341").ordered
      @trans.should_receive(:write).with("\3007<2\336\372v\324").ordered
      @trans.should_receive(:write).with("\000\000\000\000\000\000\000\000").ordered
      @trans.should_receive(:write).with("@\310\037\220\365\302\217\\").ordered
      @trans.should_receive(:write).with("@\200Y\327\n=p\244").ordered
      @trans.should_receive(:write).with("\177\357\377\377\377\377\377\377").ordered
      [Float::MIN, -1231.15325, -123123.23, -23.23515123, 0, 12351.1325, 523.23, Float::MAX].each do |f|
        @prot.write_double(f)
      end
    end

    it "should write a string" do
      str = "hello world"
      @prot.should_receive(:write_i32).with(str.length).ordered
      @trans.should_receive(:write).with(str).ordered
      @prot.write_string(str)
    end

    it "should read a message header" do
      @prot.should_receive(:read_i32).and_return(BinaryProtocol::VERSION_1 | MessageTypes::REPLY, 42)
      @prot.should_receive(:read_string).and_return('testMessage')
      @prot.read_message_begin.should == ['testMessage', MessageTypes::REPLY, 42]
    end

    it "should raise an exception if the message header has the wrong version" do
      @prot.should_receive(:read_i32).and_return(42)
      lambda { @prot.read_message_begin }.should raise_error(ProtocolException, 'Missing version identifier') { |e| e.type == ProtocolException::BAD_VERSION }
    end

    # message footer is a noop

    it "should read a field header" do
      @prot.should_receive(:read_byte).ordered.and_return(Types::STRING)
      @prot.should_receive(:read_i16).ordered.and_return(3)
      @prot.read_field_begin.should == [nil, Types::STRING, 3]
    end

    # field footer is a noop

    it "should read a stop field" do
      @prot.should_receive(:read_byte).and_return(Types::STOP)
      @prot.should_not_receive(:read_i16)
      @prot.read_field_begin.should == [nil, Types::STOP, 0]
    end

    it "should read a map header" do
      @prot.should_receive(:read_byte).and_return(Types::DOUBLE, Types::I64)
      @prot.should_receive(:read_i32).and_return(42)
      @prot.read_map_begin.should == [Types::DOUBLE, Types::I64, 42]
    end

    # map footer is a noop

    it "should read a list header" do
      @prot.should_receive(:read_byte).ordered.and_return(Types::STRING)
      @prot.should_receive(:read_i32).and_return(17)
      @prot.read_list_begin.should == [Types::STRING, 17]
    end

    # list footer is a noop

    it "should read a set header" do
      @prot.should_receive(:read_byte).ordered.and_return(Types::MAP)
      @prot.should_receive(:read_i32).ordered.and_return(42)
      @prot.read_set_begin.should == [Types::MAP, 42]
    end

    # set footer is a noop

    it "should read a bool" do
      @prot.should_receive(:read_byte).and_return(1, 0)
      @prot.read_bool.should == true
      @prot.read_bool.should == false
    end

    it "should read a byte" do
      # try a scattering of values, including min/max
      @trans.should_receive(:read_all).with(1).and_return(
        "\200", "\307", "\375",
        "\000", "\021", "\030", "\177"
      )
      [-128, -57, -3, 0, 17, 24, 127].each do |i|
        @prot.read_byte.should == i
      end
    end

    it "should read an i16" do
      # try a scattering of values, including min/max
      @trans.should_receive(:read_all).with(2).and_return(
        "\200\000", "\353\213", "\376\237",
        "\000\000", "\005\367", "\b\272", "\177\377"
      )
      [-2**15, -5237, -353, 0, 1527, 2234, 2**15-1].each do |i|
        @prot.read_i16.should == i
      end
    end

    it "should read an i32" do
      # try a scattering of values, including min/max
      @trans.should_receive(:read_all).with(4).and_return(
        "\200\000\000\000", "\377\374i\213", "\377\377\347\244",
        "\000\000\000\000", "\000\000\t/", "\000\001\340\363", "\177\377\377\377"
      )
      [-2**31, -235125, -6236, 0, 2351, 123123, 2**31-1].each do |i|
        @prot.read_i32.should == i
      end
    end

    it "should read an i64" do
      # try a scattering of values, including min/max
      @trans.should_receive(:read_all).with(8).and_return(
        "\200\000\000\000\000\000\000\000", "\377\377\377\377\370\243Z\b",
        "\377\377\377\377\377\377\3476", "\000\000\000\000\000\000\000\000",
        "\000\000\000\000\000\000\000 ", "\000\000\000\000\213\332\t\223",
        "\177\377\377\377\377\377\377\377"
      )
      [-2**63, -123512312, -6346, 0, 32, 2346322323, 2**63-1].each do |i|
        @prot.read_i64.should == i
      end
    end

    it "should read a double" do
      # try a random scattering of values, including min/max
      @trans.should_receive(:read_all).with(8).and_return(
        "\000\020\000\000\000\000\000\000", "\301\f9\370\374\362\317\226",
        "\300t3\274x \243\016", "\000\000\000\000\000\000\000\000", "@^\317\fCo\301Y",
        "AA\360A\217\317@\260", "\177\357\377\377\377\377\377\377"
      )
      [Float::MIN, -231231.12351, -323.233513, 0, 123.2351235, 2351235.12351235, Float::MAX].each do |f|
        @prot.read_double.should == f
      end
    end

    it "should read a string" do
      str = "hello world"
      @prot.should_receive(:read_i32).and_return(str.length)
      @trans.should_receive(:read_all).with(str.length).and_return(str)
      @prot.read_string.should == str
    end
  end

  describe BinaryProtocolFactory do
    it "should create a BinaryProtocol" do
      BinaryProtocolFactory.new.get_protocol(mock("MockTransport")).should be_instance_of(BinaryProtocol)
    end
  end
end
