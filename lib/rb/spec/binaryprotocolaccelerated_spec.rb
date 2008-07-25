require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/protocol/binaryprotocolaccelerated'
require File.dirname(__FILE__) + '/binaryprotocol_spec_shared'
require File.dirname(__FILE__) + '/gen-rb/ThriftSpec_types'

class ThriftBinaryProtocolAcceleratedSpec < Spec::ExampleGroup
  include Thrift

  describe BinaryProtocolAccelerated do
    # given that BinaryProtocolAccelerated only actually overrides read_message_begin
    # this shared spec isn't going to do much, but it's still worth including
    # for future-proofing in case we start overriding individual methods
    it_should_behave_like 'a binary protocol'

    def protocol_class
      BinaryProtocolAccelerated
    end

    before(:each) do
      @buffer = ""
      @trans.stub!(:borrow).and_return { @buffer }
      @trans.stub!(:consume!).and_return do |*args|
        n = args.first || 0
        @buffer.slice!(0,n)
      end
    end

    it "should read a message header" do
      @buffer = "\200\001\000\002\000\000\000\vtestMessage\000\000\000*"
      # @prot.should_receive(:read_i32).and_return(protocol_class.const_get(:VERSION_1) | Thrift::MessageTypes::REPLY, 42)
      # @prot.should_receive(:read_string).and_return('testMessage')
      @prot.read_message_begin.should == ['testMessage', Thrift::MessageTypes::REPLY, 42]
    end

    it "should raise an exception if the message header has the wrong version" do
      @buffer = "\000\000\000\v"
      # @prot.should_receive(:read_i32).and_return(42)
      lambda { @prot.read_message_begin }.should raise_error(Thrift::ProtocolException, 'Missing version identifier') do |e|
        e.type == Thrift::ProtocolException::BAD_VERSION
      end
    end

    it "should encode a struct with all fields set identically to Thrift::BinaryProtocol" do
      foo = SpecNamespace::Foo.new(:complex => {5 => {"foo" => 1.2}, 17 => {"bar" => 3.14159, "baz" => 5.8}})
      @prot.encode_binary(foo).should == "\r\000\005\b\r\000\000\000\002\000\000\000\005\v\004\000\000\000\001\000\
\000\000\003foo?\363333333\000\000\000\021\v\004\000\000\000\002\000\000\000\003baz@\027333333\000\000\000\003bar@\
\t!\371\360\e\206n\016\000\006\006\000\000\000\003\000\005\000\021\000\357\b\000\001\000\000\0005\v\000\002\000\000\
\000\005words\f\000\003\v\000\001\000\000\000\rhello, world!\000\017\000\004\b\000\000\000\004\000\000\000\001\000\
\000\000\002\000\000\000\002\000\000\000\003\000"
    end

    it "should encode a struct with missing fields identically to Thrift::BinaryProtocol" do
      foo = SpecNamespace::Foo.new(:simple => nil, :ints => nil)
      @prot.encode_binary(foo).should == "\016\000\006\006\000\000\000\003\000\005\000\021\000\357\v\000\002\000\000\
\000\005words\f\000\003\v\000\001\000\000\000\rhello, world!\000\000"
    end

    it "should decode a struct with all fields set identically to Thrift::BinaryProtocol" do
      foo = SpecNamespace::Foo.new(:complex => {5 => {"foo" => 1.2}, 17 => {"bar" => 3.14159, "baz" => 5.8}})
      trans = Thrift::MemoryBuffer.new("\r\000\005\b\r\000\000\000\002\000\000\000\005\v\004\000\000\000\001\000\
\000\000\003foo?\363333333\000\000\000\021\v\004\000\000\000\002\000\000\000\003baz@\027333333\000\000\000\003bar@\
\t!\371\360\e\206n\016\000\006\006\000\000\000\003\000\005\000\021\000\357\b\000\001\000\000\0005\v\000\002\000\000\
\000\005words\f\000\003\v\000\001\000\000\000\rhello, world!\000\017\000\004\b\000\000\000\004\000\000\000\001\000\
\000\000\002\000\000\000\002\000\000\000\003\000")
      @prot.decode_binary(SpecNamespace::Foo.new, trans).should == foo
    end

    it "should decode a struct with missing fields identically to Thrift::BinaryProtocol" do
      trans = Thrift::MemoryBuffer.new("\016\000\006\006\000\000\000\003\000\005\000\021\000\357\v\000\002\000\000\
\000\005words\f\000\003\v\000\001\000\000\000\rhello, world!\000\000")
      @prot.decode_binary(SpecNamespace::Foo.new, trans).should == SpecNamespace::Foo.new
    end

    it "should encode a string with null bytes in it" do
      foo = SpecNamespace::Hello.new(:greeting => "Hello\000World!")
      @prot.encode_binary(foo).should == "\v\000\001\000\000\000\fHello\000World!\000"
    end

    it "should decode a string with null bytes in it" do
      trans = Thrift::MemoryBuffer.new("\v\000\001\000\000\000\fHello\000World!\000")
      @prot.decode_binary(SpecNamespace::Hello.new, trans).should == SpecNamespace::Hello.new(:greeting => "Hello\000World!")
    end
  end

  describe BinaryProtocolAcceleratedFactory do
    it "should create a BinaryProtocolAccelerated" do
      BinaryProtocolAcceleratedFactory.new.get_protocol(mock("MockTransport")).should be_instance_of(BinaryProtocolAccelerated)
    end
  end
end
