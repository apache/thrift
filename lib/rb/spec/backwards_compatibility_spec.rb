require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/protocol/binaryprotocol'
require 'thrift/server/httpserver'
require 'thrift/transport/httpclient'

context "Backwards compatibility" do
  specify "old class names should map to new classes" do
    # use an Array because a Hash will call #hash and trigger deprecation warnings
    klasses = [
      [:module, :ThriftClient,               Thrift::Client],
      [:class,  :TException,                 Thrift::Exception],
      [:class,  :TApplicationException,      Thrift::ApplicationException],
      [:module, :TProcessor,                 Thrift::Processor],
      [:class,  :TProtocol,                  Thrift::Protocol],
      [:class,  :TProtocolFactory,           Thrift::ProtocolFactory],
      [:class,  :TBinaryProtocol,            Thrift::BinaryProtocol],
      [:class,  :TBinaryProtocolFactory,     Thrift::BinaryProtocolFactory],
      [:class,  :TSimpleMongrelHTTPServer,   Thrift::SimpleMongrelHTTPServer],
      [:class,  :TServer,                    Thrift::Server],
      [:class,  :TSimpleServer,              Thrift::SimpleServer],
      [:class,  :TThreadedServer,            Thrift::ThreadedServer],
      [:class,  :TThreadPoolServer,          Thrift::ThreadPoolServer],
      [:module, :ThriftStruct,               Thrift::Struct],
      [:class,  :THttpClient,                Thrift::HTTPClient],
      [:class,  :TSocket,                    Thrift::Socket],
      [:class,  :TServerSocket,              Thrift::ServerSocket],
      [:class,  :TTransportException,        Thrift::TransportException],
      [:class,  :TTransport,                 Thrift::Transport],
      [:class,  :TServerTransport,           Thrift::ServerTransport],
      [:class,  :TTransportFactory,          Thrift::TransportFactory],
      [:class,  :TBufferedTransport,         Thrift::BufferedTransport],
      [:class,  :TBufferedTransportFactory,  Thrift::BufferedTransportFactory],
      [:class,  :TFramedTransport,           Thrift::FramedTransport],
      [:class,  :TFramedTransportFactory,    Thrift::FramedTransportFactory],
      [:class,  :TMemoryBuffer,              Thrift::MemoryBuffer],
      [:class,  :TIOStreamTransport,         Thrift::IOStreamTransport],
      [:module, :TType,                      Thrift::Types],
      [:module, :TMessageType,               Thrift::MessageTypes]
    ]
    klasses.each do |(type, oldname, newklass)|
      oldklass = Object.const_get(oldname)
      STDERR.should_receive(:puts).with("Warning: #{type} #{oldname} is deprecated").ordered
      STDERR.should_receive(:puts).with("  from #{__FILE__}:#{__LINE__+1}").ordered
      oldklass.should eql(newklass)
      STDERR.rspec_verify
      STDERR.rspec_reset
    end
  end

  specify "old method names should map to new method names" do
    mapping = {
      Thrift::Protocol => {
        :writeMessageBegin => :write_message_begin,
        :writeMessageEnd => :write_message_end,
        :writeStructBegin => :write_struct_begin,
        :writeStructEnd => :write_struct_end,
        :writeFieldBegin => :write_field_begin,
        :writeFieldEnd => :write_field_end,
        :writeFieldStop => :write_field_stop,
        :writeMapBegin => :write_map_begin,
        :writeMapEnd => :write_map_end,
        :writeListBegin => :write_list_begin,
        :writeListEnd => :write_list_end,
        :writeSetBegin => :write_set_begin,
        :writeSetEnd => :write_set_end,
        :writeBool => :write_bool,
        :writeByte => :write_byte,
        :writeI16 => :write_i16,
        :writeI32 => :write_i32,
        :writeI64 => :write_i64,
        :writeDouble => :write_double,
        :writeString => :write_string,
        :readMessageBegin => :read_message_begin,
        :readMessageEnd => :read_message_end,
        :readStructBegin => :read_struct_begin,
        :readStructEnd => :read_struct_end,
        :readFieldBegin => :read_field_begin,
        :readFieldEnd => :read_field_end,
        :readMapBegin => :read_map_begin,
        :readMapEnd => :read_map_end,
        :readListBegin => :read_list_begin,
        :readListEnd => :read_list_end,
        :readSetBegin => :read_set_begin,
        :readSetEnd => :read_set_end,
        :readBool => :read_bool,
        :readByte => :read_byte,
        :readI16 => :read_i16,
        :readI32 => :read_i32,
        :readI64 => :read_i64,
        :readDouble => :read_double,
        :readString => :read_string
      },
      Thrift::ProtocolFactory => {
        :getProtocol => :get_protocol
      },
      Thrift::Transport => {
        :isOpen => :open?,
        :is_open? => :open?,
        :readAll => :read_all
      },
      Thrift::TransportFactory => {
        :getTransport => :get_transport
      }
    }
    mapping.each_pair do |klass, methods|
      methods.each_pair do |oldmeth, newmeth|
        # at the moment there's no way to introspect the deprecated methods
        # to make sure they point to the new ones
        # so let's just make sure both old and new methods exist
        klass.should be_method_defined(oldmeth)
        klass.should be_method_defined(newmeth)
      end
    end
  end
end
