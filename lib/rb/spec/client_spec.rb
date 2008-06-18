require File.dirname(__FILE__) + '/spec_helper'

class ThriftSpec < Spec::ExampleGroup
  include Thrift

  class ClientSpec
    include Thrift::Client
  end

  before(:each) do
    @prot = mock("MockProtocol")
    @client = ClientSpec.new(@prot)
  end

  describe "Client" do
    it "should re-use iprot for oprot if not otherwise specified" do
      @client.instance_variable_get(:'@iprot').should eql(@prot)
      @client.instance_variable_get(:'@oprot').should eql(@prot)
    end

    it "should send a test message" do
      @prot.should_receive(:write_message_begin).with('testMessage', MessageTypes::CALL, 0)
      mock_args = mock('#<TestMessage_args:mock>')
      mock_args.should_receive(:foo=).with('foo')
      mock_args.should_receive(:bar=).with(42)
      mock_args.should_receive(:write).with(@prot)
      @prot.should_receive(:write_message_end)
      @prot.should_receive(:trans) do
        mock('trans').tee do |trans|
          trans.should_receive(:flush)
        end
      end
      klass = stub("TestMessage_args", :new => mock_args)
      @client.send_message('testMessage', klass, :foo => 'foo', :bar => 42)
    end

    it "should receive a test message" do
      @prot.should_receive(:read_message_begin).and_return [nil, MessageTypes::CALL, 0]
      @prot.should_receive(:read_message_end)
      mock_klass = mock("#<MockClass:mock>")
      mock_klass.should_receive(:read).with(@prot)
      @client.receive_message(stub("MockClass", :new => mock_klass))
    end

    it "should handle received exceptions" do
      @prot.should_receive(:read_message_begin).and_return [nil, MessageTypes::EXCEPTION, 0]
      @prot.should_receive(:read_message_end)
      ApplicationException.should_receive(:new).and_return do
        StandardError.new.tee do |mock_exc|
          mock_exc.should_receive(:read).with(@prot)
        end
      end
      lambda { @client.receive_message(nil) }.should raise_error(StandardError)
    end
  end
end
