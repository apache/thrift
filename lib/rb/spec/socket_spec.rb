require File.dirname(__FILE__) + '/spec_helper'
require File.dirname(__FILE__) + "/socket_spec_shared"

class ThriftSocketSpec < Spec::ExampleGroup
  include Thrift

  describe Socket do
    before(:each) do
      @socket = Socket.new
      @handle = mock("Handle", :closed? => false)
      @handle.stub!(:close)
      TCPSocket.stub!(:new).and_return(@handle)
    end

    it_should_behave_like "a socket"

    it "should raise a TransportException when it cannot open a socket" do
      TCPSocket.should_receive(:new).and_raise(StandardError)
      lambda { @socket.open }.should raise_error(Thrift::TransportException) { |e| e.type.should == Thrift::TransportException::NOT_OPEN }
    end

    it "should open a TCPSocket with default args" do
      TCPSocket.should_receive(:new).with('localhost', 9090)
      @socket.open
    end

    it "should accept host/port options" do
      TCPSocket.should_receive(:new).with('my.domain', 1234)
      Socket.new('my.domain', 1234).open
    end
  end

  describe ServerSocket do
    before(:each) do
      @socket = ServerSocket.new(1234)
    end

    it "should create a handle when calling listen" do
      TCPServer.should_receive(:new).with(nil, 1234)
      @socket.listen
    end

    it "should accept an optional host argument" do
      @socket = ServerSocket.new('localhost', 1234)
      TCPServer.should_receive(:new).with('localhost', 1234)
      @socket.listen
    end

    it "should create a Thrift::Socket to wrap accepted sockets" do
      handle = mock("TCPServer")
      TCPServer.should_receive(:new).with(nil, 1234).and_return(handle)
      @socket.listen
      sock = mock("sock")
      handle.should_receive(:accept).and_return(sock)
      trans = mock("Socket")
      Socket.should_receive(:new).and_return(trans)
      trans.should_receive(:handle=).with(sock)
      @socket.accept.should == trans
    end

    it "should close the handle when closed" do
      handle = mock("TCPServer", :closed? => false)
      TCPServer.should_receive(:new).with(nil, 1234).and_return(handle)
      @socket.listen
      handle.should_receive(:close)
      @socket.close
    end

    it "should return nil when accepting if there is no handle" do
      @socket.accept.should be_nil
    end
  end
end
