require File.dirname(__FILE__) + '/spec_helper'

class ThriftSocketSpec < Spec::ExampleGroup
  include Thrift

  before(:each) do
    @socket = Socket.new
    @handle = mock("Handle")
  end

  describe Socket do
    it "should open a TCPSocket" do
      TCPSocket.should_receive(:new).with('localhost', 9090).and_return(@handle)
      @socket.open.should == @handle
    end

    it "should accept host/port options" do
      TCPSocket.should_receive(:new).with('my.domain', 1234)
      Socket.new('my.domain', 1234).open
    end

    it "should raise a TransportException when it cannot open a socket" do
      TCPSocket.should_receive(:new).with('localhost', 9090).and_raise(StandardError)
      lambda { @socket.open }.should raise_error(TransportException, "Could not connect to localhost:9090") { |e| e.type.should == TransportException::NOT_OPEN }
    end

    it "should be open whenever it has a handle" do
      @socket.should_not be_open
      TCPSocket.should_receive(:new).and_return(@handle)
      @socket.open
      @socket.should be_open
      @socket.set_handle nil
      @socket.should_not be_open
      @socket.set_handle @handle
      @handle.should_receive(:close)
      @socket.close
      @socket.should_not be_open
    end

    it "should write data to the handle" do
      TCPSocket.should_receive(:new).and_return(@handle)
      @socket.open
      @handle.should_receive(:write).with("foobar")
      @socket.write("foobar")
      @handle.should_receive(:write).with("fail").and_raise(StandardError)
      lambda { @socket.write("fail") }.should raise_error(TransportException) { |e| e.type.should == TransportException::NOT_OPEN }
    end

    it "should raise an error when it cannot read from the handle" do
      TCPSocket.should_receive(:new).and_return(@handle)
      @socket.open
      @handle.should_receive(:recv).with(17).and_raise(StandardError)
      lambda { @socket.read(17) }.should raise_error(TransportException) { |e| e.type.should == TransportException::NOT_OPEN }
    end

    it "should raise an error when it reads no data from the handle" do
      TCPSocket.should_receive(:new).and_return(@handle)
      @socket.open
      @handle.should_receive(:recv).with(17).and_return("")
      lambda { @socket.read(17) }.should raise_error(TransportException, "Socket: Could not read 17 bytes from localhost:9090")
    end

    it "should return the data read when reading from the handle works" do
      TCPSocket.should_receive(:new).and_return(@handle)
      @socket.open
      @handle.should_receive(:recv).with(17).and_return("test data")
      @socket.read(17).should == "test data"
    end

    it "should declare itself as closed when it has an error" do
      TCPSocket.should_receive(:new).and_return(@handle)
      @socket.open
      @handle.should_receive(:write).with("fail").and_raise(StandardError)
      @socket.should be_open
      lambda { @socket.write("fail") }.should raise_error
      @socket.should_not be_open
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

    it "should create a Thrift::Socket to wrap accepted sockets" do
      handle = mock("TCPServer")
      TCPServer.should_receive(:new).with(nil, 1234).and_return(handle)
      @socket.listen
      sock = mock("sock")
      handle.should_receive(:accept).and_return(sock)
      trans = mock("Socket")
      Socket.should_receive(:new).and_return(trans)
      trans.should_receive(:set_handle).with(sock)
      @socket.accept.should == trans
    end

    it "should close the handle when closed" do
      handle = mock("TCPServer")
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
