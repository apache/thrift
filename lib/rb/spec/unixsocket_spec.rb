require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/transport/unixsocket'
require File.dirname(__FILE__) + "/socket_spec_shared"

class ThriftUNIXSocketSpec < Spec::ExampleGroup
  include Thrift

  describe UNIXSocket do
    before(:each) do
      @path = '/tmp/thrift_spec_socket'
      @socket = UNIXSocket.new(@path)
      @handle = mock("Handle", :closed? => false)
      @handle.stub!(:close)
      ::UNIXSocket.stub!(:new).and_return(@handle)
    end

    it_should_behave_like "a socket"

    it "should raise a TransportException when it cannot open a socket" do
      ::UNIXSocket.should_receive(:new).and_raise(StandardError)
      lambda { @socket.open }.should raise_error(Thrift::TransportException) { |e| e.type.should == Thrift::TransportException::NOT_OPEN }
    end
  end

  describe UNIXServerSocket do
    before(:each) do
      @path = '/tmp/thrift_spec_socket'
      @socket = UNIXServerSocket.new(@path)
    end

    it "should create a handle when calling listen" do
      UNIXServer.should_receive(:new).with(@path)
      @socket.listen
    end

    it "should create a Thrift::UNIXSocket to wrap accepted sockets" do
      handle = mock("UNIXServer")
      UNIXServer.should_receive(:new).with(@path).and_return(handle)
      @socket.listen
      sock = mock("sock")
      handle.should_receive(:accept).and_return(sock)
      trans = mock("UNIXSocket")
      UNIXSocket.should_receive(:new).and_return(trans)
      trans.should_receive(:handle=).with(sock)
      @socket.accept.should == trans
    end

    it "should close the handle when closed" do
      handle = mock("UNIXServer", :closed? => false)
      UNIXServer.should_receive(:new).with(@path).and_return(handle)
      @socket.listen
      handle.should_receive(:close)
      @socket.close
    end

    it "should return nil when accepting if there is no handle" do
      @socket.accept.should be_nil
    end
  end
end
