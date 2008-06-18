require File.dirname(__FILE__) + '/spec_helper'

shared_examples_for "a socket" do
  it "should open a socket" do
    @socket.open.should == @handle
  end

  it "should be open whenever it has a handle" do
    @socket.should_not be_open
    @socket.open
    @socket.should be_open
    @socket.handle = nil
    @socket.should_not be_open
    @socket.handle = @handle
    @socket.close
    @socket.should_not be_open
  end

  it "should write data to the handle" do
    @socket.open
    @handle.should_receive(:write).with("foobar")
    @socket.write("foobar")
    @handle.should_receive(:write).with("fail").and_raise(StandardError)
    lambda { @socket.write("fail") }.should raise_error(Thrift::TransportException) { |e| e.type.should == Thrift::TransportException::NOT_OPEN }
  end

  it "should raise an error when it cannot read from the handle" do
    @socket.open
    @handle.should_receive(:read).with(17).and_raise(StandardError)
    lambda { @socket.read(17) }.should raise_error(Thrift::TransportException) { |e| e.type.should == Thrift::TransportException::NOT_OPEN }
  end

  it "should raise an error when it reads no data from the handle" do
    @socket.open
    @handle.should_receive(:read).with(17).and_return("")
    lambda { @socket.read(17) }.should raise_error(Thrift::TransportException, "Socket: Could not read 17 bytes from #{@socket.instance_variable_get("@desc")}")
  end

  it "should return the data read when reading from the handle works" do
    @socket.open
    @handle.should_receive(:read).with(17).and_return("test data")
    @socket.read(17).should == "test data"
  end

  it "should declare itself as closed when it has an error" do
    @socket.open
    @handle.should_receive(:write).with("fail").and_raise(StandardError)
    @socket.should be_open
    lambda { @socket.write("fail") }.should raise_error
    @socket.should_not be_open
  end
end
